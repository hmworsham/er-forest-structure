# EastRiver_Derive_Topos
# Calculates the values of various topographic factors for rectangular 40x40 m neighborhoods around specified coordinates and/or within the extents of forest inventory plots in the East River watershed.

# Author: Marshall Worsham
# Created: 10-06-20
# Revised: 07-11-21

#############################
# Set up workspace
#############################
# Function to install new packages if they're not already installed
load.pkgs <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# List packages
pkgs <- c('data.table', 
          'dplyr',
          'ggplot2',
          'googledrive',
          'readxl',
          'sf',
          'terra',
          'tidyverse')

# Run load on the list of packages named in pkgs
load.pkgs(pkgs)

# Source helper functions
source(file.path(dirname(rstudioapi::getSourceEditorContext()$path), 'ss.helpers.R'))

# Set working directory.
erdir <- file.path('/Volumes', 'GoogleDrive', 'My Drive', 'Research', 'RMBL', fsep='/')
fidir <- file.path(erdir, 'Working_Files', 'Forest_Inventory_Dataset', 'Output', fsep = '/')
wsdir <- file.path(erdir, 'Working_Files', 'Watershed_Spatial_Dataset', 'Output', fsep = '/')
rasdir <- file.path(erdir, 'RMBL-East River Watershed Forest Data', 'Data', 'Geospatial', 'Worsham_2021_SiteSelection', '2021_Analysis_Layers', 'USGS_1-9_arcsec_DEM')
sfdir <- file.path(erdir, 'RMBL-East River Watershed Forest Data', 'Data', 'Geospatial')

#############################
# Ingest data
#############################
topo.factors <- c('Aspect', 
                  'Curvature',
                  'DTM',
                  'DEM',
                  'Heat_Load',
                  'Slope', 
                  'TPI', 
                  'TWI')

# Ingest 2020 Kueppers plot characteristics CSVs
tmpfile <- tempfile()
tmpfile <- drive_download('Kueppers_EastRiver_Site_Index', tmpfile)$local_path
siteinfo.ext <- read_excel(tmpfile)
coords.ext <- siteinfo.ext[,c('Location_ID', 'Latitude', 'Longitude')]

# Ingest 2022 proposed site info
siteinfo.22 <- read.csv(file.path(fidir, 'EastRiver_Proposed_Sites_2022_10.csv'))

# Make df of location names and coordinates
coords.22 <- siteinfo.22[,c('Location_Name', 'Latitude', 'Longitude')]
names(coords.22)[1] <- 'Location_ID'

# Ingest boundaries of existing plots
plots.ext <- list.files(file.path(sfdir, 'Kueppers_EastRiver_Plot_Shapefiles_WGS84UTM13N', 'AllPlots'), pattern = 'shp', full.names = T)

#############################
# Compute topographic stats
#############################

# Compute topo statistics for new plots
zonnew <- zonals(coords.22, rasdir, topo.factors, type = 'coord', radius = 20)

# Write out topo stats to csv
write.csv(zonnew, '~/Desktop/EastRiver_Proposed_Sites_22_10.csv')

########################################
# Prepare coordinates to submit for review
########################################

# Bind existing coordinates df to new coordinates df
allcoords <- rbind(coords.ext, coords.22)

# Create sets of 'good' and 'bad' sites for inclusion/exclusion
# goodsites <- c() # Use if good sites list is short
badsites <- c('Ute Gulch 2',
              'Baldy Mountain east 4', 
              'Snodgrass NW slope 2', 
              'Emerald 1',
              'Cement Creek 28')
goodsites <- coords.22[!coords.22$Location_ID %in% badsites, 'Location_ID']

# Filter all coordinates to those in goodsites
submitcoords <- allcoords[allcoords$Location_ID %in% goodsites,]

# Rename rows to integer sequence
rownames(submitcoords) <- seq(1, nrow(submitcoords))

# Write coordinates to CSV
write.csv(
  submitcoords, 
  file.path(
    fidir, 
    'EastRiver_Proposed_Coordinates_2022_10.csv')
  )

#######################################################
# Create hypothetical plot polygons and write to shp
#######################################################

# Make polygons from submission coordinates
submitpolys <- makepolys(submitcoords, radius=20, shape='rectangle')
submitpolys.sf <- st_as_sf(submitpolys)
submitpolys.sf$Location_ID <- goodsites

# Write polygons to shp
st_write(
  submitpolys, 
  file.path(sfdir, 
            'Worsham_2021_SiteSelection', 
            '2022_Proposed_Sites', 
            'Kueppers_EastRiver_Proposed_Sites_2022_10'), 
  driver="ESRI Shapefile", append=F)

# Check that shp is readable and plot
plot(
  rast(
    list.files(
      rasdir, 
      recursive=T, 
      full.names=T)[12]), 
  col=gray.colors(12))

plot(st_read(
  file.path(
    sfdir, 
    'Worsham_2021_SiteSelection', 
    '2022_Proposed_Sites', 
    'Kueppers_EastRiver_Proposed_Sites_2022_10')),
  lwd=6, 
  border=magma(10), 
  add=T)

