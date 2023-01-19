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
rasdir <- file.path(erdir, 'RMBL-East River Watershed Forest Data', 'Data', 'Geospatial', 'Worsham_SiteSelection', '2021_Analysis_Layers', 'USGS_1-9_arcsec_DEM')
sfdir <- file.path(erdir, 'RMBL-East River Watershed Forest Data', 'Data', 'Geospatial')
potrdir <- file.path(erdir, 'RMBL-East River Watershed Forest Data', 'Data', 'Geospatial', 'Blonder_Aspen_Plots_2020')
berkdir <- file.path(erdir, 'RMBL-East River Watershed Forest Data', 'Data', 'Geospatial', 'Berkelhammer_Still_Sapflux_Sites_2021')

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

# Ingest cored aspen coordinates
aspen <- read.csv(file.path(potrdir, 'aspen data site-level processed 30 Mar 2020.csv'))
View(aspen)
aspen.coords <- data.frame(Site_Code=aspen$Site_Code, Longitude=aspen$Longitude, Latitude=aspen$Latitude)
aspen.coords <- na.omit(aspen.coords)

# Ingest Berkelhammer coordinates
berk <- read.csv(file.path(berkdir, 'SapSites.csv'))
berk.coords <- berk %>%
  group_by(site..) %>%
  filter(row_number()==1)
berk.coords <- data.frame(Location_ID=berk.coords$site.., Longitude=berk.coords$longitude, Latitude=berk.coords$latitude)

#############################
# Compute topographic stats
#############################

# Compute topo statistics for Kueppers plots
zon.kplots <- zonals(coords.ext, rasdir, topo.factors, type='coord', radius=20)

# Write out topo stats to csv
write.csv(zon.kplots, '~/Desktop/kueppers_plots_zonals.csv')

# Compute topo statistics for aspen plots
zon.asp <- zonals(aspen.coords, rasdir, topo.factors, type = 'coord', radius = 10)
aspen.coords[aspsites,]
aspsites <- which(aspen.coords$Site_Code %in% c('ACGNT01',
                                                'AGXLZ',
                                                'DBUZY',
                                                'FOASS01',
                                                'GRJXK',
                                                'NGTWF',
                                                'OFCNI',
                                                'POZDQ',
                                                'WCPJH',
                                                'WPRLZ',
                                                'XKPJS',
                                                'ZJQRW'))

aspsites
zon.asp <- zon.asp[aspsites,]

# Write out topo stats to csv
write.csv(zon.asp, '~/Desktop/aspen_zonals.csv')

# Compute zonals for Berkelhammer sapflux sites
zon.berk <- zonals(berk.coords, rasdir, topo.factors, type='coord', radius=20)

# Write out topo stats to csv
write.csv(zon.berk, '~/Desktop/berkelhammer_zonals.csv')

# Potential new sap flux sites
p1 <- c(-106.9744584,38.9271352)
p2 <- c(-106.983025,38.924723)
p3 <- c(-107.0071540,38.9736823)
lons <- c(p1,p2,p3)[c(TRUE, FALSE)]
lats <- c(p1,p2,p3)[c(FALSE,TRUE)]
newsf.coords <- data.frame('ID'=seq(length(lons)),
                            'Longitude'=lons,
                            'Latitude'=lats)


zon.newsf <- zonals(newsf.coords, rasdir, topo.factors, type='coord', radius=5)
write.csv(zon.newsf, '~/Desktop/newsf_zonals.csv')

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

