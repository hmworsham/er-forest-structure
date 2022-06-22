# EastRiver_Derive_Topos
# Calculates the values of various topographic factors for rectangular 40x40 m neighborhoods around specified coordinates and/or within the extents of forest inventory plots in the East River watershed.

# Author: Marshall Worsham
# Created: 10-06-20
# Revised: 07-11-21

#############################
# Set up workspace
#############################

## Install and load libraries
pkgs <- c('data.table', 
          'dplyr',
          'ggplot2',
          'googledrive',
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
sfdir <- file.path(erdir, 'RMBL-East River Watershed Forest Data', 'Data')

#############################
# Ingest raster data
#############################
topo.factors <- c('Aspect', 
                  'Curvature',
                  'DTM',
                  'DEM',
                  'Heat_Load',
                  'Slope', 
                  'TPI', 
                  'TWI')

#############################
# Ingest source data
#############################

# Ingest 2020 Kueppers plot characteristics CSVs
tmpfile <- tempfile()
tmpfile <- drive_download('Kueppers_EastRiver_Site_Index', tmpfile)$local_path
siteinfo21 <- read_excel(tmpfile)

# Ingest 2020-2021 Kueppers plot info
siteinfo22 <- read.csv(file.path(fidir, 'EastRiver_ProposedSites_2021_25.csv'), header = T)

# Refactor a couple of columns in siteinfo20 to row bind with 2021 data
siteinfo20 <- rename(siteinfo20, Site_ID = SFA_ID)
#siteinfo20$Established <- as.factor(siteinfo20$Established)
siteinfo20$Established <- 'Established'

coords <- siteinfo21[,c(1,6:7)]

plots21 <- list.files(file.path(sfdir, 'Geospatial', 'Kueppers_EastRiver_Plot_Shapefiles_WGS84UTM13N', 'AllPlots'), pattern = 'shp', full.names = T)

plots22 <- list.files(file.path(sfdir,
                                'Geospatial',
                                'Worsham_2021_SiteSelection', 
                                '2021_Proposed_Sites_All',
                                'Kueppers_EastRiver_ProposedSites_2021_25'),
                     pattern = 'shp', full.names = T)

newcoords <- data.frame(
  Site_ID = c('emerald','emerald2', 'cement 28', 'walrod1', 'walrod2', 'axtell1', 'coal north 2', 'wildcat1', 'scarp1', 'scarp2', 'scarp3', 'coal north 3', 'sr-pvg1', 'cc-cvs1'), 
  Longitude = c(-107.047670, -107.047914, -106.8253285, -106.846080, -106.8472989, -107.0121336, -107.025286, -107.0357038, -107.067599, -107.0672603, -107.069762, -107.0204872, -107.0806799, -107.0353710), 
  Latitude = c(39.010148, 39.014249, 38.8274440, 38.828859, 38.8292840, 38.8524498, 38.881999, 38.8608894, 38.876385, 38.8758445, 38.877791, 38.8751397, 38.9544081, 38.8608831))




zon21 <- zonals(plots21, rasdir, topo.factors, type = 'sf')
zon21 <- zon21[order(row.names(zon21)),]
#zon21 <- zon21[order(zon21$usgs_205adjsouthness_100m),]
write.csv(zon21, '~/Desktop/zonals.csv')

zon22 <- zonals(plots22, rasdir, topo.factors, type = 'sf')
zonnew <- zonals(newcoords, rasdir, type = 'coord', radius = 20)
#fullset <- rbind(zon20, zon21, zonnew)

newcoords
coords20 <- siteinfo20[,c(1,5:6)]
coords21 <- siteinfo21[,c(1,5:6)]

allcoords <- rbind(coords20, coords21, newcoords)

goodsites <- c('Coal Creek Valley North 1B',
               'coal north 2',
               'coal north 3',
               'cement 28',
               '25 Granite Basin',
               'emerald',
               'Carbon 20',
               #'axtell1',
               'Point Lookout North 3',
               'wildcat1', 
               #'scarp1', 
               'scarp2' 
               #'scarp3'
               )

submitcoords <- allcoords[allcoords$Site_ID %in% goodsites,]
rownames(submitcoords) <- seq(1, nrow(submitcoords))
submitcoords
write.csv(submitcoords, 'EastRiver_Coordinates_9_Final.csv')
okok <- makepolys(submitcoords, 20, 'rectangle')
okok.sf <- st_as_sf(okok)
st_write(okok.sf, file.path(sfdir, 'Worsham_2021_SiteSelection', '2021_Proposed_Sites_All', 'Kueppers_EastRiver_ProposedSites_2021_9Final'), driver="ESRI Shapefile")