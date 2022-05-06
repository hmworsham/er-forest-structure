# Install and load libraries
library(moose)
pkgs <- c('dplyr',
          'ggplot2',
          'rgdal',
          'raster', 
          'spatialEco',
          'dynatopmodel', 
          'SpaDES', 
          'parallel')

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

##################################
# Topo factor computations
##################################

# # Ingest DEM raster
dem <- rast(file.path(rasdir, 'DEM', 'USGS_13_n39-40_w107-108_mosaic_wgs84utm13n.tif'))
dem1000 <- aggregate(dem, fact=100)
dem100 <- aggregate(dem, fact=10)
dem30 <- aggregate(dem, fact=3)

# slope
slope <- terrain(dem100, v = 'slope', unit = 'degrees')
writeRaster(slope, file.path(rasdir, 'Slope', 'usgs_slope_100m.tif'))
slope <- NULL

# aspect
aspect100 <- terrain(dem100, v = 'aspect', unit = 'degrees')
names(aspect100) <- 'usgs_aspect_100m'
writeRaster(aspect100, file.path(rasdir, 'Aspect', 'usgs_aspect_100m.tif'), overwrite=T)
aspect100 <- NULL

# cos(aspect)
cosaspect <- cos(aspect100)
names(cosaspect) <- 'usgs_cosaspect_100m'
writeRaster(cosaspect, file.path(rasdir, 'Aspect', 'usgs_cosaspect_100m.tif'), overwrite=T)

# sin(aspect)
sinaspect <- sin(aspect100)
names(sinaspect) <- 'usgs_sinaspect_100m'
writeRaster(sinaspect, file.path(rasdir, 'Aspect', 'usgs_sinaspect_100m.tif'), overwrite=T)

# folded aspect
faspect <- foldaspect(aspect, 205)
names(faspect) <- 'usgs_205faspect_100m'
writeRaster(faspect, file.path(rasdir, 'Aspect', 'usgs_205faspect_100m.tif'))

# Adjusted southness
a.southness <- adjsouthness(slope, aspect, unit='deg')
names(a.southness) <- 'usgs_205adjsouthness_100m'
writeRaster(a.southness, file.path(rasdir, 'Aspect', 'usgs_205adjsouthness_100m.tif'))

# THL
thl <- thl(38, slope, aspect, fold=205)
names(thl) <- 'usgs_heatload_100m'
writeRaster(thl, file.path(rasdir, 'Heat_Load', 'usgs_heatload_100m.tif'))

# curvature
curvature <- spatialEco::curvature(dem, type = c('total', 'bolstad'))
writeRaster(curvature, file.path(rasdir, 'usgs_curvature_10m.tif'))
curvature <- NULL

# tpi
tpi_1000 <- spatialEco::tpi(dem100, scale = 9, win = 'rectangle', normalize = T)
tpi_1000 <- disaggregate(tpi_1000, fact=10)
writeRaster(tpi_1000, file.path(rasdir, 'TPI', 'usgs_tpi_1km.tif'), overwrite = T)
tpi_1000 <- NULL

tpi_2000 <- spatialEco::tpi(dem100, scale = 19, win = 'rectangle', normalize = T)
tpi_2000 <- disaggregate(tpi_2000, fact=10)
writeRaster(tpi_2000, file.path(rasdir, 'TPI', 'usgs_tpi_2km.tif'))
tpi_2000 <- NULL

# twi
topmod <- dynatopmodel::build_layers(dem100, fill.sinks = T)
twi <- topmod[[3]]
twi <- disaggregate(twi, fact = 10)
writeRaster(twi, file.path(rasdir, 'TWI', 'usgs_twi_100m.tif'), overwrite = T)
twi <- NULL

# upslope contributing area
uca <- topmod[[2]]
uca <- disaggregate(uca, fact = 10)
writeRaster(uca, file.path(rasdir, 'Upslope_Contributing_Area', 'usgs_uca_100m.tif'))
uca <- NULL