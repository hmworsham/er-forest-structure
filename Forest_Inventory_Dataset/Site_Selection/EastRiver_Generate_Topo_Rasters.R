# Install and load libraries
pkgs <- c('dplyr',
          'ggplot2',
          'rgdal',
          'raster', 
          'spatialEco',
          'dynatopmodel', 
          'SpaDES', 
          'parallel')

# Name the packages you want to use here
load.pkgs <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} # Function to install new packages if they're not already installed
load.pkgs(pkgs) # Runs the function on the list of packages defined in pkgs

# Set working directory.
setwd(file.path('~', 'Desktop', 'RMBL', 'Projects', fsep = '/'))
fidir <- file.path(getwd(), 'Forest_Inventory_Dataset', 'Output', fsep = '/')
wsdir <- file.path(getwd(), 'Watershed_Spatial_Dataset', 'Source', fsep = '/')
rasdir <- file.path('..', '..', '..', 'Google Drive (worsham@berkeley.edu)', 'Research', 'RMBL', 'RMBL_East River Watershed Forest Data', 'Data', 'Geospatial', 'Worsham_2021_SiteSelection', '2021_Analysis_Layers', 'USGS_1-9_arcsec_DEM', fsep = '/')

##################################
# Topo factor computations
##################################

# # Ingest DEM raster
dem <- raster(file.path(rasdir, 'DEM', 'USGS_13_n39-40_w107-108_mosaic_wgs84utm13n.tif'))
dem100 <- aggregate(dem, fact=10)
dem30 <- aggregate(dem, fact=3)

# slope
?terrain
slope <- terrain(dem30, opt = 'slope', unit = 'degrees')
writeRaster(slope, file.path(rasdir, 'Slope', 'usgs_slope_10m.tif'))
slope <- NULL

# aspect
aspect <- terrain(dem, opt = 'aspect', unit = 'degrees')
writeRaster(aspect, file.path(rasdir, 'Aspect', 'usgs_aspect_10m.tif'))
aspect <- NULL

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