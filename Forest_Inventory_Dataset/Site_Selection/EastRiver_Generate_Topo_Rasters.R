# Install and load libraries
pkgs <- c('dplyr',
          #'tidyverse',
          'ggplot2',
          'rgdal',
          'raster', 
          'spatialEco',
          'dynatopmodel')

# Name the packages you want to use here
load.pkgs <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} # Function to install new packages if they're not already installed
load.pkgs(pkgs) # Runs the function on the list of packages defined in pkgs

# Set working directory.
# setwd(file.path('~', 'Desktop', 'RMBL', 'Projects', fsep = '/'))
# fidir <- file.path(getwd(), 'Forest_Inventory_Dataset', 'Output', fsep = '/')
# wsdir <- file.path(getwd(), 'Watershed_Spatial_Dataset', 'Source', fsep = '/')
# rasdir <- file.path('..', '..', '..', 'Google Drive (worsham@berkeley.edu)', 'Research', 'RMBL', 'RMBL_East River Watershed Forest Data', 'Data', 'Geospatial', 'Worsham_2021_SiteSelection', '2021_Analysis_Layers', 'USGS_1-9_arcsec_DEM', fsep = '/')
rasdir <-  file.path('~', 'dem')

##################################
# Topo factor computations
##################################

# Ingest raster
dem <- raster(file.path(rasdir, 'USGS_13_n39-40_w107-108_mosaic_wgs84utm13n.tif'))
plot(dem)

# slope
slope <- terrain(dem, opt = 'slope', unit = 'degrees')
writeRaster(slope, file.path(rasdir, 'usgs_slope_10m.tif'))
slope <- NULL

# aspect
aspect <- terrain(dem, opt = 'aspect', unit = 'degrees')
writeRaster(aspect, file.path(rasdir, 'usgs_aspect_10m.tif'))
aspect <- NULL

# curvature
curvature <- spatialEco::curvature(dem, type = c('total', 'bolstad'))
writeRaster(curvature, file.path(rasdir, 'usgs_curvature_10m.tif'))
curvature <- NULL

# tpi
tpi_1000 <- spatialEco::tpi(dem, scale = 99, win = 'rectangle', normalize = T)
writeRaster(tpi_1000, file.path(rasdir, 'usgs_tpi_1km.tif'))
tpi_1000 <- NULL

tpi_2000 <- spatialEco::tpi(dem, scale = 199, win = 'rectangle', normalize = T)
writeRaster(tpi_2000, file.path(rasdir, 'usgs_tpi_2km.tif'))
tpi_2000 <- NULL

# twi
twi <- dynatopmodel::build_layers(dem, fill.sinks = T)
writeRaster(twi, file.path(rasdir, 'usgs_twi_10m.tif'))
twi <- NULL


# Compare USGS 1/9 arc-second DEM-derived values to original AOP DEM-derived values
slope.aop = raster::extract(slope, spdf, buffer = 20, fun = mean)
slope.usgs = raster::extract(toporasters[[6]], spdf, buffer = 20, fun = mean)

aspect.aop = raster::extract(aspect, spdf, buffer = 20, fun = mean)
aspect.usgs = raster::extract(toporasters[[1]], spdf, buffer = 20, fun = mean)

slope.usgs
aspect.usgs