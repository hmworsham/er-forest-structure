# EastRiver_Generate_Topo_Rasters
# Generates a series of rasters from an input digital elevation model, each depicting a topographic parameter, such as slope, aspect, curvature, topographic position, etc.

# Author: Marshall Worsham
# Created: 10-06-20
# Revised: 06-29-21

# Install and load libraries
pkgs <- c('dplyr',
          'tidyverse',
          'ggplot2',
          'data.table', 
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
setwd(file.path('~', 'Desktop', 'RMBL', 'Projects', fsep = '/'))
fidir <- file.path(getwd(), 'Forest_Inventory_Dataset', 'Output', fsep = '/')
wsdir <- file.path(getwd(), 'Watershed_Spatial_Dataset', 'Source', fsep = '/')
rasdir <- file.path('..', '..', '..', 'Google Drive (worsham@berkeley.edu)', 'Research', 'RMBL', 'RMBL_East River Watershed Forest Data', 'Data', 'Geospatial', 'Worsham_2021_SiteSelection', '2021_Analysis_Layers', 'USGS_1-9_arcsec_DEM', fsep = '/')

##################################
# Mosaic raster for full coverage
##################################

# Ingest USGS 1/9 arc-second DEM rasters to mosaic
dem1 <- raster(file.path(rasdir, 'USGS_1-9_arcsec_DEMs', 'USGS_13_n39w107_20210312.tif'))
dem2 <- raster(file.path(rasdir, 'USGS_1-9_arcsec_DEMs', 'USGS_13_n39w108_20210312.tif'))
dem3 <- raster(file.path(rasdir, 'USGS_1-9_arcsec_DEMs', 'USGS_13_n40w107_20210312.tif'))
dem4 <- raster(file.path(rasdir, 'USGS_1-9_arcsec_DEMs', 'USGS_13_n40w108_20210312.tif'))

demdir <- file.path(rasdir, 'USGS_1-9_arcsec_DEMs')
dems <- lapply(list.files(demdir, pattern = 'tif', full.names = T), raster)

# Concatenate rasters to list and define a mosaic function
#x <- list(dem1, dem2, dem3, dem4)
names(dems)[1:2] <- c('x', 'y')
dems$fun <- mean
dems$na.rm <- T

# Run the mosaic function
mergedem <- do.call(mosaic, x)

# Plot the merged raster
plot(mergedem)

# Write the raster to disk
writeRaster(mergedem, file.path(rasdir, 'DEM', 'USGS_13_n39-40_w107-108_20210312_mosaic.tif'))

##################################
# Topo factor computations
##################################

# Ingest raster
dem <- raster(file.path(rasdir, 'DEM', 'USGS_13_n39-40_w107-108_mosaic_wgs84utm13n.tif'))

# Compute topographic factors
slope <- terrain(dem, opt = 'slope', unit = 'degrees')
aspect <- terrain(dem, opt = 'aspect', unit = 'degrees')
curvature <- spatialEco::curvature(dem, type = c('total'))
tpi_1000 <- spatialEco::tpi(dem, scale = 99, win = 'rectangle', normalize = T)
tpi_2000 <- spatialEco::tpi(dem, scale = 199, win = 'rectangle', normalize = T)
twi <- dynatopmodel::build_layers(dem, fill.sinks = T)

# Write topographic factors as rasters
# TK

# Compare USGS 1/9 arc-second DEM-derived values to original AOP DEM-derived values
slope.aop = raster::extract(slope, spdf, buffer = 20, fun = mean)
slope.usgs = raster::extract(toporasters[[6]], spdf, buffer = 20, fun = mean)

aspect.aop = raster::extract(aspect, spdf, buffer = 20, fun = mean)
aspect.usgs = raster::extract(toporasters[[1]], spdf, buffer = 20, fun = mean)

slope.usgs
aspect.usgs