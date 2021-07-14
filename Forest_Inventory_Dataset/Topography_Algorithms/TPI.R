
# Set up workspace
## Install and load libraries
pkgs <- c('dplyr',
          'tidyverse',
          'ggplot2',
          'raster',
          'rgdal',
          'sf',
          'ggspatial',
          'rasterVis',
          'XML',
          'RCurl',
          'googledrive',
          'ForestTools',
          'spatialEco') # Name the packages you want to use here
load.pkgs <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} # Function to install new packages if they're not already installed
load.pkgs(pkgs) # Runs the function on the list of packages defined in pkgs

# Set working directory.
setwd('~/Desktop/RMBL/Projects/Watershed_Spatial_Dataset/Output/SDP_DEM_Resampled')
outdir <- paste0('~/Desktop/RMBL/Projects/Watershed_Spatial_Dataset/Output/TPI')

# Ingest edge-filled DEM
dem <- paste0(getwd(), '/sdp_dem10.tif')
dem <- raster(dem)

tpi1k <- tpi(dem, win = 'rectangle', scale = 999, zero.correct = T)

opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
plot(dem, main="original raster")
plot(tpi1k, main="tpi 999x999")


