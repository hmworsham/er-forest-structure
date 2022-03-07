# Individual tree crown segmentation on LiDAR points
# Author: Marshall Worsham | worsham@berkeley.edu.
# Created: 04-08-21
# Revised: 03-02-22

#########################
# Front matter
########################

# Install non-CRAN libraries
#devtools::install_github("jrminter/rPeaks") # rPeaks for deconv/decomp
#devtools::install_github('tankwin08/waveformlidar') # waveformlidar for general processing
#devtools::install_github('lwasser/neon-aop-package/neonAOP') #neonAOP for reading binary data

# Install and load typical libraries
pkgs <- c('dplyr',
          'tidyverse',
          'ggplot2',
          'raster',
          'devtools',
          'plotly',
          'rPeaks',
          'rgdal',
          'caTools',
          'sf', 
          'parallel',
          'itcSegment',
          'lidR',
          'rlas',
          'broom',
          'plot3D') # Name the packages you want to use here

# Function to install new packages if they're not already installed
load.pkgs <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
# Runs the function on the list of packages defined in pkgs
load.pkgs(pkgs)

# Name directories
datadir <- '/global/scratch/users/worsham/geolocated_returns_plots'
shapedir <- '/global/scratch/users/worsham/EastRiver/Plot_Shapefiles/Polygons/'
outdir <- '/global/scratch/users/worsham/trees'
fidir <- '/global/scratch/users/worsham/EastRiver/Inventory_Plots'
gpsdir <- '/global/scratch/users/worsham/EastRiver/StemGeolocations'

#########################################
# Ingest points from waveform processing
########################################

ptcsv <- list.files(datadir, full.names = T)
xx <- read.csv(ptcsv[1], header=T)

aois <- list.files(datadir, full.names = F)
aois <- unique(sapply(strsplit(aois, '_2018'), '[', 1))
aois

##########################################
# ITC segmentation
##########################################
aoi = 'ER-GT1'

testlas <- pts2las(aoi)
sgtrees <- itd(testlas, aoi)
xx <- predictloss(sgtrees, aoi)

itcfun <- function(aoi){
  olas = pts2las(aoi)
  oft = itd(olas, aoi)
  return(oft)
}

itcfun('CC-CVN2')



