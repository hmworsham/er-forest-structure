# R Script to find 2018 NEON LiDAR flightpaths that intersect Kueppers 2020 forest inventory plots

# Install and load libraries
pkgs <- c('dplyr',
          'tidyverse',
          'ggplot2',
          'rgdal',
          'raster',
          'rgeos',
          'caTools',
          'sf') # Name the packages you want to use here

# Function to install new packages if they're not already installed
load.pkgs <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
# Runs the function on the list of packages defined in pkgs
load.pkgs(pkgs)

###################
# Ingest plots 
###################

# Name directory where shapefiles live
allplotsdir <- '/Volumes/GoogleDrive/My Drive/Research/RMBL/RMBL-East River Watershed Forest Data/Data/Geospatial/Kueppers_EastRiver_Plot_Shapefiles_WGS84UTM13N/AllPlots'
sfdir <- '/Volumes/GoogleDrive/My Drive/Research/RMBL/RMBL-East River Watershed Forest Data/Data/Geospatial/Kueppers_EastRiver_Plot_Shapefiles_WGS84UTM13N/Polygons'
tifdir <- '/Volumes/GoogleDrive/My Drive/Research/RMBL/Working_Files/Watershed_Spatial_Dataset/Output/'

# Read in the shapefile containing all plots
allplots_path = list.files(allplotsdir, 
                           pattern = glob2rx(paste0('*AllPlots*',"*shp")),
                           full.names = T)
allplots <- st_read(allplots_path, quiet=T)

######################################
# Find flightpaths that contain plots
######################################

# Name data directory
datadir <- '/Volumes/GoogleDrive/My Drive/Research/RMBL/RMBL-East River Watershed Forest Data/Data/LiDAR/'
#datadir <- '/global/scratch/users/worsham/waveform_binary_chunks'

extents <- read.csv(paste0(datadir, 'flightpath_chunk_extents.csv'), header=T)
dim(extents)
makeboxes <- function(x) {
  ext = bbox2SP(x[4], x[3], x[2], x[1],
                proj4string = CRS('+proj=utm +zone=13 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'))
  return(ext)
}

boxes <- apply(extents, 1, makeboxes)
testbox <- boxes[[1]]

forest <- raster(paste0(tifdir, 'aop_forest.tif'))
plot(forest)
for (b in nonfor) plot(boxes[[b]], add=T)


extractfun <- function(shf, ras) {
  sz = nrow(extract(ras, shf, na.rm=T, df=T))
  val = extract(ras, shf, fun=sum, na.rm=T)
  return(list(val, sz))
}

overlaps <- lapply(boxes, extractfun, forest)

pctforest <- list()
for (i in seq(1,length(overlaps))){
  pair = unlist(overlaps[i])
  pct = pair[1]/pair[2]
  pctforest[i] = pct
  }

nonfor = which(unlist(pctforest)<0.08)
length(nonfor)

extents$pct_forest <- unlist(pctforest)
extents <- data.frame(extents)
write.csv(extents, '~/Desktop/flightpath_forest_intersections.csv')
