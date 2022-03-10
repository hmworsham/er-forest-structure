# R Script to find 2018 NEON LiDAR flightpaths that intersect forest

# Install and load libraries
pkgs <- c(
  'caTools',
  'dplyr',
  'ggplot2',
  'parallel',
  'pbmcapply',
  'raster',
  'rgdal',
  'rgeos',
  'sf',
  'tidyverse') # Name the packages you want to use here

# Function to install new packages if they're not already installed
load.pkgs <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
# Runs the function on the list of packages defined in pkgs
load.pkgs(pkgs)

##############################################
# Find flightpaths that intersect with forest
##############################################

# Name data directory
datadir <- '/global/scratch/users/worsham/waveform_binary_chunks'
tifdir <- '/global/scratch/users/worsham'

# Ingest flightpath chunk extents
extents <- read.csv(file.path('~','Output', 'flightpath_chunk_extents.csv'), header=F)
names(extents) <- c('xmin', 'xmax', 'ymin', 'ymax')
extents$index <- seq(nrow(extents))
dim(extents)
head(extents)

# Ingest forest/nonforest binary tif
forest <- raster(file.path(tifdir, 'aop_forest.tif'))
plot(forest)

# Define function to turn flightpath chunk extent coordinates into sp boxes
makeboxes <- function(x) {
  ext = bbox2SP(x[4], x[3], x[2], x[1],
                proj4string = CRS('+proj=utm +zone=13 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'))
  ext$id = x[5]
  return(ext)
}

# Make boxes for all chunk extents
boxes <- apply(extents, 1, makeboxes)

# Define function to get n forested pixels and flightpath size in pixels
extractfun <- function(shf, ras) {
  sz = nrow(raster::extract(ras, shf, na.rm=T, df=T))
  val = raster::extract(ras, shf, fun=sum, na.rm=T)
  id = shf$id
  return(list(val, sz, id))
}

# Get n forested pixels per flightpath
# Uses `pbmclapply` to parallelize and track progress
overlaps <- pbmclapply(boxes, extractfun, forest, mc.cores=22L)

# Calculate percent forest cover per flightpath
pctforest <- list()
indexes <- list()
for (i in seq(1,length(overlaps))){
  pair = unlist(overlaps[i])
  pct = pair[1]/pair[2]
  idx = pair[3]
  pctforest[i] = pct
  indexes[i] = idx
  }

pctf <- data.frame('index'=unlist(indexes), 'pctforest'=unlist(pctforest))

# Define non-forested chunks as those with < 8% forest
nonfor = which(unlist(pctforest)<0.08)
length(nonfor)

# Add %forest to extents table
#extents$pct_forest <- unlist(pctforest)
extents = merge(extents, pctf, by='index')

# Write results to csv
extents <- data.frame(extents)
View(extents)
write.csv(extents, '~/Output/flightpath_forest_intersections.csv')

read.csv('~/Output/flightpath_forest_intersections.csv')
