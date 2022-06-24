# Load libraries
options(rgl.useNULL=TRUE)
library(terra)
library(parallel)
library(tidyverse)
library(plyr)
library(RColorBrewer)
library(pbmcapply)
library(devtools)
library(lidR)
library(rlas)
library(rgdal)
library(rgl)
load_all('~/Repos/rwaveform')

# Setup workspace
scrdir <- '/global/scratch/users/worsham'
shapedir <- '/global/scratch/users/worsham/EastRiver/RMBL_2020_EastRiver_SDP_Boundary'
datadir <- '/global/scratch/users/worsham/las'
griddir <- '/global/scratch/users/worsham/gridded_returns'
neondir <- file.path(scrdir, 'neon_las')
outdir <- '/global/scratch/users/worsham/las_decimated'

############################
# Prep data and visualize
############################

# Ingest gridded points
infiles <- list.files(datadir, full.names=T)

# # Make raster grid and convert to vector grid
aop <- vect(file.path(shapedir, 'RMBL_2020_EastRiver_SDP_Boundary', 'SDP_Boundary.shp'))
aop.ras <- rast(ext(aop)+1000, nrows=30, ncols=30, crs=crs(aop))
values(aop.ras) <- seq(1:900)
aop.grid <- as.polygons(aop.ras)
aop.ras

# Ingest gridded point counts
returncounts <- read.table(file.path(scrdir, 'gridded_returns_wc.txt'))

# Add gridcell vector to dataframe
returncounts['gridcell'] <- as.numeric(unlist(lapply(str_split(returncounts$V2, './|_'), '[', 2)))

# Set any point count=1 to NA
returncounts[returncounts['V1']==1,]$V1 <- NA
returncounts <- returncounts[order(returncounts$gridcell),]

# Assign point counts as raster cell values
values(aop.ras) <- returncounts$V1

# Plot heatmap of returns
plot(aop.ras, col=heat.colors(100))
lines(aop, col='grey10', lwd=3)

#############
# Downsample
#############

# Ingest LAS as LAS catalog
lascat <- readLAScatalog(infiles)
las_check(lascat)

# Specify out directory for large las files
opt_output_files(lascat) <- file.path(outdir, '{ORIGINALFILENAME}_decimated')

# Downsample (decimate) points with homogenization function
dp <- decimate_points(lascat, homogenize(8, 1))

#################
# Check results
################
lascat1 <- readLAScatalog(infiles)
lascat2 <- readLAScatalog(list.files(outdir, full.names=T))