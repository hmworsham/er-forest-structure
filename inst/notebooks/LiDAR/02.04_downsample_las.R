library(terra)
library(parallel)
library(tidyverse)
library(plyr)
library(RColorBrewer)
library(pbmcapply)
library(lidR)
library(rlas)

shapedir <- '/global/scratch/users/worsham/EastRiver/RMBL_2020_EastRiver_SDP_Boundary'
datadir <- '/global/scratch/users/worsham/las_regridded'
outdir <- '/global/scratch/users/worsham/las_downsampled'
dir.create(outdir)

########################
# Downsample las catalog
########################

resample_las <- function(las, outdir){
  xx = readLAS(las)
  pk = xx@data[!is.na(xx@data$t)]
  npk = xx@data[is.na(xx@data$t)]
  npk = npk[sample(1:nrow(npk), nrow(npk)/4)]
  xx@data = rbind(pk, npk)
  xx@header = header_update(xx@header, xx@data)

  newlas = str_replace(basename(las), 'regridded', 'downsampled')
  newlas = file.path(outdir, newlas)
  write.las(newlas, xx@header, xx@data)
}

# Identify which files have been resampled
did <- list.files(outdir)
inputs <- list.files(datadir)
notdid <- file.path(datadir, inputs[!inputs %in% did])

# Resample files not completed
mclapply(notdid, resample_las, outdir, mc.cores=getOption('mc.cores', 24L))

# Plot to check
lascat <- readLAScatalog(file.path(outdir, did))
plot(lascat['Number.of.point.records'])
plot(lascat[lascat['Number.of.point.records']$Number.of.point.records>0])
