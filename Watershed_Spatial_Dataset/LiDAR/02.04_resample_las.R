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
outdir <- '/global/scratch/users/worsham/las_resampled2'
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

  newlas = basename(las)
  newlas = file.path(outdir, newlas)
  write.las(newlas, xx@header, xx@data)
}

mclapply(list.files(datadir, full.names=T)[190:1627], resample_las, outdir, mc.cores=getOption('mc.cores', 24L))
