# Load libraries
library(parallel)
library(tidyverse)
library(devtools)
library(lidR)
library(rlas)
load_all('~/Repos/rwaveform')

# Setup workspace
scrdir <- '/global/scratch/users/worsham'
shapedir <- '/global/scratch/users/worsham/EastRiver/RMBL_2020_EastRiver_SDP_Boundary'
datadir <- '/global/scratch/users/worsham/hyperpointcloud'
outdir <- '/global/scratch/users/worsham/las_ungridded'

# Ingest geolocated points
infiles <- list.files(datadir, full.names=T)
did = str_replace(list.files(outdir), '_hpc.las', '_hpc.csv')
did = file.path(datadir, did)
infiles <- infiles[!infiles %in% did]
print(length(infiles))

# Convert points to las
mclapply(infiles, rwaveform::pts2las, outpath=outdir, mc.cores=getOption('mc.cores', 24L))
