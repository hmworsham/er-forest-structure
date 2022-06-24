# Load libraries
library(terra)
library(parallel)
library(tidyverse)
library(plyr)
library(RColorBrewer)
library(pbmcapply)
library(lidR)
library(future)

shapedir <- '/global/scratch/users/worsham/EastRiver/RMBL_2020_EastRiver_SDP_Boundary'
datadir <- '/global/scratch/users/worsham/las_ungridded'
outdir <- '/global/scratch/users/worsham/las_regridded'
dir.create(outdir)

########################
# Regrid las catalog
########################

lascat <- readLAScatalog(datadir)
opt_chunk_buffer(lascat) <- 0
opt_chunk_size(lascat) <- 900
opt_output_files(lascat) <- file.path(outdir, "las_regridded_{XLEFT}_{YBOTTOM}")

# Preview the chunk pattern
plot(lascat, chunk = TRUE)
lascat
# Retile with multiple cores
plan(multisession, workers = 30L)
set_lidr_threads(30L)
newlascat = catalog_retile(lascat)

# Check regridded las catalog
lascatrg <- readLAScatalog(list.files(outdir, full.names=T, pattern='.las'))
plot(lascatrg['Number.of.point.records'])
