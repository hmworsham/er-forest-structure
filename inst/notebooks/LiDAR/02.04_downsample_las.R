# Downsample LAS
# Author: Marshall Worsham | worsham@berkeley.edu
# Created: 02-28-22
# Revised: 07-22-24

#############################
# Set up working environment
#############################

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

# Define directories
shapedir <- file.path(config$extdata$scratch, 'EastRiver', 'RMBL_2020_EastRiver_SDP_Boundary')
datadir <- file.path(config$extdata$scratch, 'las_regridded')
outdir <- file.path(config$extdata$scratch, 'las_downsampled')
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
