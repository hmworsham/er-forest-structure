# Regrid LAS catalog
# Author: Marshall Worsham

# Install and load libraries
pkgs <- c('future',
          'lidR',
          'raster',
          'rgl',
          'RColorBrewer',
          'sf',
          'terra',
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

# Define directories
shapedir <- '/global/scratch/users/worsham/EastRiver/RMBL_2020_EastRiver_SDP_Boundary'
datadir <- '/global/scratch/users/worsham/las_resampled'
outdir <- '/global/scratch/users/worsham/las_regridded'
neondir <- '/global/scratch/users/worsham/neon_las'
ngdir <- '/global/scratch/users/worsham/neon_las_gaps'
dir.create(outdir)

#############################################
# Pull in neon points to fill gaps and regrid
#############################################

# Ingest gap shapefile
fp.gap <- vect('/global/scratch/users/worsham/EastRiver/missing_flightpath.shp')
fp.gap <- st_as_sf(fp.gap)

# Ingest neon point cloud las catalog
neoncat <- readLAScatalog(list.files(neondir, full.names=T, pattern='*2018062013*')[c(1:5, 13)])

# Grid the gap polygon for faster processing
gridz <- st_make_grid(fp.gap, cellsize=500)
gridz <- st_intersection(fp.gap, gridz)
gridz <- st_as_sf(vect(gridz))

# Set up parallel and output parameters
plan(multisession, workers = 24L)
set_lidr_threads(24L)
opt_laz_compression(neoncat) <- T
opt_output_files(neoncat) <- file.path(outdir, 'neongaps', 'neongaps_las_regridded_{XLEFT}_{YBOTTOM}')

# Clip neon points to gap polygon and regrid, writing to file
gaps <- clip_roi(neoncat, gridz)

######################
# Regrid las catalog
######################
lascat <- readLAScatalog(datadir)
opt_chunk_buffer(lascat) <- 0
opt_chunk_size(lascat) <- 500
opt_laz_compression(lascat) <- T
opt_output_files(lascat) <- file.path(outdir, "las_regridded_{XLEFT}_{YBOTTOM}")

# Preview the chunk pattern
lascat <- lascat[lascat['Number.of.point.records']$Number.of.point.records>0,]
plot(lascat['Number.of.point.records'])

# Retile with multiple cores
plan(multisession, workers = 30L)
set_lidr_threads(30L)
newlascat <- catalog_retile(lascat)

# Check regridded las catalog
lascatrg <- readLAScatalog(list.files(outdir, full.names=T, pattern='.laz'))
plot(lascatrg['Number.of.point.records'])