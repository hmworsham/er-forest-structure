# Normalize points to ground surface
# Author: Marshall Worsham | worsham@berkeley.edu
# Created: 03-31-21
# Revised: 07-23-24

#############################
# Set up working environment
#############################

# Specify RGL option
options(rgl.useNULL=TRUE)

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

# Define directories
shapedir <- file.path(config$extdata$scratch, 'EastRiverInputs', 'RMBL_2020_EastRiver_SDP_Boundary')
datadir <- file.path(config$extdata$scratch, 'las_downsampled')
geredir <- file.path(config$extdata$scratch, 'geolocated_returns')
outdir <- file.path(config$extdata$scratch, 'las_normalized')
dir.create(outdir)

############################
# Prep data
############################

# Ingest las dtm
dtm <- terra::rast(file.path(config$extdata$scratch, 'EastRiverInputs', 'dtm_mosaic.tif'))
dtm <- raster(dtm) # Coerce to raster, as SpatRaster isn't serializable for pll processing

# Ingest gridded points as las catalog
infiles <- list.files(datadir, full.names=T)
lascat <- readLAScatalog(infiles)

# Plot las catalog by number of points in file
#lascat <- lascat[lascat['Number.of.point.records']$Number.of.point.records>0,]
plot(lascat['Number.of.point.records'])
which(lascat['Number.of.point.records']$Number.of.point.records==0,)

# Set chunk buffer and other catalog processing params
#opt_chunk_size(lascat) <- 1000
opt_chunk_buffer(lascat) <- 20
opt_output_files(lascat) <-  paste0(outdir, '/las_norm_{XLEFT}_{YBOTTOM}')
opt_laz_compression(lascat) <- T

# Plot catalog showing buffered chunks to process
plot(lascat, chunk = TRUE)
summary(lascat)

# Set up parallel processing
plan(multisession, workers = 26L)
set_lidr_threads(26L)

# Normalize height on full las catalog
lascat_norm <- normalize_height(lascat, tin(), dtm=dtm)
