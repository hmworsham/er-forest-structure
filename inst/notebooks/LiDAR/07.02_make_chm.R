# Make canopy height model (CHM)
# Author: Marshall Worsham | worsham@berkeley.edu
# Created: 03-21-24
# Revised: 07-23-24

#############################
# Set up working environment
#############################

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

# Configure drive auth
drive_auth(path=config$drivesa)

# Define parallel scope
nCores <- as.integer(availableCores()-2)

#############################
# Data ingest
#############################

# Ingest las
infiles <- list.files(config$extdata$las_dec, full.names=T)
lascat <- readLAScatalog(infiles)

#############################
# Create CHM
#############################

# Processing controls
plan(multisession, workers=nCores)
opt_output_files(lascat) <- file.path(config$extdata$scratch, 'chm', 'chm_{XLEFT}_{YBOTTOM}')
opt_chunk_size(lascat) <- 500
opt_chunk_buffer(lascat) <- 10

# Make CHM with p2r algorithm
chm.pitfree.025 <- rasterize_canopy(lascat, 0.5, p2r(.2), pkg='terra', overwrite=T)
plot(chm.pitfree.025)

# Smooth CHM
kernel <- matrix(1,7,7)
chm.smooth <- focal(chm.pitfree.025, w = kernel, fun = mean, na.rm = TRUE)
plot(chm.smooth)

# Write smoothed CHM as intermediate output
writeRaster(chm.smooth, file.path(config$extdata$scratch, 'chm.smooth.tif'))

# Mask CHM to conifer forest
chm.smooth <- rast(file.path(config$extdata$scratch, 'chm.smooth.tif'))
full.mask <- rast(file.path(config$extdata$scratch, 'tifs', 'fullmask_5m.tif'))
full.mask <- alignfun(full.mask, chm.smooth)
chm.masked <- mask(chm.smooth, full.mask)

#############################
# Write
#############################

# Write masked smoothed CHM
writeRaster(chm.masked, file.path(config$extdata$scratch, 'chm_smooth_masked.tif'))
