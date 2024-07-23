# Regrid LAS catalog
# Author: Marshall Worsham | worsham@berkeley.edu
# Created: 03-31-21
# Revised: 03-02-22

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
datadir <- file.path(config$extdata$scratch, 'las_ungridded')
outdir <- file.path(config$extdata$scratch, 'las_regridded')
dir.create(outdir)

######################
# Regrid las catalog
######################

# Ingest LAS and set processing specs
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
