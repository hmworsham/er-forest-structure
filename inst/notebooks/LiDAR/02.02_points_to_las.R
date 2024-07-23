# Convert discretized points to LAS specification
# Author: Marshall Worsham | worsham@berkeley.edu
# Created: 04-02-22
# Revised: 07-22-24

#############################
# Set up working environment
#############################

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

#############################
# Data ingest
#############################

# Define directories
shapedir <- file.path(config$extdata$scratch, 'EastRiver', 'RMBL_2020_EastRiver_SDP_Boundary')
datadir <- file.path(config$extdata$scratch, 'hyperpointcloud')
outdir <- file.path(config$extdata$scratch, 'las_ungridded')

# Ingest geolocated points
infiles <- list.files(datadir, full.names=T)
did = str_replace(list.files(outdir), '_hpc.las', '_hpc.csv')
did = file.path(datadir, did)
infiles <- infiles[!infiles %in% did]
print(length(infiles))

#############################
# Processing
#############################

# Convert points to las
mclapply(infiles, rwaveform::pts2las, outpath=outdir, mc.cores=getOption('mc.cores', 24L))
