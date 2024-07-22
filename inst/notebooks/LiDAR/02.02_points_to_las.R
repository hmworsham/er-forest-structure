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

# Name directories
datadir <- '/global/scratch/users/worsham/geolocated_returns'
wfdir <- '/global/scratch/users/worsham/waveform_binary_chunks'
outdir <- '/global/scratch/users/worsham/hyperpointcloud'

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

#############################
# Processing
#############################

# Convert points to las
mclapply(infiles, rwaveform::pts2las, outpath=outdir, mc.cores=getOption('mc.cores', 24L))
