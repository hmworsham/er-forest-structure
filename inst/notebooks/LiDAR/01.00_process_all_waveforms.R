# Process NEON waveform LiDAR
# Author: Marshall Worsham | worsham@berkeley.edu
# Created: 03-26-21
# Revised: 03-22-22

#############################
# Set up working environment
#############################

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

# Name data directory
datadir <- '/global/scratch/users/worsham/waveform_binary_chunks'

# Name output directory
outdir <- '/global/scratch/users/worsham/geolocated_returns'

# Name logpath
logpath = '/global/scratch/users/worsham/logs/sbatch_pwf_log.txt'

# Name flightpaths as filenames
flightpaths <- list.files(datadir, full.names = T)

# Subset flightpaths not already completed
did = str_replace(list.files(outdir), '_returnpoints.csv', '')
did = file.path(datadir, did)
flightpaths <- flightpaths[!flightpaths %in% did]

###############################################################
# Process waveforms at forested flightpaths on multiple cores
###############################################################

main <- function(flightpaths, range, datadir, logpath, outdir){

  # Find which have completed in outdir
  #did = str_replace(list.files(outdir), '_returnpoints.csv', '')
  #did = file.path(datadir, did)
  #lastdid = max(which(flightpaths[range] %in% did))
  #todorange = (range[1]+lastdid):tail(range,1)
  fps = flightpaths[range]

  # Timeout handling
  # tl.process.wf <- function(fp, logpath, outdir) {
  #   setTimeLimit(elapsed=300)
  #   rwaveform::process.wf(fp, logpath, outdir)
  # }

  # Process and write csv
  for(f in fps){
    process.wf(f, logpath, outdir)
  }
}

main(flightpaths, 1:30, datadir, logpath, outdir)

# Check completion
# range = 5001:6000
# did = str_replace(list.files(outdir), '_returnpoints.csv', '')
# did = file.path(datadir, did)
# notdid = which(!flightpaths[range] %in% did)
# notdidabs = notdid+(range[1]-1)
# notdidabs
# length(notdidabs)
#
# lastdid = max(which(flightpaths[range] %in% did))
# todorange = (range[1]+lastdid):tail(range,1)
# print(lastdid)
# print(todorange)
# fps = flightpaths[todorange]
