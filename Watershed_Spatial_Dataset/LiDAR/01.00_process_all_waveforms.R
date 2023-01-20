# Process NEON waveform LiDAR
# Author: Marshall Worsham | worsham@berkeley.edu
# Created: 03-26-21
# Revised: 03-22-22

########################
# Front matter
########################

# Install non-CRAN libraries
#Sys.setenv(GDAL_DATA = '/global/home/groups/consultsw/sl-7.x86_64/modules/gdal/2.2.3/share/gdal/')

# Install and load libraries
pkgs <- c('dplyr',
          'caTools',
          'tidyverse',
          'data.table',
          'devtools',
          'doParallel',
          'ParallelLogger',
          'parallel',
          'pbapply',
          'plyr',
          'rwaveform',
          'rlist') # Name the packages you want to use here

# Function to install new packages if they're not already installed
load.pkgs <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
# Runs the function on the list of packages defined in pkgs
load.pkgs(pkgs)

################################
# Setup workspace
################################

# Name data directory
datadir <- '/global/scratch/users/worsham/waveform_binary_chunks'

# Name output directory
outdir <- '/global/scratch/users/worsham/geolocated_returns'

# Name logpath
logpath = '/global/scratch/users/worsham/logs/sbatch_pwf_log.txt'

# Name flightpaths as filenames
flightpaths <- list.files(datadir, full.names = T)

# Subset flightpaths intersecting forest
# forestcsv <- '~/Output/flightpath_forest_intersections.csv'
# forest <- read.csv(forestcsv)
# isforest <- which(forest$pctforest > 0.05)
# flightpaths <- flightpaths[isforest]

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
