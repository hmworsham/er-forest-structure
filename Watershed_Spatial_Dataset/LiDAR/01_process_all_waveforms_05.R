# Process NEON waveform LiDAR
# Author: Marshall Worsham | worsham@berkeley.edu
# Created: 03-26-21
# Revised: 02-23-22

########################
# Front matter
########################

# Install non-CRAN libraries
#Sys.setenv(GDAL_DATA = '/global/home/groups/consultsw/sl-7.x86_64/modules/gdal/2.2.3/share/gdal/')
#libproj::libproj_install_proj_data()
#usethis::edit_r_profile() 
#install.packages('libproj', repos = "https://paleolimbot.r-universe.dev")
#devtools::install_github("jrminter/rPeaks") # rPeaks for deconv/decomp
#devtools::install_github('tankwin08/waveformlidar') # waveformlidar for general processing
#devtools::install_github('lwasser/neon-aop-package/neonAOP') #neonAOP for reading binary data

# Install and load typical libraries
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
load_all('~/Repos/rwaveform')

################################
# Setup workspace
################################

# Name data directory
datadir <- '/global/scratch/users/worsham/waveform_binary_chunks'

# Name output directory
outdir <- '/global/scratch/users/worsham/geolocated_returns'

# Name logpath
logpath = '/global/scratch/users/worsham/logs/processwf_log.txt'

# Name flightpaths as filenames
flightpaths <- list.files(datadir, full.names = T)

# Get plot/LiDAR intersections
intersectscsv <- '~/Output/EastRiver_Plot_LiDAR_Chunk_Intersections.csv'
intersects <- read.csv(intersectscsv)
names(intersects) <- str_replace(names(intersects), '\\.', '-')

# Subset flightpaths intersecting forest
forestcsv <- '~/Output/flightpath_forest_intersections.csv'
forest <- read.csv(forestcsv)
isforest <- which(forest$pct_forest > 0.05)
flightpaths <- flightpaths[isforest]

###############################################################
# Process waveforms at forested flightpaths on multiple cores
###############################################################
clearLoggers()
lapply(flightpaths[401:500], rwaveform::process_wf, logpath, outdir)
#process_wf(flightpaths[1], logpath, outdir)

###############################################################
# Process waveforms at forested flightpaths on multiple nodes
###############################################################

# workernodes <- system('srun hostname', intern = TRUE)
# cl <- parallel::makeCluster(workernodes)
# 
# parLapply(
# cl=cl,
# X=flightpaths[100:400],
# fun=rwaveform::process_wf,
# outdir)
