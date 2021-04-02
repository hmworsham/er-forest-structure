# Rough exploratory work for waveform LiDAR processing
# Author: Marshall Worsham 
# Revised: 03-26-21

########################
# Front matter
########################

# Install non-CRAN libraries
#devtools::install_github("jrminter/rPeaks") # rPeaks for deconv/decomp
#devtools::install_github('tankwin08/waveformlidar') # waveformlidar for general processing
#devtools::install_github('lwasser/neon-aop-package/neonAOP') #neonAOP for reading binary data

# Install and load typical libraries
pkgs <- c('dplyr',
          'tidyverse',
          'ggplot2',
          'raster',
          'data.table',
          'devtools',
          'plotly',
          'rPeaks',
          'waveformlidar',
          'rgdal',
          'caTools',
          'sf', 
          'parallel') # Name the packages you want to use here

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
# Ingest ENVI binary files
################################

# Name data directory
datadir <- '/Volumes/Brain/GIS/RMBL/NEON_AOP_2018/Waveform_Lidar/Binary_All/'

# Zipfiles
zipfiles <- paste0(datadir, grep(list.files(datadir), pattern = '.7z', value = T))

# Name flightpaths as filenames
flightpaths <- paste0(datadir, grep(list.files(datadir), pattern = '.7z', invert = T, value = T))

# Function to ingest files and store as environment variables
ingest <- function(flightpath){
  
  # Name the 
  obs_bin = grep(list.files(flightpath, full.names = T), # Observation
                 pattern = 'observation', 
                 value = T)
  out_bin = grep(list.files(flightpath, full.names = T), # Outgoing pulse
                 pattern = 'outgoing', 
                 value = T)
  geo_bin = grep(list.files(flightpath, full.names = T), # Geolocation array
                 pattern = 'geolocation', 
                 value = T)
  re_bin = grep(list.files(flightpath, full.names = T), # Return pulse
                pattern = 'return_pulse', 
                value = T)
  imp_re_bin = grep(list.files(flightpath, full.names = T), # System impulse response (for deconvolution)
                    pattern = 'impulse_response_', 
                    value = T)
  imp_out_bin = grep(list.files(flightpath, full.names = T), # Outgoing impulse response (for deconvolution)
                     pattern = 'impulse_response_T0', 
                     value = T)
  
  # Read the binary files as arrays
  obs_array <- read.ENVI(obs_bin[1], headerfile = obs_bin[2])
  out_array <- read.ENVI(out_bin[1], headerfile = out_bin[2])
  geo_array <- read.ENVI(geo_bin[1], headerfile = geo_bin[2])
  re_array <- read.ENVI(re_bin[1], headerfile = re_bin[2])
  imp_re_array <- read.ENVI(imp_re_bin[1], headerfile = imp_re_bin[2])
  imp_out_array <- read.ENVI(imp_out_bin[1], headerfile = imp_out_bin[2])
  
  # Load return as reshaped data table
  out <<- data.table(index=c(1:nrow(out_array)), out_array)
  re <<- data.table(index=c(1:nrow(re_array)), re_array)
  geol <<- data.table(index=c(1:nrow(geo_array)), geo_array)
  sysir <<- data.table(index=c(1:nrow(imp_re_array)), imp_re_array)
  outir <<- data.table(index=c(1:nrow(imp_re_array)), imp_re_array)
  
  # Assign geo column names
  geoindex <- c(1:9,16)
  colnames(geol)[geoindex] <- c('index', 'orix', 'oriy', 'oriz', 'dx', 'dy', 'dz', 'outref', 'refbin', 'outpeak')
  
  # Reassemble data tables into list of objects
  #wf_tbls <- list("out" = out, "re" = re, "geo" = geo, "sysir" = sysir, "outir" = outir)
  #return(wf_tbls)
}

# Ingest one flightpath
fp <- flightpaths[grep('2018_CRBU_1_2018061314_FL014', flightpaths)]
wf <- mclapply(fp, ingest, mc.cores = detectCores())