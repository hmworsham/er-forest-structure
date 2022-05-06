# Process NEON waveform LiDAR
# Author: Marshall Worsham | worsham@berkeley.edu
# Created: 03-26-21
# Revised: 03-02-22

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
          'tidyverse',
          'ggplot2',
          'raster',
          'data.table',
          'devtools',
          'plotly',
          'rPeaks',
          'rgdal',
          'caTools',
          'sf', 
          'parallel',
          'itcSegment',
          'rlist',
          'plot3D') # Name the packages you want to use here

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

# Name directory where inventory plot shapefiles live
shapedir <- '/global/scratch/users/worsham/EastRiver/Plot_Shapefiles/Polygons/'

# Name output directory
outdir <- '/global/scratch/users/worsham/geolocated_returns_plots'

# Name logfile
logpath = '/global/scratch/users/worsham/logs/plots_pwf_log.txt'

# Name flightpaths as filenames
flightpaths <- list.files(datadir, full.names = T)

# Get forest intersections
forestcsv <- '~/Output/flightpath_forest_intersections.csv'
forest <- read.csv(forestcsv)

# Get plot/LiDAR intersections
intersectscsv <- '~/Output/EastRiver_Plot_LiDAR_Chunk_Intersections.csv'

intersects <- read.csv(intersectscsv)
names(intersects) <- str_replace(names(intersects), '\\.', '-')

##########################################
# Process waveforms for one flightpath
##########################################

# tic <- proc.time()
# test1 <- rwaveform::process_wf(flightpaths[648], outdir)
# toc <- proc.time()
# print(toc-tic)

##########################################
# process waveforms at all plot locations
##########################################

aop.plots <- c(
  'CC-CVN1',
  'CC-CVN2',
  'CC-CVS1',
  'CC-EMN1',
  'CC-UC1',
  'CC-UC2',
  'ER-APL1',
  'ER-APU1',
  'ER-BME1',
  'ER-BME2',
  'ER-GT1',
  'SG-NES1',
  'SG-NES2',
  'SG-NES3',
  'SG-SWR1',
  'SR-PVG1',
  'WG-WGM1'
)

processatplots <- function(plt, itx, datadir, shapedir){

  # Get flighpaths intersecting plot
  itx_true = itx[itx[plt]==T,1]
  
  # Find which have completed in outdir
  did = lapply(strsplit(list.files(outdir), '_'), '[', c(2:6))
  did = unlist(lapply(did, paste, collapse ='_'))
  incomplete = itx_true[which(!intersects[intersects[plt]==T,1] %in% did)]
  aoi_flps = file.path(datadir, incomplete)

  # Process and write csv
  lapply(aoi_flps, process.wf.clip, plt, datadir, shapedir, buff=2, logpath, outdir)
}

for(i in aop.plots){
  processatplots(i, intersects, datadir, shapedir)
}


dc <- process.wf.clip(file.path(datadir,'2018_CRBU_1_2018061914_FL011-050'), 'ER-BME2', datadir, shapedir, buff=2, logpath, outdir)


#############

# 
# 
# flightpaths <- list.dirs(datadir, full.names = T)
# fp <- flightpaths[grep('2018_CRBU_1_2018061314_FL013', flightpaths)]
# wf <- ingest(flightpaths[245])
# 
# re <- as.numeric(wf$re[500,])
# out <- as.numeric(wf$out[500,])
# geol <- as.numeric(wf$geol[500,])
# sysir <- as.numeric(wf$sysir)
# outir <- as.numeric(wf$outir)
# 
# decon <- deconvolution(re, out, sysir, outir, 'Gold', rescale=F, small_paras = c(30, 2, 1.8, 30, 2, 1.8),
#                        large_paras = c(30, 3, 1.8, 40, 3, 1.8))
# 
# decom <- decom.adaptive(decon)
# 
# decom
# xx <- as.data.frame(decom[[3]])
# 
# ayi<-xx[1:2,]
# sumayi<-0
# x <- 1:100
# for (i in 1:nrow(ayi)){
#   sumayi<-sumayi + ayi[i,2] * exp(-abs(x - ayi[i,3])**ayi[i,5]/(2 * ayi[i,4]**2))
# }
# 
# 
# 
# ayi<-decom[1:3,]
# sumayi<-0
# x<-1:wavelen(y1)
# for (i in 1:nrow(ayi)){
#   sumayi<-sumayi + ayi[i,2] * exp(-abs(x - ayi[i,3])**ayi[i,5]/(2 * ayi[i,4]**2))
# }
# plot(sumayi, type = 'l')
# 
# 
# plot(re[1:100], type='l', main = 'Waveform Intensity Estimation', ylim=c(0,1000), lty='dotted', xlab='Time (ns)', ylab='Amplitude')
# lines(decon[1:100], col='orange')
# lines(sumayi, col='firebrick2')
# legend(x='topright',legend=c('Raw waveform', 'Spectral deconvolution', 'Gaussian decomposition'), lty=c(3,1,1), col = c('black', 'orange', 'firebrick'), cex=0.8)
# 
# ?deconvolution
# geoindex <- c(1:9,16)
# colnames(geol)[geoindex] <- c('index', 'orix', 'oriy', 'oriz', 'dx', 'dy', 'dz', 'outref', 'refbin', 'outpeak')
