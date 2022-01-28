# Process NEON waveform LiDAR
# Author: Marshall Worsham | worsham@berkeley.edu
# Created: 03-26-21
# Revised: 05-18-21

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
load_all('~/rwaveform')

################################
# Setup workspace
################################

# Name data directory
datadir <- '/global/scratch/users/worsham/waveform_binary_chunks'
#datadir <- '/Volumes/GoogleDrive/My Drive/Research/RMBL/RMBL-East River Watershed Forest Data/Data/LiDAR/waveform_lidar_chunks'

# Name directory where inventory plot shapefiles live
shapedir <- '/global/scratch/users/worsham/EastRiver/Plot_Shapefiles/Polygons/'
#shapedir <- '/Volumes/GoogleDrive/My Drive/Research/RMBL/RMBL-East River Watershed Forest Data/Data/Geospatial/Kueppers_EastRiver_Plot_Shapefiles_WGS84UTM13N/Polygons'

# Name flightpaths as filenames
flightpaths <- list.files(datadir, full.names = T)
intersectscsv <- '~/EastRiver_Plot_LiDAR_Chunk_Intersections.csv'
#intersectcsv <- '/Volumes/GoogleDrive/My Drive/Research/RMBL/RMBL-East River Watershed Forest Data/Data/LiDAR/EastRiver_Plot_LiDAR_Intersections.csv'
intersects <- read.csv(intersectscsv)
names(intersects) <- str_replace(names(intersects), '\\.', '-')

################################
# Ingest binary files for one flightpath
################################

# Define area of interest by plot ID
aoi <- 'CC-UC1'

# Select flightpaths that intersect the aoi
itx_true <- intersects[intersects[aoi] == T,1]
aoi_fps <- file.path(datadir, itx_true)
aoi_fps <- flightpaths
wf_arrays = ingest(aoi_fps[1])
aoi_fps

# clip waveform to one plot extent
aoiext = rwaveform::aoiextent('SG-NES2', shapedir)
xyz = rwaveform::clipwf(wf_arrays, aoiext, buff=20)

# Subset to a manageable set of waveforms for testing
out <- wf_arrays$out
re <- wf_arrays$re
geol <- wf_arrays$geol
outir <- wf_arrays$outir
sysir <- wf_arrays$sysir

out_sub <- out[500000:510000]
re_sub <- re[500000:510000]
geol_sub <- geol[500000:510000]
sub_arrays = list('out'=out_sub, 're'=re_sub, 'geol'=geol_sub)
################################
# deconvolve waveforms
################################
tic <- proc.time()
decon <- rwaveform::deconv.apply(
  wf_arrays, 
  wf_arrays, 
  method='Gold', 
  rescale=F,
  small_paras = list(c(30,2,1.2,30,2,2)),
  large_paras = list(c(40,4,1.8,30,2,2)))
toc <- proc.time()
print(toc-tic)

# Check for NaNs and extreme values
#decon <- subset(decon, select = -index)
nanrows = which(rowSums(is.na(decon))>0)
bigrows = which(rowSums(decon[,2:501])>10^5)
print(nanrows)
print(bigrows)

# Clean NaNs and extreme values
if(length(nanrows) | length(bigrows)) {
  decon <- deconv.clean(decon)
}

# Restore original index values
#newindex <- re_sub[-nanrows]$index
#decon$index <- re_sub$index
#setindex(decon, index)

# Find npeaks
decon <- data.table(t(apply(decon, 1, peakfix)))
np <- apply(decon, 1, npeaks, smooth=F, threshold=0)

# Store indices of returns with potentially unreasonable number of peaks or 0 peaks
#unreasonable <- decon[np>12]$index
nopeaks <- which(np==0)

# Filter out 0 and unreasonable peak vectors
decon <- decon[-nopeaks,]
#decon <- decon[-unreasonable,]

# waveform decomposition 
unload('rwaveform')
load_all('~/Repos/rwaveform')
cl <- makeCluster(detectCores())
decomp <- mclapply(re,
               1,
               rwaveform::decom.adaptive,
               smooth=T,
               peakfix=T,
               thres=0.05,
               width=3)
stopCluster(cl)
par
length(which(lengths(decomp)==0))

###################################
# functions to run full processing
###################################

## Run full procedure on all waveforms intersecting with aoi

process_wf <- function(fp, clip=FALSE, aoi){
  
  # Ingest waveforms
  wfarrays = ingest(fp)
  print(paste('flightpath', fp, 'ingested'))
  
  # Clip waveforms to geometry if specified
  if (clip) {
    
    arrays = rwaveform::clipwf(wfarrays, aoiext, buff=20)
    
    if(dim(arrays[[1]])[1]==0) {
      print('clip failed: flightpath does not intersect plot')
    } else {
      print('clip succeeded')
    }
  } else {
    arrays = wfarrays
  }
  
  # deconvolve
  decon <- rwaveform::deconv.apply(
    wfarrays, 
    arrays, 
    method='Gold', 
    rescale=F,
    small_paras = list(c(30,2,1.2,30,2,2)),
    large_paras = list(c(40,4,1.8,30,2,2)))

  # Check for NaNs and extreme values
  #decon <- subset(decon, select = -index)
  nanrows = which(rowSums(is.na(decon))>0)
  bigrows = which(rowSums(decon[,2:length(decon)])>10^5)
  
  # Clean NaNs and extreme values
  if(length(nanrows) | length(bigrows)) {
    decon <- deconv.clean(decon)
  }
  
  # Find npeaks
  decon <- data.table(t(apply(decon, 1, peakfix)))
  np <- apply(decon, 1, npeaks, smooth=F, threshold=0)
  
  # Store indices of returns with potentially unreasonable number of peaks or 0 peaks
  #unreasonable <- decon[np>12]$index
  nopeaks <- which(np==0)
  
  # Filter out 0 and unreasonable peak vectors
  decon <- decon[-nopeaks,]
  #decon <- decon[-unreasonable,]
  
  # Decompose waveforms
  decomp <- apply(decon,
                  1,
                  rwaveform::decom.adaptive,
                  smooth=T,
                  peakfix=T,
                  thres=0.05,
                  width=3)
  
  # geotransform waveforms to points
  wfpts = geotransform(decomp = decom$repars, decom$geolocation)
  
  return(wfpts)
}

##########################################
# process waveforms for one flightpath
##########################################

tic <- proc.time()
test1 <- process_wf(aoi_fps[1], clip=F)
toc <- proc.time() - tic

##########################################
# process waveforms at all plot locations
##########################################


aois <- names(intersects)[-c(1,3,4,9,10)] # subset plots within AOP acquisition area

intersects[intersects[aoi] == T,1]

### DEBUG
for(aoi in aois[6]){
  itx_true = intersects[intersects[aoi] == T,1]
  aoi_flps = file.path(datadir, itx_true)
  wfpts = lapply(aoi_flps, process_wf)
}

View(wfpts)
dc = apply(wfpts[[2]], 1, safe_decomp)

for(aoi in aois){
  itx_true = intersects[intersects[aoi] == T,1]
  aoi_flps = file.path(datadir, itx_true)
  wfpts = lapply(aoi_flps, process_wf)
  wfpts_df = data.frame(do.call('rbind', wfpts))
  write.csv(wfpts_df, paste0('~/Desktop/RMBL/Projects/Watershed_Spatial_Dataset/Output/waveform_points_02/', aoi, '.csv'))
}
