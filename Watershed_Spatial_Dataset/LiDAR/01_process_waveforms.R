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
load_all('~/Repos/rwaveform')

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

# Get forest intersections
forestcsv <- '~/flightpath_forest_intersections.csv'
forest <- read.csv(forestcsv)

# Get plot/LiDAR intersections
intersectscsv <- '~/EastRiver_Plot_LiDAR_Chunk_Intersections.csv'
#intersectcsv <- '/Volumes/GoogleDrive/My Drive/Research/RMBL/RMBL-East River Watershed Forest Data/Data/LiDAR/EastRiver_Plot_LiDAR_Intersections.csv'

intersects <- read.csv(intersectscsv)
names(intersects) <- str_replace(names(intersects), '\\.', '-')

################################
# Subset flighpaths intersecting forest
################################

isforest <- which(forest$pct_forest > 0.08)
flightpaths <- flightpaths[isforest]

################################
# Ingest binary files for one flightpath
################################

# Define area of interest by plot ID
aoi <- 'CC-UC1'

# Select flightpaths that intersect the aoi
itx_true <- intersects[intersects[aoi] == T,1]
#aoi_fps <- file.path(datadir, itx_true)
aoi_fps <- flightpaths
wf_arrays = rwaveform::ingest(aoi_fps[42])

# clip waveform to one plot extent
#aoiext = rwaveform::aoiextent(aoi, shapedir)
#xyz = rwaveform::clipwf(wf_arrays, aoiext, buff=20)

# Subset to a manageable set of waveforms for testing
out <- wf_arrays$out
re <- wf_arrays$re
geol <- wf_arrays$geol
outir <- wf_arrays$outir
sysir <- wf_arrays$sysir

out_sub <- out[100000:100100]
re_sub <- re[100000:100100]
geol_sub <- geol[100000:100100]
sub_arrays = list('out'=out_sub, 're'=re_sub, 'geol'=geol_sub)

################################
# deconvolve waveforms
################################
tic <- proc.time()
decon <- rwaveform::deconv.apply(
  wf_arrays, 
  sub_arrays, 
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
unreasonable <- decon[np>12]$index
nopeaks <- which(np==0)

# Filter out 0 and unreasonable peak vectors
if (length(nopeaks)| length(unreasonable)){
  decon <- decon[-nopeaks,]
  #decon <- decon[-unreasonable,]
}

################################
# Decompose waveforms
################################

# Remove the index columns -- we'll replace them later
decon = subset(decon, select = -index)
geol = subset(geol, select = -index)

## Convert the arrays to lists for batch deconvolution
decon2 = lapply(as.list(as.data.frame(t(decon))), as.numeric)
#geol2 = lapply(as.list(as.data.frame(t(geol))), as.numeric)

# Run adaptive decomposition algorithm on clipped returns
# Use error handling to identify erroneous or un-decomposable returns
safe_decomp = function(x){
  tryCatch(decom.adaptive(x, smooth = T, peakfix=T, thres = 0.2, width = 3), 
           error = function(e){NA})}


# Apply safe decomposition to the set
# DOESN'T WORK
decomp = pbmcmapply(
  safe_decomp,
  decon2,
  mc.cores=getOption("mc.cores", ceiling(detectCores()/2))
)

# WORKS BUT ON ONE CORE
decomp = mapply(
  safe_decomp,
  decon
)

decomp

# WORKS BUT ON ONE CORE
decomp = apply(
  decon,
  1,
  safe_decomp
)

x <- rnorm(1e4, mean = 12, sd = 5)
dt <- as.data.table( x )
dt[ , c("y1", "y2", "y3") := as.vector( mode = "double", NA ) ]


# BREAKS
tic <- proc.time()
decomp <- rwaveform::decom.waveforms(
  sub_arrays,
  decon,
  smooth=T,
  thres=0.,
  window=3)
toc <- proc.time()
print(toc-tic)

##########################################
# process waveforms for one flightpath
##########################################

tic <- proc.time()
test1 <- rwaveform::process_wf(flightpaths[42])
toc <- proc.time()
print(toc-tic)

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
