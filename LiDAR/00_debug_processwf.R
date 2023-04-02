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
          'rwaveform',
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

wf_arrays = rwaveform::ingest(aoi_fps[648])

# clip waveform to one plot extent
#aoiext = rwaveform::aoiextent(aoi, shapedir)
#xyz = rwaveform::clipwf(wf_arrays, aoiext, buff=20)

# Subset to a manageable set of waveforms for testing
out <- wf_arrays$out
re <- wf_arrays$re
geol <- wf_arrays$geol
outir <- wf_arrays$outir
sysir <- wf_arrays$sysir

out_sub <- out[300000:305000]
re_sub <- re[300000:305000]
geol_sub <- geol[300000:305000]
sub_arrays = list('out'=out_sub, 're'=re_sub, 'geol'=geol_sub)

set.seed(123)
sub <- sample(seq(1:nrow(out)), 5000, replace=F)
#sub <- seq(1:nrow(re))
out_sub <- out[sub]
re_sub <- re[sub]
geol_sub <- geol[sub]
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
decon$index <- re_sub$index
View(decon)

# Check for NaNs and extreme values
#decon <- subset(decon, select = -index)
nanrows = which(rowSums(is.na(decon))>0)
bigrows = which(rowSums(decon[,2:501])>10^5)
print(length(nanrows))
print(bigrows)

#Test if there are nanrows or bigrows
nanrows = c(2,7,226)
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
unreasonable <- decon[np>18]$index
nopeaks <- decon[np==0]$index

# Filter out 0 and unreasonable peak vectors
#dim(decon[!decon$index %in% unreasonable,])

if (length(nopeaks) | length(unreasonable)){
  decon <- decon[!decon$index %in% nopeaks,]
  decon <- decon[!decon$index %in% unreasonable,]
}
decon$index ######### AT THIS POINT THE RESET INDEX STILL HOLDS

################################
# Decompose waveforms
################################

# Define return and geo arrays
#re = wfarray$return
geol = sub_arrays$geol

# WORKS
tic <- proc.time()
decomp <- rwaveform::decom.apply(
  sub_arrays,
  deconvolved=T,
  decon,
  smooth=T,
  thres=0.05,
  window=3)
toc <- proc.time()
print(toc-tic)

############ DEBUGGING #################
rawarray = sub_arrays
deconvolved=T
deconarray = decon
peakfix=F
smooth=T
thres=0.05
window=3

if (deconvolved){
  re <- deconarray
} else {
  re <- rawarray$re
}

# Define return and geo arrays
geol = rawarray$geol

## Convert the arrays to lists for batch deconvolution
idx = re$index
#re = subset(re, select = -index)
decon2 = lapply(as.list(as.data.frame(t(re))), as.numeric)
# Run adaptive decomposition algorithm on clipped returns
# Use error handling to identify erroneous or un-decomposable returns
safedecomp_ = function(x, peakfix=peakfix, smooth = smooth, thres = thres, width = window){
  tryCatch(decom.adaptive(x, peakfix=peakfix, smooth = smooth, thres = thres, width = window), 
           error = function(e){NA})}

# Apply safe decomposition to the set
decomp = pbmclapply(
  decon2,
  safedecomp_,
  peakfix=peakfix,
  smooth=smooth,
  thres=thres,
  width=window,
  mc.cores=getOption("mc.cores", ceiling(detectCores()-2))
)
decomp

# Filter out returns that threw exceptions
successes = which(!is.na(decomp) & lengths(decomp)>=3)
#successes = sample(seq(1:length(successes)), 2920)
#successes = sort(successes)
length(successes)

decomp2 = decomp[successes]
idx2 = idx[successes]
geol2 = geol[geol$index %in% idx2,]

# sub <- sample(seq(1:nrow(out_sub)), 10000)
# #sub <- seq(1:nrow(re))
# out_sub <- out[sub]
# re_sub <- re[sub]
# geol_sub2 <- geol[sub]
# geol_sub2

# Pull Gaussian parameters
rfit = do.call('rbind', lapply(decomp2, '[[', 1)) # Indices of correctly processed waveforms
gpars = do.call('rbind', lapply(decomp2, '[[', 3)) # The Gaussian parameters

# Get indices of waveforms that need to be reprocessed
problem_wfs = setdiff(as.numeric(idx), as.numeric(idx2))
print(paste('There were', length(problem_wfs), 'problems. Retrying with stricter parameters.'))

reprocess_ <- function(probwfs, ge=geol, th, wd){
  
  # Set problematic waveforms to list
  decon.prob = lapply(as.list(as.data.frame(t(re[re$index %in% probwfs]))), as.numeric)
  geol.prob = ge[ge$index %in% probwfs,]
  
  # Attempt decomposition with stricter threshold parameters
  decomp.prob = pbmcmapply(
    safedecomp_,
    decon.prob,
    peakfix=peakfix,
    smooth=smooth,
    thres=th,
    width=wd,
    mc.cores=getOption("mc.cores", ceiling(detectCores()-2))
  )
  
  # Filter out returns that threw exceptions
  successes2 = which(!is.na(decomp.prob) & lengths(decomp.prob)>=3)
  print(paste('successes2:', length(successes2)))
  idx3 = probwfs[successes2]
  print(paste('idx3:', length(idx3)))
  
  if (length(successes2)) {
    decomp3 = decomp.prob[successes2]
    geol3 = geol.prob[geol.prob$index %in% idx3,]
    #print(paste('decomp3', length(decomp3)))
    #print(paste('geol3', nrow(geol3)))
    
    # Pull indices and Gaussian parameters
    rfit.prob = do.call('rbind', lapply(decomp3, '[[', 1)) # Indices of correctly processed waveforms
    gpars.prob = do.call('rbind', lapply(decomp3, '[[', 3)) # The Gaussian parameters
    
    # Bind re-processed problematic waveform indices and Gaussian params to first successful set
    rfit = rbind(rfit, rfit.prob)
    gpars = rbind(gpars, gpars.prob)
    geol2 = rbind(geol2, geol3)
    print(c(nrow(rfit), nrow(gpars), nrow(geol2)))
  }
  
  return(list('rfit'=rfit, 'gpars' = gpars, 'geol2'=geol2, 'successidx'=idx3))
}
# 
# dim(rfit)
# dim(gpars)
# dim(geol2)

# Retry with more strict params
if (length(problem_wfs)) {
  retry1 = reprocess_(problem_wfs, geol, 0.25, 5)
  problem_wfs = setdiff(as.numeric(problem_wfs), as.numeric(retry1$successidx))
  rfit = retry1$rfit
  gpars = retry1$gpars
  geol2 = retry1$geol2
  
  # Retry with extremely strict params
  if(length(problem_wfs)) {
    print(paste('There were', length(problem_wfs), 'problems. Retrying with strictest parameters.'))
    retry2 = reprocess_(problem_wfs, geol, 0.99, 9)
    problem_wfs = setdiff(as.numeric(problem_wfs), as.numeric(retry2$successidx))
    rfit = retry2$rfit
    gpars = retry2$gpars
    geol2 = retry2$geol2
  }
}

dim(rfit)
dim(gpars)
dim(geol2)
dim(retry1$rfit)
dim(retry1$gpars)
dim(retry1$geol2)
dim(retry2$rfit)
dim(retry2$gpars)
dim(retry2$geol2)

length(unique(geol2$index))

# Preserve parameters that resulted from successful decomposition
repars = gpars[!is.na(gpars[,1]),]
colnames(repars) = c('index', 'A', 'u', 'sigma', 'r', 'A_std', 'u_std', 'sig_std', 'r_std')
geolcols <- c(1:9,16)
colnames(geol2)[geolcols] <- c('index', 'orix', 'oriy', 'oriz', 'dx', 'dy', 'dz', 'outref', 'refbin', 'outpeak')

return(list('repars' = repars, 'geolocation' = geol2))


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
