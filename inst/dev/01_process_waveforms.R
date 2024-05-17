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

# Name output directory
outdir <- '/global/scratch/users/worsham/geolocated_returns'

# Name flightpaths as filenames
flightpaths <- list.files(datadir, full.names = T)

# Get forest intersections
forestcsv <- '~/Output/flightpath_forest_intersections.csv'
forest <- read.csv(forestcsv)

# Get plot/LiDAR intersections
intersectscsv <- '~/Output/EastRiver_Plot_LiDAR_Chunk_Intersections.csv'
#intersectcsv <- '/Volumes/GoogleDrive/My Drive/Research/RMBL/RMBL-East River Watershed Forest Data/Data/LiDAR/EastRiver_Plot_LiDAR_Intersections.csv'

intersects <- read.csv(intersectscsv)
names(intersects) <- str_replace(names(intersects), '\\.', '-')

################################
# Subset flighpaths intersecting forest
################################

isforest <- which(forest$pct_forest > 0.05)
flightpaths <- flightpaths[isforest]

##########################################
# Ingest binary files for one flightpath
###########################################

# Define area of interest by plot ID
aoi <- 'CC-UC1'

# Select flightpaths that intersect the aoi
itx_true <- intersects[intersects[aoi] == T,1]
#aoi_fps <- file.path(datadir, itx_true)
aoi_fps <- flightpaths

wf_arrays <- rwaveform::ingest(aoi_fps[2])

# clip waveform to one plot extent
#aoiext = rwaveform::aoiextent(aoi, shapedir)
#xyz = rwaveform::clipwf(wf_arrays, aoiext, buff=20)

did <- list.files('/global/scratch/users/worsham/geolocated_returns')
did = unlist(strsplit(did, '_returnpoints.csv', 1))
fps = list.files(datadir)
fps = fps[isforest]
did = did[!did %in% setdiff(did,fps)] 
length(fps[!fps %in% did])

flightpaths = fps[!fps %in% did]
flightpaths = paste0(datadir, '/',flightpaths)
length(flightpaths)

##########################################
# process waveforms for one flightpath
##########################################

tic <- proc.time()
test1 <- rwaveform::process_wf(flightpaths[648], outdir)
toc <- proc.time()
print(toc-tic)

##########################################
# process waveforms at forested flightpaths
##########################################

mclapply(flightpaths[3:40], rwaveform::process_wf, outdir, mc.cores=getOption('mc.cores', 3L))

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
