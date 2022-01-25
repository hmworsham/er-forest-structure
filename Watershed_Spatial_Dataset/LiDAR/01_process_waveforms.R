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
          'rwaveform',
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
#datadir <- '/global/scratch/users/worsham/waveform_binary_split'
datadir <- '/Volumes/GoogleDrive/.shortcut-targets-by-id/1xCDkpB9tRCZwEv2R3hSPKvGkQ6kdy8ip/waveformlidarchunks'

# Name directory where inventory plot shapefiles live
#shapedir <- '/global/scratch/users/worsham/EastRiver/Plot_Shapefiles/Polygons/'
shapedir <- '/Volumes/GoogleDrive/My Drive/Research/RMBL/RMBL-East River Watershed Forest Data/Data/Geospatial/Kueppers_EastRiver_Plot_Shapefiles_WGS84UTM13N/Polygons'

# Name flightpaths as filenames
flightpaths <- list.files(datadir, full.names = T)
#intersectscsv <- '~/eastriver/Watershed_Spatial_Dataset/LiDAR/Output/EastRiver_Plot_LiDAR_Intersections.csv'
intersectcsv <- '/Volumes/GoogleDrive/My Drive/Research/RMBL/Working_Files/Watershed_Spatial_Dataset/Output/EastRiver_Plot_LiDAR_Intersections.csv'

intersects <- read.csv(intersectcsv)
names(intersects) <- str_replace(names(intersects), '\\.', '-')

################################
# Ingest binary files for one flightpath
################################

# Define area of interest by plot ID
aoi <- 'SG-NES2'

# Select flightpaths that intersect the aoi
itx_true <- intersects[intersects[aoi] == T,1]
aoi_fps <- file.path(datadir, itx_true)
#aoi_fps <- flightpaths
wf_arrays = ingest(aoi_fps[2])

# clip waveform to one plot extent
#aoiext = rwaveform::aoiextent('SG-NES2', shapedir)
#xyz = rwaveform::clipwf(wf_arrays, aoiext, buff=20)

# Subset to a manageable set of waveforms for testing

out <- wf_arrays$out
re <- wf_arrays$re
geol <- wf_arrays$geol
outir <- wf_arrays$outir
sysir <- wf_arrays$sysir

out_sub <- out[120000:121000]
re_sub <- re[120000:121000]
geol_sub <- geol[120000:121000]

sub_arrays = list('out'=out_sub, 're'=re_sub, 'geol'=geol_sub)

# deconvolve waveforms
library(rwaveform)
decon <- rwaveform::deconv.apply(wf_arrays, sub_arrays, method='RL')

# Restore original index values
decon$index <- re_sub$index

# Check for NaNs and extreme values
deconvals <- subset(decon, select = -index)
nanrows = which(rowSums(is.na(deconvals))>0)
bigrows = which(rowSums(deconvals)>10^5)

# Clean NaNs and extreme values
if(length(nanrows) | length(bigrows)) {
  decon <- deconv.clean(decon)
}


# waveform decomposition 



###################################
# functions to run full processing
###################################

## Run full procedure on all waveforms intersecting with aoi

process_wf <- function(fp){
  
  # clip
  wfarrays = ingest(fp)
  print('flightpath ingested')
  #cliparrays = doclip(wfarrays, 20)
  cliparrays = wfarrays
  if(dim(cliparrays[[1]])[1]==0){
    print('clip failed: flightpath does not intersect plot')
    return()
  }
  else{
    print('clip succeeded')
    
    # deconvolve
    deconv = deconv.waveforms(wfarrays, cliparrays, method = 'Gold')
    print(dim(deconv))

    # decompose
    decom = decom.waveforms(cliparrays, deconv)
    print('decomposition succeeded')

    # geotransform waveforms to points
    #wfpts = geotransform(decomp = decom$repars, decom$geolocation)
  }
  
  return(decom)
}

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

######################################
# Process 1 waveform for illustration
######################################
process_wf(aoi_fps[3])
aoi_fps
aoi_fps[3]
aoi_fps

testfps <- ingest(aoi_fps[1])
cliparrs <- lapply(testfps, doclip)
fullset <- bind_rows(cliparrs)
outi <- testfps$out
reti <- testfps$re
geoi <- testfps$geol

geoi$index <- NULL
colnames(geoi)[1:8]<-c("x","y","z","dx","dy","dz","or","fr")
hpc<-data.frame(hyperpointcloud(waveform=reti,geo=geoi))


fig <- plot_ly(data.frame(hpc), x = ~x, y = ~y, z = ~z, 
               type = 'scatter3d',
               mode = 'markers',
               marker = list(color = ~log10(intensity), colorscale = c('#FFE1A1', '#683531'),
                             showscale = T,
                             size = 1)) 
fig <- fig %>%
  layout(scene = list(xaxis = list(title = 'X',range=c(327700, 327850)),
                      yaxis = list(title = 'Y',range=c(4310900,4310960)),
                      zaxis = list(title = 'Z', range = c(3300,3700))))
fig

library(plot3D)

rs <- sample(nrow(hpc), 500000, replace = F)
hpc <- hpc[rs,]
scatter3D(hpc$x, hpc$y, hpc$z, colvar = log10(hpc$intensity), pch = '.', col = ramp.col(c('grey90', 'darkblue'), alpha = 0.6), ticktype='detailed')
