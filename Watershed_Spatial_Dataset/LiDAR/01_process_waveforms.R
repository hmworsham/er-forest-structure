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
          'waveformlidar',
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
# Ingest ENVI binary files
################################

# Name data directory
datadir <- '/global/scratch/users/worsham/waveform_binary_split'

# Name directory where inventory plot shapefiles live
shapedir <- '/global/scratch/users/worsham/EastRiver/Plot_Shapefiles/Polygons/'

# Name flightpaths as filenames
flightpaths <- list.files(datadir, full.names = T)
intersects <- read.csv('~/eastriver/Watershed_Spatial_Dataset/LiDAR/Output/EastRiver_Plot_LiDAR_Intersections.csv')
names(intersects) <- str_replace(names(intersects), '\\.', '-')

# Define area of interest by plot ID
aoi <- 'SG-NES2'

# Select flightpaths that intersect the aoi
itx_true <- intersects[intersects[aoi] == T,1]
aoi_fps <- file.path(datadir, itx_true)

# Function to ingest files
ingest <- function(flightpath){
  
  # Name the waveform files to ingest
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
  obs_array = read.ENVI(obs_bin[1], headerfile = obs_bin[2])
  out_array = read.ENVI(out_bin[1], headerfile = out_bin[2])
  geo_array = read.ENVI(geo_bin[1], headerfile = geo_bin[2])
  re_array = read.ENVI(re_bin[1], headerfile = re_bin[2])
  imp_re_array = read.ENVI(imp_re_bin[1], headerfile = imp_re_bin[2])
  imp_out_array = read.ENVI(imp_out_bin[1], headerfile = imp_out_bin[2])
  
  # Load return as reshaped data table
  out = data.table(index=c(1:nrow(out_array)), out_array)
  re = data.table(index=c(1:nrow(re_array)), re_array)
  geol = data.table(index=c(1:nrow(geo_array)), geo_array)
  sysir = data.table(index=c(1:nrow(imp_re_array)), imp_re_array)
  outir = data.table(index=c(1:nrow(imp_re_array)), imp_re_array)
  
  # Assign geo column names
  geoindex = c(1:9,16)
  colnames(geol)[geoindex] = c('index', 'orix', 'oriy', 'oriz', 'dx', 'dy', 'dz', 'outref', 'refbin', 'outpeak')
  
  # Return values
  wf_arrays = list('out' = out, 're' = re, 'geol' = geol, 'sysir' = sysir, 'outir' = outir)
  return(wf_arrays)
}

###################################
# clip waveform to one plot extent
###################################

# Function to clip any waveform
clipwf <- function(wf, geol, aoi, buff=20){
  
  # Specify a plot of interest
  plotpath = list.files(shapedir, 
                      pattern = glob2rx(paste0(aoi,"*shp")),
                      full.names = T)
  plotsf = st_read(plotpath, quiet=T)
  plotbuff = st_buffer(plotsf,
                       buff,
                       endCapStyle = "SQUARE",
                       joinStyle = "MITRE",
                       mitreLimit = 0.05,)
  geoextent = as.list(extent(plotbuff))
  
  # Clip the waveforms that intersect the aoi
  waveform1 = wf[,-1]
  colnames(geol)[2:9] = c('x', 'y', 'z', 'dx', 'dy', 'dz', 'or', 'fr')
  ll = apply(waveform1, 1, wavelen)
  x = geol$x + geol$dx*(round(ll/2)-geol$fr) # use the middle point to represent the waveform position
  y = geol$y + geol$dy*(round(ll/2)-geol$fr)
  ind = which (x >= geoextent[1] & x<= geoextent[2] & y >= geoextent[3] & y<= geoextent[4])
  swaveform = wf[ind,]
  
  return(swaveform)
}

# Function to clip outgoing and return pulse waveforms, using generic clipwf function
doclip <- function(in_arrays, buff=20){
  
  # specify waveform arrays
  out = in_arrays$out
  re = in_arrays$re
  geol = in_arrays$geol
  #colnames(geol)[2:9] = c('x', 'y', 'z', 'dx', 'dy', 'dz', 'or', 'fr')
  
  # Apply the clipwf function to subset waveforms
  out_sub_tmp = clipwf(out, geol, aoi, buff)
  re_sub_tmp = clipwf(re, geol, aoi, buff)
  geol_sub_tmp = clipwf(geol, geol, aoi, buff)
  
  # Trim the results to the same indexes to make sure they match
  out_sub  = out_sub_tmp[out_sub_tmp$index %in% geol_sub_tmp$index]
  out_sub = out_sub[out_sub$index %in% re_sub_tmp$index]
  re_sub = re_sub_tmp[re_sub_tmp$index %in% geol_sub_tmp$index]
  re_sub = re_sub[re_sub$index %in% out_sub$index]
  geol_sub = geol_sub_tmp[geol_sub_tmp$index %in% re_sub$index]
  geol_sub = geol_sub[geol_sub$index %in% out_sub$index]
  
  # Return a list of the clipped arrays
  clips = list('outgoing' = out_sub, 'return' = re_sub, 'geolocation' = geol_sub)
  return(clips)
}

################################################
# Clean error-generating waveforms functions
################################################

# Function to find waveforms that will break Gaussian fit procedure
# gfitesc <- function(x){
#   xre <- x$return
#   apply(xre, 1, function(xwf){
#     tre<-try(decom(xwf), silent = T)
#     if(class(tre) == 'try-error'){
#       tre <- NULL
#     }
#     tre
#   })
# }
# 
# # Function to find indices of problematic waveforms
# gfitindxs <- function(breaks){
#   okindx = which(lengths(breaks)>0)
#   return(okindx)
# }
# 
# # Function to remove those waveforms
# rmbreaks <- function(indxs, wf){
#   wfre <- wf$return
#   wfout <- wf$outgoing
#   wfgeo <- wf$geolocation
#   newre <- wfre[indxs]
#   newout <- wfout[indxs]
#   newgeo <- wfgeo[indxs]
#   
#   return(list('return' = newre, 'outgoing' = newout, 'geolocation' = newgeo))
# }

###################################
# waveform deconvolution function
###################################

deconv.waveforms <- function(rawarray, subarray, method = 'Gold', np = 2, rescale = T){
  # Function to deconvolve an array of waveform returns. Inputs:
    # - rawarray: the full set of waveforms output from the ingest function
    # - subarray: either the full set of waveforms or a subset, the output of doclip function
  # Uses the waveformlidar::deconvolution function with either Gold or Richardson-Lucy algorithm to (1) first deconvolve the system impulse response with the system outgoing response to estimate the true system response and (2) deconvolve the raw waveform returns from target with both the outgoing pulse and the system response. The second step approximately removes outgoing, system, and atmospheric scattering noise from the returns, yielding a direct estimate of the photon energy returned to sensor. 
  # Returns: an nx501 matrix of deconvolved waveform returns
  
  # Repeat the system
  outir_rep = rawarray$outir[rep(seq_len(nrow(rawarray$outir)), nrow(subarray$return))]
  sysir_rep = rawarray$sysir[rep(seq_len(nrow(rawarray$sysir)), nrow(subarray$return))]
  
  # Remove the index columns -- replace them later
  out = subset(subarray$outgoing, select = -index)
  re = subset(subarray$return, select = -index)
  outir_rep = subset(outir_rep, select = -index)
  sysir_rep = subset(sysir_rep, select = -index)
  
  # Convert the arrays to lists for batch deconvolution
  re.ls = as.list(as.data.frame(t(re)))
  out.ls = as.list(as.data.frame(t(out)))
  outir.ls = as.list(as.data.frame(t(outir_rep)))
  sysir.ls = as.list(as.data.frame(t(sysir_rep)))
  
  # Run deconvolution
  decon = mapply(waveformlidar::deconvolution,
               re = re.ls, 
               out = out.ls, 
               imp = sysir.ls,
               imp_out = outir.ls,
               method = method,
               np = np,
               rescale = rescale)
  decon.t = t(decon)
  decon.tbl = data.table(index = 1:nrow(decon.t), decon.t)
  
  return(decon.tbl)
}

###################################
# waveform decomposition functions
###################################

# Function to run waveform decomposition
decom.waveforms <- function(wfarray, deconarray) {
  
  # Define return and geo arrays
  #re = wfarray$return
  re = deconarray
  geol = wfarray$geolocation
  
  # Run adaptive decomposition algorithm on clipped returns
  # Use error handling to identify erroneous or un-decomposable returns
  safe_decomp = function(x){
    tryCatch(decom.adaptive(x, smooth = T, thres = 0.22, width = 3), 
             error = function(e){NA})}
  
  # Apply the safe decomposition to the set
  decom = apply(re, 1, safe_decomp)
  
  # Filter out returns that threw exceptions
  successes = which(!is.na(decom)) 
  decom = decom[successes]
  geol = geol[successes]
  
  # Pull Gaussian parameters
  rfit = do.call('rbind', lapply(decom, '[[', 1)) # Indices of correctly processed waveforms
  gpars = do.call('rbind', lapply(decom, '[[', 3)) # The Gaussian parameters
  
  # Get indices of waveforms that need to be reprocessed
  problem_wfs = setdiff(as.numeric(re[,1]$index), rfit[!is.na(rfit)])
  problem_index = which(lapply(decom, 'is.null')==T)
  
  # Preserve parameters that resulted from successful decomposition
  repars = gpars[!is.na(gpars[,1]),]
  colnames(repars) = c('index', 'A', 'u', 'sigma', 'r', 'A_std', 'u_std', 'sig_std', 'r_std')
  geol = geol[!problem_index]
  geolcols <- c(1:9,16)
  colnames(geol)[geolcols] <- c('index', 'orix', 'oriy', 'oriz', 'dx', 'dy', 'dz', 'outref', 'refbin', 'outpeak')

  return(list('repars' = repars, 'geolocation' = geol))
}

###################################
# functions to run full processing
###################################

## Run full procedure on all waveforms intersecting with aoi

process_wf <- function(fp){
  
  # clip
  wfarrays = ingest(fp)
  print('flightpath ingested')
  cliparrays = doclip(wfarrays, 20)
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

### DEBUG
for(aoi in aois[1]){
  itx_true = intersects[intersects[aoi] == T,1]
  aoi_fpls = file.path(datadir, itx_true)
  wfpts = lapply(aoi_flps, process_wf)
}

View(wfpts)
dc = apply(wfpts[[2]], 1, safe_decomp)
dc
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
aoi_fps[3]
aoi_fps

#testfps <- ingest(aoi_fps[1])
cliparrs <- lapply(testfps, doclip)
fullset <- bind_rows(cliparrs)
outi <- fullset$outgoing
reti <- fullset$return
geoi <- fullset$geolocation

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
