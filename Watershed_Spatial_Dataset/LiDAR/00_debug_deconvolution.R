# Debugging deconvolution
# Author: Marshall Worsham 
# Revised: 05-17-2021

########################
# Front matter
########################

# Install tricky libraries
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
datadir <- '/Volumes/Brain10/Geospatial/RMBL/NEON_AOP_2018/Waveform_Lidar/Binary_All/'

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
flightpaths <- list.files(datadir, full.names = T)
fp <- flightpaths[grep('2018_CRBU_1_2018061314_FL013', flightpaths)]
wf <- ingest(fp[1])

# Subset to a manageable set of waveforms for testing
out_sub <- out[1:100000]
re_sub <- re[1:100000]
geol_sub <- geol[1:100000]

## Assign geo column names
geoindex <- c(1:9,16)
colnames(geol_sub)[geoindex] <- c('index', 'orix', 'oriy', 'oriz', 'dx', 'dy', 'dz', 'outref', 'refbin', 'outpeak')

#######################################
# Waveform deconvolution
#######################################

# Repeat the system and outgoing impulse responses to nrow of return pulse
outir_rep <- outir[rep(seq_len(nrow(outir)), nrow(re_sub))]
sysir_rep <- sysir[rep(seq_len(nrow(sysir)), nrow(re_sub))]

# Remove the index columns -- we'll replace them later 
out_sub <- subset(out_sub, select = -index)
re_sub <- subset(re_sub, select = -index)
sysir_rep <- subset(sysir_rep, select = -index)
outir_rep <- subset(outir_rep, select = -index)

# Convert the data into lists before batch deconvolving
return1 <- as.list(as.data.frame(t(re_sub)))
out1 <- as.list(as.data.frame(t(out_sub)))
sysir2 <- as.list(as.data.frame(t(sysir_rep)))
outir2 <- as.list(as.data.frame(t(outir_rep)))

# Run deconvolution
dec <- mapply(deconvolution, 
              re=return1,
              out=out1,
              imp=sysir2,
              imp_out=outir2,
              method='Gold',
              np = 2,
              rescale = T
              )

View(dec)
tdec <- t(dec)
fdec <- data.table(index=1:nrow(tdec),tdec)
View(fdec)
dim(fdec)

plot(seq(100), dec[1:100,58589], type = 'l')
plot(seq(100), dec[1:100,2], type = 'l')

safe_decomp <- function(x){
  tryCatch(decom.adaptive(x, smooth = T, thres = 0.22, width = 3), 
           error = function(e){'NA'})}

# Apply the safe decomposition to the set
dc = apply(fdec, 1, safe_decomp)

testdeco <- decom.adaptive(fdec[58589], smooth = T, thres = 0.22, width = 3)
ayi<-data.frame(testdeco[[2]])
sumayi<-0
x <- 1:100
sumayi <- sumayi + ayi[1,2] * exp(-abs(x - ayi[2,2])**ayi[4,2]/(2 * ayi[3,2]**2))
plot(sumayi, type='l')

testdeco <- decom.adaptive(re_sub[2], smooth = T, thres = 0.22, width = 3)
ayi<-data.frame(testdeco[[2]])
sumayi<-0
x <- 1:100
sumayi<-sumayi + ayi[1,2] * exp(-abs(x - ayi[2,2])**ayi[4,2]/(2 * ayi[3,2]**2))
lines(sumayi)


# Filter out returns that threw exceptions
View(dc)
successes = which(dc!='NA')
length(successes)
dc = dc[successes]

geol = geol[successes]
View(geol)

