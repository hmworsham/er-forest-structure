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
pkgs <- c('caTools',
          'dplyr',
          'tidyverse',
          'ggplot2',
          'raster',
          'data.table',
          'devtools',
          'minpack.lm',
          'plotly',
          'rPeaks',
          'rwaveform',
          'rgdal',
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

setwd('~/Repos/eastriver/Watershed_Spatial_Dataset/LiDAR/')

################################
# Ingest ENVI binary files
################################

# Name data directory
#datadir <- '/Volumes/Brain10/Geospatial/RMBL/NEON_AOP_2018/Waveform_Lidar/Binary_All/'
datadir <- '/Volumes/GoogleDrive/.shortcut-targets-by-id/1xCDkpB9tRCZwEv2R3hSPKvGkQ6kdy8ip/waveformlidarchunks'

# Ingest one flightpath
flightpaths <- list.files(datadir, full.names = T)

fp <- flightpaths[92]
wf <- ingest(fp)
#fp <- flightpaths[grep('2018_CRBU_1_2018061314_FL013', flightpaths)]
#wf <- ingest(fp[1])

out <- wf$out
re <- wf$re
geol <- wf$geol
outir <- wf$outir
sysir <- wf$sysir

# Subset to a manageable set of waveforms for testing
out_sub <- out[120000:121000]
re_sub <- re[120000:121000]
geol_sub <- geol[120000:121000]

## Assign geo column names to work in downstream functions
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
return1 <- lapply(as.list(as.data.frame(t(re_sub))), as.numeric)
out1 <- lapply(as.list(as.data.frame(t(out_sub))), as.numeric)
sysir2 <- lapply(as.list(as.data.frame(t(sysir_rep))), as.numeric)
outir2 <- lapply(as.list(as.data.frame(t(outir_rep))), as.numeric)

decon <- mapply(rwaveform::deconvolution, 
              re=return1,
              out=out1,
              imp=sysir2,
              imp_out=outir2,
              method='Gold',
              np = 3,
              rescale = F,
              small_paras = list(c(20,2,1.2,20,1.2,2)),
              large_paras=list(c(40,4,1.8,40,4,1.8))
              )

# Transpose deconvolution result
tdecon <- t(decon)
dim(tdecon)

# Create a data table of dim nrow tdecon, ncol decon 
decon.dt <- data.table(index=1:nrow(tdecon), tdecon)
dim(decon.dt)

# Count number of weird returns
length(decon.dt[rowSums(decon.dt!=0)])

# Plot some deconvolved returns to check
plot(seq(500), decon[1:500,16], type = 'l')
lines(seq(500), return1[16][[1]], type = 'l')

return1.dt <- data.table(index=1:length(return1), return1)
plot(seq(500), decon[1:500,42], type = 'l', col = sample(rainbow(10)))
for(d in seq(1:100)){
  lines(seq(500), decon[1:500,d], type = 'l', col = sample(rainbow(10)))
}

########################################
# Waveform decomposition
########################################

xx2 <- decom(decon.dt[42], peakfix=F, thres=0.01)
xx <- apply(decon.dt, 1, decom, thres=0.002)

which(lengths(xx)==0)
plot(seq(500), decon[1:500,22], type = 'l', col = sample(rainbow(10)))

which.max(decon.dt[22])
max(decon.dt[22])
decon.dt[22]

plot(xx[42], type = 'l', col = sample(rainbow(10)))
for(d in seq(1:100)){
  lines(seq(500), xx[1:500,d], type = 'l', col = sample(rainbow(10)))
}

###########################
safe_decomp <- function(x){
  tryCatch(decom.adaptive(x, smooth = T, thres = 0.05, width = 5), 
           error = function(e){'NA'})}

# Apply the safe decomposition to the set
dc = apply(fdec, 1, safe_decomp)

# Find which returns failed
fails = which(dc=='NA')
length(fails)
fails

testdeco <- decom(fdec[374], smooth = T, thres = 0.22, width = 3)
testdeco = dc[1438][[1]]
ayi<-data.frame(testdeco[[2]])
sumayi<-0
x <- 1:100
sumayi <- sumayi + ayi[1,2] * exp(-abs(x - ayi[2,2])**ayi[4,2]/(2 * ayi[3,2]**2))
plot(sumayi, type='l')

lines(seq(500), dec[1:500,1438], type = 'l')
lines(seq(100), return1[[1438]][1:100], type='l')

# thing to do is keep successful decompositions and infill successful deconvolutions where necessary

# Filter out returns that threw exceptions
View(dc)
successes = which(dc!='NA')
length(successes)
dc = dc[successes]

geol = geol[successes]
View(geol)

