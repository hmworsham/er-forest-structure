# Rough exploratory work for waveform LiDAR processing
# Author: Marshall Worsham 
# Revised: 03-26-21

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
#wf <- mclapply(fp, ingest, mc.cores = detectCores())

###################################
# clip waveform to one plot extent
###################################

# Name directory where shapefiles live
shapedir <- '~/Google Drive (worsham@berkeley.edu)/Research/RMBL/RMBL_East River Watershed Forest Data/Data/Geospatial/Kueppers_EastRiver_Plot_Shapefiles_2020_WGS84UTM13N/Polygons'

# Specify a plot of interest
plot_sf <- 'SG-SWR1_Bound_poly_WGS84UTM13N.shp'

clipwf <- function(wf, sf, buff=100){
  plotpath = paste(shapedir, sf, sep = '/')
  plotsf = st_read(plotpath, quiet=T)
  plotbuff = st_buffer(plotsf,
                   buff,
                   endCapStyle = "SQUARE",
                   joinStyle = "MITRE",
                   mitreLimit = 0.05,)
  ext = as.list(extent(plotbuff))
  colnames(geol)[2:9] <- c('x', 'y', 'z', 'dx', 'dy', 'dz', 'or', 'fr')
  subset <- waveformclip(wf, geol, geoextent = ext)
  
  return(subset)
}

# Run function to subset outgoing and return pulses
out_sub_rf <- clipwf(out, plot_sf, 200)
re_sub_rf <- clipwf(re, plot_sf, 200)
geol_sub_rf <- clipwf(geol, plot_sf, 200)


# Trim the results to the same indexes to make sure they match
out_sub <- out_sub_rf[out_sub_rf$index %in% geol_sub_rf$index]
out_sub <- out_sub[out_sub$index %in% re_sub_rf$index]
re_sub <- re_sub_rf[re_sub_rf$index %in% geol_sub_rf$index]
re_sub <- re_sub[re_sub$index %in% out_sub$index]
geol_sub <- geol_sub_rf[geol_sub_rf$index %in% re_sub$index]
geol_sub <- geol_sub[geol_sub$index %in% out_sub$index]

# Assign geo column names
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
out1<-as.list(as.data.frame(t(out_sub)))
sysir2<-as.list(as.data.frame(t(sysir_rep)))
outir2<-as.list(as.data.frame(t(outir_rep)))

# Run deconvolution
dec <- mapply(deconvolution, 
              re=return1,
              out=out1,
              imp=sysir2,
              #imp_out=outir2,
              method='Gold',
              np = 3,
              rescale = T
              )

tdec <- t(dec)
fdec <- data.table(index=1:nrow(tdec),tdec)
View(fdec)

out_sub <- subset(testfps$out, select = -index)
re_sub <- subset(testfps$re, select = -index)
sysir_rep <- as.numeric(subset(testfps$sysir, select = -index))
outir_rep <- as.numeric(subset(testfps$outir, select = -index))

return1 <- as.list(as.data.frame(t(re_sub)))
out1<-as.list(as.data.frame(t(out_sub)))
sysir2<-as.list(as.data.frame(t(sysir_rep)))
outir2<-as.list(as.data.frame(t(outir_rep)))

testdecon <- deconvolution(re_sub[445,], out_sub[445,], sysir_rep, sysir_rep)
testdecomp <- decom.adaptive(testdecon)

plot(t(re_sub[445])[1:48], type = 'l', xlab = 't (ns)', ylab = 'Intensity', lwd =5, cex.axis =2, cex.lab = 2)
plot(testdecon[1:48], col = 'darkblue', lwd = 5, type = 'l', xlab = 't (ns)', ylab = 'Intensity', cex.axis = 2, cex.lab = 2)


ayi<-data.frame(testdecomp[[2]])
sumayi<-0
x<-1:48
sumayi<-sumayi + ayi[1,2] * exp(-abs(x - ayi[2,2])**ayi[4,2]/(2 * ayi[3,2]**2))

lines(sumayi, col = 'cyan2', lwd =5, type = 'l', xlab = 't (ns)', ylab = 'Intensity')
legend('topleft', legend = c('Deconvolved', 'Decomposed'), col = c('darkblue', 'cyan2'), lty = 1, box.lty = 0, cex = 1.2)
#final_result <- mapply(peakfind, fdec)



#######################################
# Waveform decomposition
#######################################

#### run on all data
dr3 <- apply(re_sub, 1, decom.adaptive, smooth = T, thres = .27)
View(dr3)
View(re_sub)

rfit3<-do.call("rbind",lapply(dr3,"[[",1)) ## waveform is correctly decomposed with index,some are not correct index by NA

ga3<-do.call("rbind",lapply(dr3,"[[",2))   ###the original results, which can help to get more detailed results.

pa3<-do.call("rbind",lapply(dr3,"[[",3))   ###useful decomposition results for next step or geolocation transformation.

# Get indices of waveforms that were correctly processed
failed_null <- which(lapply(dr3, 'is.null')==TRUE)
failed_na <- setdiff(as.numeric(re_sub[,1]$index), rfit3[!is.na(rfit3),])

#Get indices of waveforms that need to be reprocessed
wid<-setdiff(as.numeric(re_sub[,1]$index), failed_null)

# Subset the params that resulted from successful decompositions 
rpars<-pa3[!is.na(pa3[,1]),]
geol_keep <- geol_sub[!failed_null]
geol_keep

colnames(rpars) <- c('index', 'A', 'u', 'sigma', 'r', 'A_std', 'u_std', 'sig_std', 'r_std')
decomre <- geotransform(decomp=rpars, geol_keep)
decomre

# Create a dataframe with time-bin values estimated through fitting decomposition
View(rpars)

re<-decom(wf[182,],smooth=TRUE, width=5)[[2]]
re[4:6,2]
yi<-cbind(re[1:3,2],re[4:6,2],re[7:9,2])

fp_plot_itx <-  data.frame(matrix(NA, 
                                  nrow = length(flightpaths), 
                                  ncol = nrow(allplots)))
names(fp_plot_itx) <- allplots$PLOT_ID
row.names(fp_plot_itx) <- flightpaths

re_sub_decom <- data.frame(matrix(NA,
                                  nrow=nrow(rpars),
                                  ncol = 250))


gauss_est <- function(x){
  tbins = 1:300
  sumyi = 0
  data = x
  for(i in seq(nrow(data))){
    sumyi = sumyi + data[i,2] * exp(-abs(tbins - data[i,3])**data[i,5]/(2 * data[i,4]**2))
  }
  sumyi
  }

ayi<-xx[1:3,]
sumayi<-0
x<-1:wavelen(y1)
for (i in 1:nrow(ayi)){
  sumayi<-sumayi + ayi[i,2] * exp(-abs(x - ayi[i,3])**ayi[i,5]/(2 * ayi[i,4]**2))
}
plot(sumayi, type = 'l')

gaussed <- lapply(split(xx, xx$index), gauss_est)

plot(gaussed[[1]], type = 'l')
lines(gaussed[[3]])
lines(gaussed[[6]])
lines(gaussed[[900]])

xx <- as.data.frame(rpars)
xx
  group_by(index) %>%
  do(data.frame(val=gauss_est(.)))

for(w in unique(xx$index)){
  for(i in seq(nrow(rpars)))
}

for(i in seq(nrow(rpars))){
  sumyi <- sumyi + yi[]
}

sumyi<-0
x<-1:wavelen(y1)
yi
for (i in 1:3){
  sumyi<-sumyi + yi[i,1] * exp(-(x - yi[i,2])**2/(2 * yi[i,3]**2))
}
plot(sumyi)


#######################################
# Hyper point cloud
#######################################

geol_sub$index<-NULL
colnames(geol_sub)[1:8]<-c("x","y","z","dx","dy","dz","or","fr")
hpc<-hyperpointcloud(waveform=re_sub,geo=geol_sub)
hpc

fig <- plot_ly(data.frame(hpc), x = ~x, y = ~y, z = ~z, 
               marker = list(color = ~log10(intensity), colorscale = c('#FFE1A1', '#683531'),
                             showscale = T,
                             size = 1))
fig <- fig %>% add_markers()
fig

hpcgrid<-waveformgrid(hpc=hpc,res=c(1,1))
hpcgrid$intensity[,1]
plot(hpcgrid$cx, hpcgrid$cy, fill=hpcgrid$intensity[,1])
names(hpcgrid)
ggplot(hpcgrid, aes(x = cx, y = cy)) +
  geom_point(aes(color = intensity[,1]))
View(hpcgrid)


###using raw data
rawgrid<-waveformgrid(waveform = return,geo=geo,method="Other")
##adding quantiles
quangrid<-waveformgrid(hpc=hpc,res=c(1,1),quan=c(0.4,0.48,0.58,0.67,0.75,0.85,0.95))
##for using the other method, we must assign the method
rquangrid<-waveformgrid(waveform = return,geo = geo,res=c(1,1),quan=c(0.4,0.48,0.58,0.67,0.75,0.85,0.95),method="Other")

####waveformvoxel
voxr<-waveformvoxel(hpc,res=c(1,1,0.15))
##adding quan
qvoxr<-waveformvoxel(hpc,res=c(.8,.8,0.4),quan=c(0.4,0.5,0.75,0.86))
qvoxr

fig <- plot_ly(data.frame(qvoxr), x = ~cx, y = ~cy, z = ~cz,
               type = 'scatter3d',
               mode = 'markers',
               color = ~intensity[,6])

fig


###convert the hpc to the composite waveforms
rtc<-rawtocomposite(voxr)
###from hyper point cloud to composite waveforms, the inten_index will determine which intensity variable of the voxel you will use
ph_rtc<-rawtocomposite(qvoxr,inten_index = 6)

voxr





# Import some local data
data(return, package = 'waveformlidar')
wf <- data.table(index = c(1:nrow(return)), return)

# simple waveform decomp
r1 <- waveformlidar::decom(wf[1,])
r2 <- waveformlidar::decom(wf[1,], smooth = F)

r1
r2

data(outg, package = 'waveformlidar')
data(imp, package = 'waveformlidar')
data(imp_out, package = 'waveformlidar')

i = 1
re  <- return[i,]
out <- outg[i,]
imp <- imp
imp_out <- imp_out

gold0 <- waveformlidar::deconvolution(re = re, out = out, imp = imp, imp_out = imp_out)
rl0 <- waveformlidar::deconvolution(re = re, out = out, imp = imp, imp_out = imp_out, method = 'RL')

data(geo, package = 'waveformlidar')
data(return, package = 'waveformlidar')
geo$index <- NULL
colnames(geo)[1:8] <- c('x', 'y', 'z', 'dx', 'dy', 'dz', 'or', 'fr')
hpc <- waveformlidar::hyperpointcloud(waveform = return, geo = geo)
hpc.df <- data.frame(hpc)

install.packages('plotly')
library(plotly)

fig <- plot_ly(hpc.df, x = ~x, y = ~y, z = ~z,
               marker = list(color = ~z, 
                             colorscale = c('#FFE1A1', '#683531', 'navy'), 
                             size = 2,
                             showscale = TRUE))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'X'),
                                   yaxis = list(title = 'Y'),
                                   zaxis = list(title = 'Z')),
                      annotations = list(
                        #x = 1.13,
                        #y = 1.05,
                        #text = 'Miles/(US) gallon',
                        #xref = 'paper',
                        #yref = 'paper',
                        #ashowarrow = FALSE
                      ))
fig



hpcgrid<- waveformlidar::waveformgrid(hpc=hpc,res=c(1,1))
###using raw data
rawgrid<-waveformlidar::waveformgrid(waveform = return,geo=geo,method="Other")
##adding quantiles
quangrid<-waveformlidar::waveformgrid(hpc=hpc,res=c(1,1),quan=c(0.4,0.48,0.58,0.67,0.75,0.85,0.95))
##for using the other method, we must assign the method
rquangrid<-waveformlidar::waveformgrid(waveform = return,geo = geo,res=c(1,1),quan=c(0.4,0.48,0.58,0.67,0.75,0.85,0.95),method="Other")

hpcgrid