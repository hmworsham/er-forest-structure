library(mgcv)
library(ggplot2)
library(corrplot)
library(terra)
library(tidyverse)
library(sf)
library(visreg)
library(psych)
library(raster)
library(RColorBrewer)
library(viridis)


datadir <- file.path('/Volumes', 'GoogleDrive', 'My Drive', 'Research', 'RMBL', 'RMBL-East River Watershed Forest Data', 'Data')
snow.dir <- file.path(datadir, 'Geospatial', 'ASO_Snow')
sfdir <- file.path(datadir, 'Geospatial', 'RMBL_2020_EastRiver_SDP', 'RMBL_2020_EastRiver_SDP_Boundary')
outdir <- file.path(snow.dir, 'Processed')

snow.files <- list.files(snow.dir, pattern='SWE.+tif$|swe.+tif$', recursive=T, full.names=T)

aop <- st_read(file.path(sfdir, 'SDP_Boundary.shp'))

snow.ras <- lapply(snow.files, raster)

cropfun <- function(ras, shp){
  ras <- crop(ras, extent(shp))
  ras <- mask(ras, shp)
  return(ras)
}

alignfun <- function(x, target, method='bilinear'){
  xnew = resample(x, target, method)
  ex = extent(target)
  xnew = crop(xnew, ex)
  return(xnew)
}

snow.ras <- sapply(snow.ras, cropfun, aop)
snow.ras <- snow.ras[c(1:4,6,5)]
snow.ras <- sapply(snow.ras, alignfun, snow.ras[[1]])

lapply(snow.ras, function(x){writeRaster(x, file.path(outdir, paste0(names(x), '_processed.tif')), format='GTiff')})

swe.mean <- mean(snow.ras[[1]], snow.ras[[3]], snow.ras[[5]])
writeRaster(swe.mean, file.path(outdir, 'mean_swe_18-22.tif'), format='GTiff')

aso.dates <- data.frame('Flight_Number'=seq(1:6), 
                        'Date'=as.Date(c('2018-03-31',
                                         '2018-05-24',
                                         '2019-04-07',
                                         '2019-06-10',
                                         '2022-04-21',
                                         '2022-05-28')))


delta.t <- diff(aso.dates$Date)

delta.swe.18 <- snow.ras[[1]]-snow.ras[[2]]
delta.swe.18[delta.swe.18 < 0] <- 0
delta.swe.18 <- delta.swe.18 / delta.t[[1]]

plot(delta.swe.18, col=viridis(5, option='C'))
writeRaster(delta.swe.18, file.path(outdir, 'delta_swe_18.tif'), format='GTiff')


delta.swe.19 <- snow.ras[[3]]-snow.ras[[4]]
delta.swe.19[delta.swe.19 < 0] <- 0
delta.swe.19 <- delta.swe.19 / delta.t[[3]]
plot(delta.swe.19, col=viridis(5, option='C'))

writeRaster(delta.swe.19, file.path(outdir, 'delta_swe_19.tif'), format='GTiff')


delta.swe.20 <- snow.ras[[5]]-snow.ras[[6]]
delta.swe.20[delta.swe.20 < 0] <- 0
delta.swe.20 <- delta.swe.20 / delta.t[[3]]
plot(delta.swe.20, col=viridis(5, option='C'))

writeRaster(delta.swe.20, file.path(outdir, 'delta_swe_20.tif'), format='GTiff')

mean.delta.swe <- mean(delta.swe.18, delta.swe.19, delta.swe.20)
writeRaster(mean.delta.swe, file.path(outdir, 'mean_delta_swe.tif'), format='GTiff')

delta.swes <- list(delta.swe.18, delta.swe.19, delta.swe.20)

par(mfcol=c(1, 3), mar=rep(3,4))
plot(delta.swes[[1]], col=viridis(5, option='E'))
title(main='∆SWE 2018')
plot(delta.swes[[2]], col=viridis(5, option='E'))
title(main='∆SWE 2019')
plot(delta.swes[[3]], col=viridis(5, option='E'))
title(main='∆SWE 2022')

par(mfcol=c(2,3), mar=rep(2,4))
for(i in seq_along(snow.ras)) {
  plot(snow.ras[[i]], col=rev(brewer.pal(5, 'Blues')))
  title(main=paste('SWE', aso.dates$Date[i]))
}
