library(raster)
library(sf)
library(ggplot2)
library(parallel)
library(plyr)

datadir <- '/global/scratch/users/worsham/geolocated_returns'
infiles <- list.files(datadir, full.names=T)

saferead <- function(x){
  tryCatch(read.csv(x, header=T), 
           error = function(cond) {
             message(paste('Reading csv failed'))
           })
}

res <- mclapply(infiles, saferead, mc.cores=getOption('mc.cores', 16))
wf <- do.call(rbind.fill, res)

ptsf <- st_as_sf(wf, coords = c('px', 'py'), crs = '+proj=utm +zone=13 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
st_bbox(ptsf)
plot(st_bbox(ptsf))
rs = raster(matrix(1:10000,100,100), xmx=st_bbox(ptsf)[3], xmn=st_bbox(ptsf)[1], ymn=st_bbox(ptsf)[2], ymx=st_bbox(ptsf)[4], crs='+proj=utm +zone=13 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
res(rs)
plot(rs, col=sample(rainbow(10000)))
res(rs) <- 100
res(rs)
ncell(rs)
values(rs) <- 1:ncell(rs)
plot(rs, col=sample(rainbow(459)))

returns <- data.frame(x=wf$px, y=wf$py)
returns <- wf[!duplicated(wf[,'index']),]
peaks <- data.frame(x=wf$px, y=wf$py)

pointcount = function(ras, wf){
  # make a raster of zeroes like the input
  r2 = ras
  r2[] = 0
  
  # make another raster of zeroes like the input
  r3 = ras
  r3[] = 0
  
  # get n returns
  returns = wf[!duplicated(wf$index),]
  returns = data.frame(x=returns$px, y=returns$py)
  
  # get peaks
  peaks = data.frame(x=wf$px, y=wf$py)
  
  # get the cell index for each point and make a table:
  returns = table(cellFromXY(ras, returns))
  peaks = table(cellFromXY(ras, peaks))
  
  # fill in the raster with the counts from the cell index:
  r2[as.numeric(names(peaks))] = peaks
  r3[as.numeric(names(returns))] = returns
  return(c(r2,r3))
}

density.raster = pointcount(rs, wf)
peaks <- density.raster[[1]]
returns <- density.raster[[2]]

returns <- reclassify(returns, cbind(-Inf, 0, 1), right=T)

dr <- peaks/returns/5
dr <- reclassify(dr, cbind(-Inf, 1, NA), right=T)

res(dr)
extent(dr)

library(sf)
bnd <- st_read('/global/scratch/users/worsham/RMBL_2020_EastRiver_SDP_Boundary/RMBL_2020_EastRiver_SDP_Boundary/SDP_Boundary.shp')


plot(dr, col=topo.colors(20))
plot(bnd, add=T)
ggplot()+
  geom_raster(data=density.raster, aes(x=x, y=y, fill = layer))


spplot(rs2)

writeRaster(density.raster, '~/stand_density_100m', format='GTiff', overwrite=T)
