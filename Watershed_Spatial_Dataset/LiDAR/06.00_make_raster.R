library(raster)
library(sf)
library(ggplot2)
library(parallel)
library(plyr)
library(rlas)
library(lidR)
library(readxl)
library(moments)
library(data.table)
library(viridis)

datadir <- '/global/scratch/users/worsham/gridded_returns'
infiles <- list.files(datadir, full.names=T)

# Create ground-normalized point cloud
nlas1 <- pts2las(datadir, range=1:502)
nlas2 <- pts2las(datadir, range=502:length(infiles))
nlas <- c(nlas1, nlas2)

###################################
# Find trees using search function
###################################

# Define search function
f = function(x) {
  y <- 2.2 * (-(exp(-0.08*(x-2)) - 1)) + 3
  y[x < 2] <- 3
  y[x > 20] <- 7
  return(y)
}

# Find trees
trees = lapply(nlas, locate_trees, lmf(f))

# Write trees as csvs for safekeeping
for(i in seq(length(trees))){
  write.csv(trees[[i]], paste0('/global/scratch/users/worsham/trees_100K/', sprintf("trees_%04d",i), '.csv'))
}

datadir <- '/global/scratch/users/worsham/trees_100K'
trfiles <- list.files(datadir, full.names=T)

gettrees <- function(fn){
  df = read.csv(fn, col.names=c('index','tree_id', 'Z', 'X', 'Y', 'Zg'), header=F)[-1,]
  df$X = as.numeric(gsub('c\\(', '', df$X))
  df$Y = as.numeric(df$Y)
  df$Z = as.numeric(df$Z)
  df[c(2:5)]
}

trees <- mclapply(trfiles, gettrees, mc.cores=getOption('mc.cores', 16))

# Bind all trees together into one dataframe
alltrees <- rbindlist(trees)

# Create a dataframe based on tree geometries
head(alltrees)

# Add diameter predictions to trees
alltrees$D <- -8.1946+16.2768*log(alltrees$Z)

# Create a shapefile of all trees
ptsf <- st_as_sf(alltrees, coords = c('X', 'Y'), crs = '+proj=utm +zone=13 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')

#st_bbox(ptsf)
#plot(st_bbox(ptsf))
st_bbox(bnd)

rs = raster(matrix(1:10000,100,100), xmx=st_bbox(ptsf)[3], xmn=st_bbox(ptsf)[1], ymn=st_bbox(ptsf)[2], ymx=st_bbox(ptsf)[4], crs='+proj=utm +zone=13 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
res(rs)
plot(rs, col=sample(rainbow(10000)))
res(rs) <- 100
res(rs)
ncell(rs)
values(rs) <- 1:ncell(rs)
plot(rs, col=sample(rainbow(459)))

returns <- data.frame(x=alltrees$X, y=alltrees$Y, z=alltrees$Z, d=alltrees$D)
coordinates(returns) <- ~x+y

height.raster = rasterize(returns[,1:2], rs, returns$z, fun=mean)
heightq.raster = rasterize(returns[,1:2], rs, returns$z, fun=function(x, ...)quantile(x, c(.1, .25, .5, .75, .9)))
heightsk.raster = rasterize(returns[,1:2], rs, returns$z, fun=function(x, ...)skewness(x))

diam.raster = rasterize(returns[,1:2], rs, returns$d, fun=mean)
diamq.raster = rasterize(returns[,1:2], rs, returns$d, fun=function(x, ...)quantile(x, c(.1, .25, .5, .75, .9)))

pointcount = function(ras, pts){
  # make a raster of zeroes like the input
  r2 = ras
  r2[] = 0
  
  # make another raster of zeroes like the input
  r3 = ras
  r3[] = 0
  
  # get n returns
  returns = data.frame(x=pts$X, y=pts$Y)
  
  # get peaks
  
  # get the cell index for each point and make a table:
  returns = table(cellFromXY(ras, returns))
  #return(returns)
  
  # fill in the raster with the counts from the cell index:
  #r2[as.numeric(names(peaks))] = peaks
  r3[as.numeric(names(returns))] = returns
  return(r3)
}

density.raster = pointcount(rs, alltrees)
density.raster

dr <- reclassify(density.raster, cbind(-Inf, 0, 1), right=T)

#dr <- peaks/returns/5
#dr <- reclassify(dr, cbind(-Inf, 25, NA), right=T)

res(density.raster)
extent(density.raster)

bnd <- st_read('/global/scratch/users/worsham/EastRiver/RMBL_2020_EastRiver_SDP_Boundary/RMBL_2020_EastRiver_SDP_Boundary/SDP_Boundary.shp')
dr <- mask(density.raster, bnd)
par(mar = c(4, 4, 4, 2) + 0.1)
cls <- c('white', viridis(20))
plot(dr, col=cls)
plot(bnd$geometry, col=NA, border='grey10', axes=T, labels=T, add=T)

runpng <- function(ras, bound, clrs, filename){
  outras = mask(ras, bound)
  png(
    file=file.path('~', 'Output', filename),
    width=1200, 
    height=1200)
  par(mar= c(5,4,4,2)+0.1)
  plot(outras, col=clrs)
  plot(bound$geometry, col=NA, border='grey10', axes=T, labels=T, add=T)
  dev.off()
}

rasters <- c(
  dr, 
  height.raster, 
  heightq.raster[[1]], 
  heightq.raster[[2]],
  heightq.raster[[3]],
  heightq.raster[[4]],
  heightq.raster[[5]],
  heightsk.raster, 
  diam.raster,
  diamq.raster[[5]])

runpng(dr, bnd, c('white', viridis(20)), 'density_100m.png')
runpng(height.raster, bnd, heat.colors(20), 'mean_height_100m.png')
runpng(heightq.raster[[5]], bnd, magma(20), 'height_90pctl_100m.png')
runpng(heightq.raster[[1]], bnd, inferno(20), 'height_10pctl_100m.png')
runpng(heightq.raster[[2]], bnd, inferno(20, direction=-1), 'height_25pctl_100m.png')
runpng(heightq.raster[[3]], bnd, cividis(20), 'height_50pctl_100m.png')
runpng(heightq.raster[[4]], bnd, rocket(20), 'height_75pctl_100m.png')
runpng(heightsk.raster, bnd, plasma(20), 'height_skew_100m.png')
runpng(diam.raster, bnd, heat.colors(20), 'mean_diameter_100m.png')
runpng(diamq.raster[[5]], bnd, magma(20), 'diam_90pctl_100m.png')


writeRaster(dr, '~/stand_density_100mx', format='GTiff', overwrite=T)
writeRaster(height.raster, '~/mean_height_100m', format='GTiff', overwrite=T)
writeRaster(heightq.raster[[1]], '~/height_10pctl', format='GTiff', overwrite=T)
writeRaster(heightq.raster[[2]], '~/height_25pctl', format='GTiff', overwrite=T)
writeRaster(heightq.raster[[3]], '~/height_50pctl', format='GTiff', overwrite=T)
writeRaster(heightq.raster[[4]], '~/height_75pctl', format='GTiff', overwrite=T)
writeRaster(heightq.raster[[5]], '~/height_90pctl', format='GTiff', overwrite=T)
writeRaster(heightsk.raster, '~/height_skew', format='GTiff', overwrite=T)
writeRaster(diam.raster, '~/Output/mean_diam_100m', format='GTiff', overwrite=T)
writeRaster(diamq.raster[[5]], '~/diam_90pctl', format='GTiff', overwrite=T)
