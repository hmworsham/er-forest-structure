## ---------------------------------------------------------------------------------------------------
# Load config
config <- config::get(file=file.path('~',
                                     'Repos',
                                     'er-forest-structure',
                                     'config',
                                     'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

## ---------------------------------------------------------------------------------------------------
# Configure drive auth
drive_auth(path=config$drivesa)

## ---------------------------------------------------------------------------------------------------
# Ingest data

# Ingest trees
#datadir <- config$extdata$trees
datadir <- '/global/scratch/users/worsham/trees_csv'
trfiles <- list.files(datadir, full.names=T)
trees <- mclapply(trfiles, read.csv, mc.cores=getOption('mc.cores', 30))

# Ingest AOP boundary
bnd <- load.plot.sf(path=as_id(config$extdata$bndid),
                    pattern=config$extdata$bndpattern)

## ---------------------------------------------------------------------------------------------------
# Clean trees

# Bind all trees together into one dataframe
alltrees <- data.table::rbindlist(trees, idcol='file')

# Remove trees missing geoinfo
alltrees <- alltrees[!is.na(alltrees$X) & !is.na(alltrees$Y),]

# Remove unlikely trees
alltrees <- alltrees[alltrees$H<=40,]

# Create a shapefile of all trees
ptsf <- st_as_sf(alltrees, coords = c('X', 'Y'), crs = '+proj=utm +zone=13 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')

# Reformat trees to populate raster
returns <- data.frame(x=alltrees$X, y=alltrees$Y, z=alltrees$H, d=alltrees$DBH_est, ba=alltrees$BA_est)
coordinates(returns) <- ~x+y

## ---------------------------------------------------------------------------------------------------
# Initialize the raster frame
reso <- 100
ncells <- 100^2
rs = raster(matrix(1:ncells,reso,reso), xmx=st_bbox(ptsf)[3], xmn=st_bbox(ptsf)[1], ymn=st_bbox(ptsf)[2], ymx=st_bbox(ptsf)[4], crs='+proj=utm +zone=13 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
res(rs) <- 10
ncell(rs)
values(rs) <- 1:ncell(rs)
plot(rs, col=sample(rainbow(ncell(rs))))

## ---------------------------------------------------------------------------------------------------
# Compute rasters

# Height rasters
height.raster = rasterize(returns[,1:2], rs, returns$z, fun=mean)
heightq.raster = rasterize(returns[,1:2], rs, returns$z, fun=function(x, ...)quantile(x, c(.1, .25, .5, .75, .8, .9, .95)))
heightsk.raster = rasterize(returns[,1:2], rs, returns$z, fun=function(x, ...)moments::skewness(x))

# Diameter rasters
qmd.raster = rasterize(returns[,1:2], rs, returns$d, fun=function(x, ...) sqrt(mean(x^2, na.rm=T)))
diamq.raster = rasterize(returns[,1:2], rs, returns$d, fun=function(x, ...)quantile(x, c(.1, .25, .5, .75, .9, .95)))

# Basal area raster
ba.raster <- rasterize(returns[,1:2], rs, returns$ba, fun=function(x, ...) sum(x, na.rm=T)*10^(-4)) # scale from cm^2 to m^2

# Density raster
density.raster = pointcount(rs, alltrees)

## Clean density
density.raster <- reclassify(density.raster, cbind(-Inf, 0, 1), right=T) # Reclassify
#dr <- mask(density.raster, bnd) # Mask
values(density.raster)[values(density.raster)==1] <- NA # Assign 1 to NA

## Plot density
par(mar = c(4, 4, 4, 2) + 0.1)
cls <- c('white', viridis(20))
plot(density.raster, col=cls)
plot(bnd$geometry, col=NA, border='grey10', axes=T, labels=T, add=T)

## ---------------------------------------------------------------------------------------------------
# Compute rasters

rasters <- c(
  'ba_100m'=ba.raster,
  'density_100m'=density.raster,
  'diam_10pctl_100m'=diamq.raster[[1]],
  'diam_25pctl_100m'=diamq.raster[[2]],
  'diam_50pctl_100m'=diamq.raster[[3]],
  'diam_75pctl_100m'=diamq.raster[[4]],
  'diam_90pctl_100m'=diamq.raster[[5]],
  'diam_95pctl_100m'=diamq.raster[[6]],
  'diam_qmd_100m'=qmd.raster,
  'height_mean_100m'=height.raster,
  'height_10pctl_100m'=heightq.raster[[1]],
  'height_25pctl_100m'=heightq.raster[[2]],
  'height_50pctl_100m'=heightq.raster[[3]],
  'height_75pctl_100m'=heightq.raster[[4]],
  'height_80pctl_100m'=heightq.raster[[5]],
  'height_90pctl_100m'=heightq.raster[[6]],
  'height_95pctl_100m'=heightq.raster[[7]],
  'height_skew_100m'=heightsk.raster
  )

pngpal <- list(cividis(20),
              viridis(20),
              heat.colors(20),
              inferno(20),
              inferno(20, direction=-1),
              cividis(20),
              rocket(20),
              magma(20),
              magma(20),
              magma(20),
              plasma(20),
              heat.colors(20),
              magma(20),
              magma(20),
              magma(20),
              magma(20),
              magma(20),
              magma(20))

assertthat::are_equal(length(pngpal), length(rasters))

runpng <- function(ras, bound, clrs, filepath){
  outras = mask(ras, bound)
  png(
    file=filepath,
    width=1200,
    height=1200)
  par(mar=c(5,5,5,5)+0.25)
  plot(outras,
       col=clrs,
       axis.args=list(cex.axis=1.8, line=2.5),
       legend.args=list(text=NULL, font=2, line=2.5, cex=1.2))
  plot(bound$geometry, col=NA, border='grey10', axes=T, labels=T, add=T)
  dev.off()
}

lapply(seq_along(rasters), \(x) {
  runpng(rasters[[x]], bnd, pngpal[[x]], file.path(config$extdata$scratch, 'pngs', paste0(names(rasters)[x], '.png')))
})

lapply(seq_along(rasters), \(x){
  writeRaster(rasters[[x]], file.path(config$extdata$scratch, 'tifs', 'gapfilled', paste0(names(rasters)[x], '.tif')), overwrite=T)
})

