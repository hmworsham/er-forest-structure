# Make rasters of gridded structure metrics from detected trees
# Author: Marshall Worsham | worsham@berkeley.edu
# Created: 03-21-24
# Revised: 07-23-24

#############################
# Set up working environment
#############################

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

# Configure drive auth
drive_auth(path=config$drivesa)

# Define parallel scope
nCores <- as.integer(availableCores()-2)

#############################
# Data ingest
#############################

# Ingest trees
alltrees <- read_csv(file.path(config$extdata$scratch, 'trees_masked_5m.csv'))
trees.spp <- read_csv(file.path(config$extdata$scratch, 'trees_species_masked_5m.csv'))

# Ingest AOP boundary
bnd <- load.plot.sf(path=as_id(config$extdata$bndid),
                    pattern=config$extdata$bndpattern)


#############################
# Data cleaning
#############################

# Remove trees missing geoinfo
alltrees <- alltrees[!is.na(alltrees$X) & !is.na(alltrees$Y),]

# Remove unlikely trees
alltrees <- alltrees[alltrees$H<=60,]

# Create a shapefile of all trees
ptsf <- st_as_sf(alltrees,
                 coords = c('X', 'Y'),
                 crs = 'EPSG:32613')

# Reformat trees to populate raster
returns <- data.frame(x=alltrees$X, y=alltrees$Y, z=alltrees$H, d=alltrees$DBH_est, ba=alltrees$BA_est)
coordinates(returns) <- ~x+y

# Clean trees_spp

# Remove trees missing geoinfo
trees.spp <- trees.spp[!is.na(trees.spp$X) & !is.na(trees.spp$Y),]

# Remove unlikely trees
trees.spp <- trees.spp[trees.spp$Z<=60,]

#############################
# Set up raster frame
#############################

# Initialize the raster frame
reso <- 100
ncells <- 100^2
rs = raster(matrix(1:ncells,reso,reso), xmx=st_bbox(ptsf)[3], xmn=st_bbox(ptsf)[1], ymn=st_bbox(ptsf)[2], ymx=st_bbox(ptsf)[4], crs='+proj=utm +zone=13 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
res(rs) <- 100
ncell(rs)
values(rs) <- 1:ncell(rs)

#############################
# Compute rasters
#############################

# Height rasters
height.raster = rasterize(returns[,1:2], rs, returns$z, fun=function(x, ...) mean(x, na.rm=T))
heightq.raster = rasterize(returns[,1:2], rs, returns$z, fun=function(x, ...)quantile(x, c(.1, .25, .5, .75, .8, .9, .95), na.rm=T))
heightsk.raster = rasterize(returns[,1:2], rs, returns$z, fun=function(x, ...)moments::skewness(x, na.rm=T))

# Diameter rasters
qmd.raster = rasterize(returns[,1:2], rs, returns$d, fun=function(x, ...) sqrt(mean(x^2, na.rm=T)))
diamq.raster = rasterize(returns[,1:2], rs, returns$d, fun=function(x, ...)quantile(x, c(.1, .25, .5, .75, .9, .95), na.rm=T))

# Basal area raster
ba.raster <- rasterize(returns[,1:2], rs, returns$ba, fun=function(x, ...) sum(x, na.rm=T)*10^(-4)) # scale from cm^2 to m^2

# Function to generate and clean up density raster
make.density <- function(ras, pts) {
  density.raster = pointcount(ras, pts)
  density.raster = reclassify(density.raster, cbind(-Inf, 0, 1), right=T)
  values(density.raster)[values(density.raster)==1] <- NA
  return(density.raster)
}

# Density raster
density.raster = make.density(rs, alltrees)

## Species rasters
trees.abla <- trees.spp %>% filter(Sp_Code=='ABLA')
trees.pien <- trees.spp %>% filter(Sp_Code=='PIEN')
trees.pico <- trees.spp %>% filter(Sp_Code=='PICO')

density.abla.raster <- make.density(rs, trees.abla)
density.pien.raster <- make.density(rs, trees.pien)
density.pico.raster <- make.density(rs, trees.pico)

## Plot density
par(mar = c(4, 4, 4, 2) + 0.1)
cls <- c('white', mako(20))
plot(density.pien.raster, col=cls)
plot(bnd$geometry, col=NA, border='grey10', axes=T, labels=T, add=T)

#############################
# Collate rasters
#############################

# Assemble as list
rasters <- c(
  'ba_100m'=ba.raster,
  'density_100m'=density.raster,
  'density_abla_100m'=density.abla.raster,
  'density_pien_100m'=density.pien.raster,
  'density_pico_100m'=density.pico.raster,
  'diam_10pctl_100m'=diamq.raster[[1]],
  'diam_25pctl_100m'=diamq.raster[[2]],
  'diam_50pctl_100m'=diamq.raster[[3]],
  'diam_75pctl_100m'=diamq.raster[[4]],
  'diam_90pctl_100m'=diamq.raster[[5]],
  'diam_95pctl_100m'=diamq.raster[[6]],
  'diam_qmd_100m'=qmd.raster,
  'height_10pctl_100m'=heightq.raster[[1]],
  'height_25pctl_100m'=heightq.raster[[2]],
  'height_50pctl_100m'=heightq.raster[[3]],
  'height_75pctl_100m'=heightq.raster[[4]],
  'height_80pctl_100m'=heightq.raster[[5]],
  'height_90pctl_100m'=heightq.raster[[6]],
  'height_95pctl_100m'=heightq.raster[[7]],
  'height_mean_100m'=height.raster,
  'height_skew_100m'=heightsk.raster
)

################
# Mask rasters
################
# Mask rasters to density >=100 stems / ha

# Across rasters, assign NA to all pixels with forest density <= 100 stems / ha
density.mask <- density.raster
density.mask[density.mask<100] <- NA
density.mask[density.mask>=100] <- 1

rasters <- lapply(rasters, \(x) {
  y <- mask(x, density.mask)
  y
})

#############################
# Write
#############################

pngpal <- list(
  cividis(20),
  viridis(20),
  mako(20),
  mako(20),
  mako(20),
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
  magma(20)
)

assertthat::are_equal(length(pngpal), length(rasters))

# Write PNG
lapply(seq_along(rasters), \(x) {
  runpng(rasters[[x]],
         bnd,
         pngpal[[x]],
         file.path(config$extdata$scratch, 'pngs', 'ls_masked', paste0(names(rasters)[x], '_masked.png')))
})

# Write GEOTiff
lapply(seq_along(rasters), \(x){
  writeRaster(rasters[[x]],
              file.path(config$extdata$scratch, 'tifs', 'ls_masked',
                        paste0(names(rasters)[x], '_masked.tif')),
              overwrite=T)
})

