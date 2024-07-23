# Mask predicted trees
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

# Ingest AOP boundary
bnd <- load.plot.sf(path=as_id(config$extdata$bndid),
                    pattern=config$extdata$bndpattern)

# Ingest trees
treefiles <- list.files(config$extdata$scratch, 'trees_ls_100m_csv', pattern='.csv', full.names=T)
trees <- mclapply(treefiles, read.csv, mc.cores=getOption('mc.cores', nCores))
alltrees <- data.table::rbindlist(trees, idcol='file')

# Ingest landcover dataset
tmpfile <- drive_download(
  config$extdata$lcpattern,
  path=file.path(tempdir(), config$extdata$invid),
  overwrite=T)$local_path

lc <- rast(tmpfile)

# Ingest Nicola's classification map
tmpfile <- drive_download(
  as_id(config$extdata$falcoid),
  path=file.path(tempdir(), config$extdata$falcoid),
  overwrite = T)$local_path

sp.class <- rast(tmpfile)
sp.class <- as.numeric(sp.class)
names(sp.class) <- 'Sp_Code'

# Ingest Nicola's species classification codes
tmpfile <- drive_download(
  as_id(config$extdata$classid),
  path=file.path(tempdir(), config$extdata$classid),
  overwrite=T)$local_path

sp.codes <- read.csv(tmpfile)
sp.codes <- sp.codes %>%
  mutate(Pixel_Code=as.numeric(Pixel_Code)) %>%
  dplyr::select(-c(Genus:Common))

# Ingest NAIP base image
tmpfile <- drive_download(
  as_id(config$extdata$naipid),
  path=file.path(tempdir(), config$extdata$naipid),
  overwrite=T)$local_path

naip <- rast(tmpfile)

# Ingest boundary of missing flightpath
missfp <-st_read(file.path(config$extdata$scratch, 'missing_flightpath', 'missing_flightpath.shp'))

# Ingest developed zones
devzones <- load.plot.sf(path=as_id(config$extdata$devid),
                            pattern=config$extdata$devpattern)
# Ingest roads
roads <- load.plot.sf(path=as_id(config$extdata$roadsid),
                         pattern=config$extdata$roadspattern)

# Ingest template forest structure raster to resample conifers to
template <- rast(file.path(config$extdata$scratch, 'tifs', 'ls', 'density_100m.tif'))

##########################################################
# Create conifer mask from Breckheimer classification data
##########################################################

# Set all non-conifer classes in landcover data to NA
# needle-leaf trees = 1
lc[!lc==1] <- NA

# Smooth with 9-pixel window
#conif <- terra::focal(lc, w=3, fun='mean', na.policy='only', na.rm=T)
conif <- terra::focal(lc, w=9, fun='mean', na.policy='only')
plot(conif, col=c('red', NULL))
conif100 <- resample(conif, template)
plot(conif100, col=c('red', NULL))

# Remove non-contiguous clumps
conifpatches <- patches(conif100, directions = 8)
patch.freq <- freq(conifpatches)

# Which rows of the data.frame are only represented by 5 or fewer pixels?
str(which(patch.freq$count <= 5))

# Which values do these correspond to?
str(patch.freq$value[which(patch.freq$count <= 5)])

# Put these into a vector of patch ID's to be removed
excludeID <- patch.freq$value[which(patch.freq$count <= 5)]

# Make a new forest mask to be sieved
conif.sieve <- conif100

# Assign NA to all patches whose IDs are found in excludeID
conif.sieve[conifpatches %in% excludeID] <- NA

# Check conifer sieve and write
plot(conif100, col=c('red', NA))
plot(conif.sieve, col=c('blue', NA), add=T)

writeRaster(conif.sieve, file.path(config$extdata$scratch, 'tifs', 'conifers_100m.tif'), overwrite=T)

##########################################################
# Create conifer mask from Falco classification data
##########################################################

# Set all non-conifer classes in Falco data to NA
sp.class[!sp.class %in% c(45:47)] <- NA
sp.class[sp.class %in% c(45:47)] <- 1
plot(sp.class, col=c('red', NULL))

# Smooth with 7-pixel window
nf.conif <- terra::focal(sp.class, w=7, fun='mean', na.policy='only', na.rm=T)
#nf.conif100 <- resample(nf.conif, template)
nf.conif5 <- aggregate(sp.class, 3, fun='mean', na.rm=T)
plot(nf.conif, col=c('blue', NULL))
plot(sp.class, col=c('red', NULL), add=T)
plot(nf.conif10, col=c('yellow', NULL), add=T)

# Remove non-contiguous clumps
nf.conifpatches <- patches(nf.conif5, directions=8)

nf.patch.freq <- freq(nf.conifpatches)

# Which rows of the dataframe are represented by 5 or fewer pixels?
str(which(nf.patch.freq$count<=30))

# Which values do these indices correspond to?
str(nf.patch.freq$value[which(nf.patch.freq$count<=30)])

# Put these into a vector of patch ID's to be removed
nf.excludeID <- nf.patch.freq$value[which(nf.patch.freq$count <= 30)]

# Make a new forest mask for sieving
nf.conif.sieve <- nf.conif5

# Assign NA to all patches whose IDs are found in excludeID
nf.conif.sieve[nf.conifpatches %in% nf.excludeID] <- NA

# Check conifer sieve
plot(nf.conif5, col=c('red', NA))
plot(nf.conif.sieve, col=c('blue', NA), add=T)

# Compare Falco sieve to Breckheimer sieve
plot(nf.conif.sieve, col=c('red', NA))
plot(conif.sieve, col=c('blue', NA), alpha=0.4, add=T)

# Write Falco conif sieve
writeRaster(nf.conif.sieve, file.path(config$extdata$scratch, 'tifs', 'nf_conifers_5m.tif'), overwrite=T)

##########################################################
## Overlay masks on NAIP imagery
##########################################################

# Read in saved sieves
nf.conif.sieve <- rast(file.path(config$extdata$scratch, 'tifs', 'nf_conifers_100m.tif'))
ib.conif.sieve <- rast(file.path(config$extdata$scratch, 'tifs', 'conifers_100m.tif'))

plot(naip)
plot(nf.conif.sieve, col=c('turquoise', NA), alpha=0.4, add=T)

plot(naip)
plot(ib.conif.sieve, col=c('red', NA), alpha=0.5, add=T)

# Pull basemap from google
er.bmap <- get_map(location=c(lon = -106.995, lat = 38.900),
                    zoom=12,
                    maptype = 'satellite',
                    source = 'google')

ib.conif.sieve.fact <- terra::as.factor(project(ib.conif.sieve, 'EPSG:4326'))
nf.conif.sieve.fact <- terra::as.factor(project(nf.conif.sieve, 'EPSG:4326'))

ggmap(er.bmap) +
  tidyterra::geom_spatraster(data=nf.conif.sieve.fact, aes(fill=label), alpha=0.2) +
  scale_fill_gradientn(colors='turquoise', na.value=NA)

##########################################################
# Add density and other masks
##########################################################

# Read in saved sieves
nf.conif.sieve <- rast(file.path(config$extdata$scratch, 'tifs', 'nf_conifers_5m.tif'))

# Mask out any values where density < 500 stems ha
density.mask <- disagg(template, 20)
density.mask[density.mask<100] <- NA
density.mask[density.mask>=100] <- 1

plot(density.mask, col=c('red', NA))

# Make a new forest mask for further masking with density
conif.density.mask <- alignfun(nf.conif.sieve, density.mask)

# Assign NA to all pixels with forest density <= 500 stems / ha
conif.density.mask[is.na(density.mask)] <- NA

plot(naip)
plot(conif.density.mask, col='red', add=T)
plot(mask(template, conif.density.mask))

# Buffer AOP boundary inward 100m
bnd100 <- st_buffer(bnd, -100)

# Buffer missing flightpath outward 100m
missfp100 <- st_buffer(missfp, 100)

# Plot to check
plot(naip)
plot(mask(template, conif.density.mask), add=T)
plot(bnd100, col=NA, add=T)
plot(missfp100, col=NA, add=T)

# Add boundary masks
conif.density.bnd.mask <- mask(conif.density.mask, bnd100)
conif.density.bnd.mfp.mask <- mask(conif.density.bnd.mask, missfp100, inverse=T)

# Add developed area mask
conif.density.bnd.mfp.dev.mask <- mask(conif.density.bnd.mfp.mask, devzones, inverse=T)

# Add road buffer mask
conif.density.bnd.mfp.dev.rd.mask <- mask(conif.density.bnd.mfp.dev.mask, roads, inverse=T)

plot(conif.density.bnd.mfp.dev.rd.mask, col='grey', add=T)

# Write out final composite mask
writeRaster(conif.density.bnd.mfp.dev.rd.mask,
            file.path(config$extdata$scratch, 'tifs', 'fullmask_5m.tif'),
            overwrite=T)

##########################################################
## Mask trees and write out
##########################################################

# Read full mask
full.mask <- rast(file.path(config$extdata$scratch, 'tifs', 'fullmask_5m.tif'))
full.mask.poly <- st_as_sf(as.polygons(full.mask))
full.mask.poly <- st_cast(full.mask.poly, 'POLYGON')
full.mask.area <- as.numeric(st_area(full.mask.poly))
full.mask.poly <- full.mask.poly[full.mask.area>=1000,]

# Subset detected trees to unmasked zones
alltrees.df <- data.frame(alltrees)
trees_geos <- geos::geos_read_xy(alltrees.df[c("X", "Y")])
full.mask.geos <- geos::as_geos_geometry(full.mask.poly)
wk::wk_crs(full.mask.geos) <- NULL
trees_tree <- geos::geos_strtree(trees_geos)
keys <- geos::geos_contains_matrix(full.mask.geos, trees_tree)
trees_conif <- alltrees.df[unlist(keys),]
trees_conif <- trees_conif %>%
  dplyr::select(-c(file))

# Write masked trees as csv
data.table::fwrite(trees_conif, file.path(config$extdata$scratch, 'trees_masked_5m.csv'), append=F)
