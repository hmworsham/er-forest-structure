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
# Configure Drive auth
drive_auth(path=config$drivesa)

## ---------------------------------------------------------------------------------------------------
# Ingest data

# Ingest AOP boundary
bnd <- load.plot.sf(path=as_id(config$extdata$bndid),
                    pattern=config$extdata$bndpattern)

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

# Ingest template forest structure raster to resample conifers to
template <- rast('/global/scratch/users/worsham/tifs/gapfilled/ba_100m.tif')

# Ingest all rasters
# TURN BACK ON AFTER TESTING ---------------> rasters <- lapply(list.files(file.path(config$extdata$scratch, 'tifs', 'gapfilled'), full.names=T), rast)

## ---------------------------------------------------------------------------------------------------
# Create conifer mask from Breckheimer classification data

# Set all non-conifer classes in landcover data to NA
# needle-leaf trees = 1
lc[!lc==1] <- NA

# Smooth with 9-pixel window
#conif <- terra::focal(lc, w=3, fun='mean', na.policy='only', na.rm=T)
conif <- terra::focal(lc, w=9, fun='mean', na.policy='only', na.rm=T)
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

conif.sieve.ib <- rast(file.path(config$extdata$scratch, 'tifs', 'conifers_100m.tif'))

## ---------------------------------------------------------------------------------------------------
# Create conifer mask from Falco classification data

# Reclassify Falco data using species codes -- DON'T THINK THIS IS NECESSARY...??

# Set all non-conifer classes in Falco data to NA
sp.class[!sp.class %in% c(45:47, 101:104, 105:109)] <- NA
sp.class[sp.class %in% c(45:47, 101:104, 105:109)] <- 1

# Smooth with 9-pixel window
nf.conif <- terra::focal(sp.class, w=9, fun='mean', na.policy='only', na.rm=T)
nf.conif100 <- resample(nf.conif, template)
plot(nf.conif100, col=c('red', NULL))

# Remove non-contiguous clumps
nf.conifpatches <- patches(nf.conif100, directions=8)
nf.patch.freq <- freq(nf.conifpatches)

# Which rows of the dataframe are represented by 5 or fewer pixels?
str(which(nf.patch.freq$count<=5))

# Which values do these indices correspond to?
str(nf.patch.freq$value[which(nf.patch.freq$count<=5)])

# Put these into a vector of patch ID's to be removed
nf.excludeID <- nf.patch.freq$value[which(nf.patch.freq$count <= 5)]

# Make a new forest mask for sieving
nf.conif.sieve <- nf.conif100

# Assign NA to all patches whose IDs are found in excludeID
nf.conif.sieve[nf.conifpatches %in% nf.excludeID] <- NA

# Check conifer sieve
plot(nf.conif100, col=c('red', NA))
plot(nf.conif.sieve, col=c('blue', NA), add=T)

# Compare Falco sieve to Breckheimer sieve
plot(nf.conif.sieve, col=c('red', NA))
plot(conif.sieve, col=c('blue', NA), alpha=0.4, add=T)

# Write Falco conif sieve
writeRaster(nf.conif.sieve, file.path(config$extdata$scratch, 'tifs', 'nf_conifers_100m.tif'), overwrite=T)

## ---------------------------------------------------------------------------------------------------
# Overlay masks on NAIP imagery

plot(naip)
plot(nf.conif.sieve, col=c('turquoise', NA), alpha=0.4, add=T)

plot(naip)
plot(conif.sieve, col=c('red', NA), alpha=0.5, add=T)

# Pull basemap from google
er.bmap <- get_map(location=c(lon = -106.995, lat = 38.900),
                    zoom=11,
                    maptype = 'satellite',
                    source = 'google')

conif.sieve.fact <- as.factor(project(conif.sieve, 'EPSG:4326'))
nf.conif.sieve.fact <- as.factor(project(nf.conif.sieve, 'EPSG:4326'))

ggmap(er.bmap) +
  #tidyterra::geom_spatraster(data=conif.sieve.fact, aes(fill=label), alpha=0.5) +
  tidyterra::geom_spatraster(data=nf.conif.sieve.fact, aes(fill=label), alpha=0.5) +
  scale_fill_continuous(na.value=NA)

## ---------------------------------------------------------------------------------------------------
# Apply masks

# Mask out any values where density < 100 stems ha
density.mask <- rasters[[2]]
density.mask[density.mask<500] <- NA
density.mask[density.mask>=500] <- 1

plot(density.mask, col=c('red', NA))

# Make a new forest mask for sieving
nf.conif.sieve.dm <- nf.conif.sieve

# Assign NA to all patches whose IDs are found in excludeID
nf.conif.sieve.dm[is.na(density.mask)] <- NA

# Mask rasters and write out
conif.mask <- rast(file.path(config$extdata$scratch, 'tifs', 'conifers_100m.tif'))
rast.mask <- lapply(rasters, mask, conif.mask)

rast.mask <- lapply(rasters, mask, density.mask)
plot(rast.mask[[6]])

## ---------------------------------------------------------------------------------------------------
# Write

## Assign names from input basenames
names(rast.mask) <- tools::file_path_sans_ext(list.files(file.path(config$extdata$scratch, 'tifs', 'gapfilled')))

## Write out
lapply(seq_along(rast.mask), \(x){
  writeRaster(rast.mask[[x]], file.path(config$extdata$scratch, 'tifs', 'conifer_masked', paste0(names(rast.mask)[x], '_conifmask.tif')), overwrite=T)
})
