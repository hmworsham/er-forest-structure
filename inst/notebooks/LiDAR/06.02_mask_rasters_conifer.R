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

# Ingest template forest structure raster to resample conifers to
template <- rast('/global/scratch/users/worsham/tifs/gapfilled/ba_100m.tif')

# Ingest all rasters
rasters <- lapply(list.files(file.path(config$extdata$scratch, 'tifs', 'gapfilled'), full.names=T), rast)

## ---------------------------------------------------------------------------------------------------
# Create conifer mask

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

# Which rows of the data.frame are only represented by 3 or fewer pixels?
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

## ---------------------------------------------------------------------------------------------------
# Apply masks

# Mask rasters and write out
conif.mask <- rast(file.path(config$extdata$scratch, 'tifs', 'conifers_100m.tif'))
rast.mask <- lapply(rasters, mask, conif.mask)

# Mask out any values where density < 100 stems ha
density.mask <- rast.mask[[2]]
density.mask[density.mask<100] <- NA
density.mask[density.mask>=100] <- 1

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
