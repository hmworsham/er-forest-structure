# Detect trees on full watershed using optimized algorithm and parameter set

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
drive_auth(path=config$drivesa)

## ---------------------------------------------------------------------------------------------------
workerNodes <- str_split(system('squeue -u $USER -o "%N"', intern=T)[[2]], ',', simplify=T)
workerNodes <- rep(workerNodes, 32)
set_lidr_threads(length(workerNodes)-2)

## ---------------------------------------------------------------------------------------------------
# Specify directories
scrdir <- file.path('/global', 'scratch', 'users', 'worsham')
datadir <- file.path(scrdir, 'trees_ls_100m')

## ---------------------------------------------------------------------------------------------------
# Ingest full LAS catalog of decimated points
infiles <- list.files(config$extdata$las_dec, full.names=T)
lascat <- readLAScatalog(infiles)
lascat@output_options$drivers$sf$param$append <- F
lascat@output_options$drivers$sf$param$delete_dsn <- T

## ---------------------------------------------------------------------------------------------------
# Check completion on LS run on 100m grid
trs.ls.fn <- list.files(datadir, pattern='shp', full.names=T)

trs.ls <- mclapply(trs.ls.fn, FUN=st_read, mc.cores=getOption("mc.cores", 16))

trs.grid100 <- do.call(rbind, lapply(trs.ls, FUN=function(x) {
  bb <- st_bbox(x)
  if(!is.na(sum(bb))) {
    bsf <- st_as_sfc(bb)
  }
}))

ntrs <- unlist(lapply(trs.ls, nrow))
ntrs <- ntrs[ntrs>0]

npercell <- data.frame('n'=ntrs, 'geometry'=trs.grid100)

npercell.sf <- st_as_sf(npercell)
npercell.rast <- rast(st_rasterize(npercell.sf))

plot(npercell.sf)

# Mask
conif.mask <- rast(file.path(config$extdata$scratch, 'tifs', 'conifers_100m.tif'))
npercell.rast <- resample(npercell.rast, conif.mask, 'bilinear')
npercell.rast <- crop(npercell.rast, ext(conif.mask))
ext(conif.mask) <- ext(npercell.rast)
npercell.rast
conif.mask

npercell.conif <- terra::mask(npercell.rast, conif.mask)
plot(npercell.conif)
plot(npercell.sf)
