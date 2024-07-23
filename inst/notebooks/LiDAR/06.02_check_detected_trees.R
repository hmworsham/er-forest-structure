# Check detected trees over full watershed
# Author: Marshall Worsham | worsham@berkeley.edu
# Created: 02-28-24
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
workerNodes <- str_split(system('squeue -u $USER -o "%N"', intern=T)[[2]], ',', simplify=T)
workerNodes <- rep(workerNodes, availableCores())
nCores <- as.integer(availableCores()-2)
set_lidr_threads(length(workerNodes)-2)

# Define directories
datadir <- file.path(config$extdata$scratch, 'trees_ls_100m')

#############################
# Data ingest
#############################

# Ingest full LAS catalog of decimated points
infiles <- list.files(config$extdata$las_dec, full.names=T)
lascat <- readLAScatalog(infiles)
lascat@output_options$drivers$sf$param$append <- F
lascat@output_options$drivers$sf$param$delete_dsn <- T

#############################
# Check completion
#############################

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

