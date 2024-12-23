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
nCores <- as.integer(availableCores()-2)
workerNodes <- rep(workerNodes, nCores)
set_lidr_threads(length(workerNodes)-2)

## ---------------------------------------------------------------------------------------------------
# Specify directories
scrdir <- file.path('/global', 'scratch', 'users', 'worsham')
datadir <- file.path(scrdir, 'las_decimated')
outdir <- file.path(scrdir, 'trees_ls_50m')
dir.create(outdir)

## ---------------------------------------------------------------------------------------------------
# Ingest full LAS catalog of decimated points
infiles <- list.files(datadir, full.names=T)
lascat <- readLAScatalog(infiles)
plot(lascat['Number.of.point.records'], lwd=0.1)

## ---------------------------------------------------------------------------------------------------
# Set las catalog processing options
opt_stop_early(lascat) <- F # Proceed through errors, leaving gaps for failed chunks
lascat@output_options$drivers$sf$param$append <- F # Don't append when writing sf to .shp
lascat@output_options$drivers$sf$param$delete_dsn <- T # Overwrite existing when writing sf to .shp

## ---------------------------------------------------------------------------------------------------
# Specify optimal LayerStacking parameters
start = 0.5
res = 0.5
ws1 = 2
ws2 = 2
buf = 0.2
hmin = 1.8

## ---------------------------------------------------------------------------------------------------
# Specify output location
opt_output_files(lascat) <- file.path(outdir, 'trees_{XLEFT}_{YBOTTOM}')
opt_chunk_size(lascat) <- 50
opt_chunk_buffer(lascat) <- 10

# Detect trees using optimal algorithm and parameters
algo <- LayerStacking(start=0.5, res=0.5, ws1=2, ws2=2, buf_size=0.2, hardwood=F, hmin=1.8)

# set5 <- split(1:nrow(lascat), ceiling(1:nrow(lascat)/5))

plan(multisession, workers=nCores)
ls.trees <- find_trees(lascat, algo, uniqueness='bitmerge')
