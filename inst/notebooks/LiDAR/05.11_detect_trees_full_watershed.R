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
datadir <- file.path(scrdir, 'las_decimated')
outdir <- file.path(scrdir, 'trees')
dir.create(outdir)

## ---------------------------------------------------------------------------------------------------
# Ingest full LAS catalog of decimated points
infiles <- list.files(config$extdata$las_dec, full.names=T)
lascat <- readLAScatalog(infiles)

## ---------------------------------------------------------------------------------------------------
# Specify optimal LayerStacking parameters
start = 0.5
res = 0.5
ws1 = 2
ws2 = 2
buf = 0.2
hmin = 1.3

# Specify optimal lmf-fw parameters
# ws=2.4
# shape='circular'

## ---------------------------------------------------------------------------------------------------
# Specify output location
opt_output_files(lascat) <- file.path(outdir, 'trees_{XLEFT}_{YBOTTOM}')
opt_chunk_size(lascat) <- 500
opt_chunk_buffer(lascat) <- 25

# Detect trees using optimal algorithm and parameters
plan(multisession, workers=24L)

#ls.init(lascat[60,], start=0.5, res=0.5, ws1=2, ws2=2, buf_size=0.2, hmin=1.3, hardwood=F)
algo <- LayerStacking(start=0.5, res=0.5, ws1=2, ws2=2, buf_size=0.2, hardwood=F, hmin=1.3)
#algo2 <- lmf(ws=ws, shape=shape, hmin=1.3)
ls.trees <- find_trees(lascat[1:110,], algo, uniqueness='incremental')
