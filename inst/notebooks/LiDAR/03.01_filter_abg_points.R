# Filter points to aboveground
# Author: Marshall Worsham | worsham@berkeley.edu
# Created: 04-23-23
# Revised: 07-23-24

#############################
# Set up working environment
#############################

# Set RGL option
options(rgl.useNULL=TRUE)

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

# Define directories
shapedir <- file.path(config$extdata$scratch, 'EastRiverInputs', 'RMBL_2020_EastRiver_SDP_Boundary')
datadir <- file.path(config$extdata$scratch, 'las_normalized')
outdir <- file.path(config$extdata$scratch, 'las_abg')
dir.create(outdir)

############################
# Prep data and visualize
############################

# Ingest gridded points
infiles <- list.files(datadir, full.names=T)

# Read as catalog
lascat <- readLAScatalog(infiles)

# Check catalog
las_check(lascat)

# Plot n returns
lascat <- lascat[lascat['Number.of.point.records']$Number.of.point.records>0,]
st_crs(lascat) <- '+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs'
plot(lascat['Number.of.point.records'], main='N points per grid cell')

# View a sample las file
las <- readLAS(infiles[1080])
plot(las[1:500000], bg='white')
rglwidget()
hist(las$Z)

######################################
# Filter to aboveground (Z>0) points
######################################

# Set chunk buffer and other catalog processing params
opt_chunk_buffer(lascat) <- 0
opt_output_files(lascat) <- file.path(outdir, 'las_decimated_{XLEFT}_{YBOTTOM}')
opt_laz_compression(lascat) <- T

# Filter below-ground returns
aboveground <- function(chunk){
  x = readLAS(chunk)
  if (is.empty(x)) return(NULL)        # check if it actually contain points
  abg = tryCatch({x[x$Z>0]},
           error=function(cond){message('Normalization error')
             return(NULL)})
  if (is.empty(abg)) return(NULL)
  return(abg) # output
}

plan(multisession, workers=30L, gc=T)
test1 <- catalog_apply(lascat, aboveground)
