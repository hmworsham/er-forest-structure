# Downsample (decimate) points to uniform density
# Load libraries
options(rgl.useNULL=TRUE)

# Install and load libraries
pkgs <- c('future',
          'lidR',
          'raster',
          'rgl',
          'terra') # Name the packages you want to use here

# Function to install new packages if they're not already installed
load.pkgs <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 

# Runs load.pkgs on the list of packages defined in pkgs
load.pkgs(pkgs)

# Setup workspace
scrdir <- file.path('/global', 'scratch', 'users', 'worsham')
shapedir <- file.path(scrdir, 'EastRiverInputs/RMBL_2020_EastRiver_SDP_Boundary')
datadir <- file.path(scrdir, 'las_normalized')
outdir <- file.path(scrdir, 'las_abg')
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