# Downsample (decimate) points to 
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

# Runs the function on the list of packages defined in pkgs
load.pkgs(pkgs)

# Setup workspace
scrdir <- file.path('/global', 'scratch', 'users', 'worsham')
shapedir <- file.path(scrdir, 'EastRiverInputs/RMBL_2020_EastRiver_SDP_Boundary')
datadir <- file.path(scrdir, 'las_normalized')
neondir <- file.path(scrdir, 'neon_las_regridded')
outdir <- file.path(scrdir, 'las_decimated')
dir.create(outdir)

############################
# Prep data and visualize
############################

# Ingest gridded points
infiles <- list.files(datadir, full.names=T)[40:1577]

# Read as catalog
lascat <- readLAScatalog(infiles)

# Check catalog
las_check(lascat)

# Plot n returns
lascat <- lascat[lascat['Number.of.point.records']$Number.of.point.records>0,]
plot(lascat['Number.of.point.records'], main='N points per grid cell')

#############
# Downsample
#############

# Set chunk buffer and other catalog processing params
opt_chunk_buffer(lascat) <- 5
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

plan(multisession)
test1 <- catalog_apply(lascat, aboveground)

# Downsample (decimate) points with homogenization function
dp <- decimate_points(lascat, homogenize(16, 10))

las <- readLAS(infiles[9])
plot(las[1:50000])
hist(las$Z)
rglwidget()

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las <- readLAS(LASfile, select = "xyz")
plot(las)
rglwidget()
# Select points randomly to reach an overall density of 1
thinned1 <- decimate_points(las, random(1))
#plot(rasterize_density(las))
plot(rasterize_density(thinned1))
rglwidget()

# Select points randomly to reach an homogeneous density of 1
thinned2 <- decimate_points(las, homogenize(1,5))
#plot(rasterize_density(thinned2))
?homogenize
# Select the highest point within each pixel of an overlayed grid
thinned3 = decimate_points(las, highest(5))
#plot(thinned3)

#################
# Check results
################
lascat1 <- readLAScatalog(infiles)
lascat2 <- readLAScatalog(list.files(outdir, full.names=T)