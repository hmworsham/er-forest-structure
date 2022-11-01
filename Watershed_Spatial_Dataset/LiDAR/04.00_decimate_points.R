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
datadir <- file.path(scrdir, 'las_regridded')
neondir <- file.path(scrdir, 'neon_las_regridded')
outdir <- file.path(scrdir, 'las_resampled2')
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
summary(lascat)
plot(lascat['Number.of.point.records'], main='N points per grid cell')

# View a sample las file
# las <- readLAS(infiles[1492])
# plot(las[1:500000], bg='white')
# rglwidget()
# hist(las$Z)

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

plan(multisession)
test1 <- catalog_apply(lascat, aboveground)

#############################################################
# Downsample (decimate) points with homogenization function
#############################################################

# Check las density (should = 9.4 pts/m2)
summary(lascat)

# Check neon density (should = 5.1 pts/m2)
neonlas <- readLAScatalog(list.files(neondir, full.names=T))
st_crs(neonlas) <- '+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs'
neonlas <- neonlas[neonlas['Number.of.point.records']$Number.of.point.records>0,]
summary(neonlas)
plot(neonlas['Number.of.point.records'], main='N points per grid cell')

# lascat.sub <- readLAScatalog(infiles[150:175])
# summary(lascat.sub)
# lascat.sub <- lascat.sub[lascat.sub['Number.of.point.records']$Number.of.point.records>0,]
# plot(lascat.sub['Number.of.point.records', main='N points per grid cell'])

# Test decimation
opt_output_files(lascat) <- file.path(outdir, 'las_resampled_{XLEFT}_{YBOTTOM}')
opt_chunk_buffer(lascat) <- 0
opt_laz_compression(lascat) <- T

# Decimate in parallel
plan(multisession, workers = 30L)
dp <- decimate_points(lascat, homogenize(48, 1))

# check decimation results
dp <- dp[dp['Number.of.point.records']$Number.of.point.records>0,]
summary(dp)
plot(lascat['Number.of.point.records'], main='N points per grid cell')



las <- readLAS(infiles[9])
plot(las[1:50000])
hist(las$Z)
rglwidget()

# Select points randomly to reach an overall density of 1
thinned1 <- decimate_points(las, random(1))
plot(rasterize_density(las))
plot(rasterize_density(thinned1))

plot(las, bg='white', legend=T)
rglwidget()

plot(thinned1, bg='white', legend=T)
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