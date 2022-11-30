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
datadir <- file.path(scrdir, 'las_abg')
neondir <- file.path(scrdir, 'neon_las_regridded')
outdir <- file.path(scrdir, 'las_decimated')
dir.create(outdir)

############################
# Prep data and visualize
############################

# Ingest gridded points
infiles <- list.files(datadir, full.names=T)

# Read as catalog
lascat <- readLAScatalog(infiles)

# Check catalog
summary(lascat)
las_check(lascat)

# Plot n returns
#lascat <- lascat[lascat['Number.of.point.records']$Number.of.point.records>0,]
st_crs(lascat) <- '+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs'
plot(lascat['Number.of.point.records'], main='N points per grid cell')

# View a sample las file
las <- readLAS(infiles[1080])
plot(las[1:500000], bg='white')
rglwidget()
hist(las$Z)

#############################################################
# Downsample (decimate) points with homogenization function
#############################################################

# Check las density (should 7.8 pts/m2)
summary(lascat)

# Check neon density (should = 5.1 pts/m2)
neonlas <- readLAScatalog(list.files(neondir, full.names=T))
st_crs(neonlas) <- '+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs'
neonlas <- neonlas[neonlas['Number.of.point.records']$Number.of.point.records>0,]
summary(neonlas)

# Plot NEON density
plot(neonlas['Number.of.point.records'], main='N points per grid cell')

# Set chunk buffer and other catalog processing params
opt_output_files(lascat) <- file.path(outdir, 'las_decimated_{XLEFT}_{YBOTTOM}')
opt_chunk_buffer(lascat) <- 0
opt_laz_compression(lascat) <- T

# Plot catalog showing buffered chunks to process
plot(lascat, chunk=T)

# Decimate in parallel
plan(multisession, workers = 18L)
#lascat.tst <- lascat[1550:1560,]
dp <- decimate_points(lascat, homogenize(48, 4))

# check decimation results
#dp <- dp[dp['Number.of.point.records']$Number.of.point.records>0,]
summary(dp)
plot(lascat.tst['Number.of.point.records'], main='N points per grid cell (Original)')
plot(dp['Number.of.point.records'], main='N points per grid cell (Decimated)')

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