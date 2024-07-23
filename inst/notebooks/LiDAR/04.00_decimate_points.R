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
shapedir <- file.path(config$extdata$scratch, 'EastRiver', 'RMBL_2020_EastRiver_SDP_Boundary')
datadir <- file.path(config$extdata$scratch, 'las_abg')
neondir <- file.path(config$extdata$scratch, 'neon_las_regridded')
outdir <- file.path(config$extdata$scratch, 'las_decimated')
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

# Check decimation results
#dp <- dp[dp['Number.of.point.records']$Number.of.point.records>0,]
summary(dp)
plot(lascat.tst['Number.of.point.records'], main='N points per grid cell (Original)')
plot(dp['Number.of.point.records'], main='N points per grid cell (Decimated)')

las <- readLAS(infiles[9])
plot(las[1:50000])
hist(las$Z)
rglwidget()
