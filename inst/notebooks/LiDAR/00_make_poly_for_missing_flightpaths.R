# Load libraries
library(lidR)
library(terra)
library(tidyverse)

# Set up workspace
scrdir <- file.path('/global/scratch/users/worsham/')
shapedir <- file.path(scrdir, 'EastRiverInputs', 'RMBL_2020_EastRiver_SDP_Boundary', 'RMBL_2020_EastRiver_SDP_Boundary')
neondir <- file.path(scrdir, 'neon_las')
datadir <- file.path(scrdir, 'las_decimated')

# Ingest NEON AOP boundary
bndpath <- list.files(shapedir, pattern='shp$', full.names=T)
bnd <- vect(bndpath)
plot(bnd)

# Make an erasing polygon to use for subsetting the las catalog
e1 <- cbind(c(320000, 340000, 340000, 320000), c(4250000, 4250000, 4330000, 4330000))
e1 <- vect(e1, type='polygons')

# Read in las catalog of decimated points
lascat <- readLAScatalog(list.files(datadir, full.names=T))

# Get lascat geometry, assign a value for aggregation, then plot to view
geo <- vect(lascat$geometry)
geo$val <- 1
plot(geo)

# Subset the las catalog to a narrower section including the gap
sub <- erase(geo, e1)
plot(sub, col='red')

# Erase the aop boundary by the erasing polygon as well
bnd.sub <- erase(bnd, e1)
sub.a <- aggregate(buffer(sub, 0.6), 'val')
miss <- erase(bnd.sub, sub.a)
plot(miss, col='red')
da <- disagg(miss)[1]

# Write as shapefile
writeVector(da, '/global/scratch/users/worsham/missing_flightpath/missing_flightpath.shp')
