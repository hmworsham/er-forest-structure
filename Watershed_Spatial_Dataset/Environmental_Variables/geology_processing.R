
#############################
# Set up working environment
#############################

# Define directories
datadir <- file.path('/Volumes', 'GoogleDrive', 'My Drive', 'Research', 'RMBL', 'RMBL-East River Watershed Forest Data', 'Data')
geodir <- file.path(datadir, 'Geospatial', 'Colorado_Geological_Survey')
outdir <- file.path(geodir, 'Processed')
dir.create(outdir)

geol <- st_read(file.path(geodir, 'RS-37', 'GIS_data', 'Gunnison_County_Geology_Map.gdb'), layer='MapUnitPolys')
geol <- st_transform(geol, crs(aop))
geol <- st_intersection(geol, aop)
ras <- raster(ncol=237, nrow=208)
extent(ras) <- extent(aop)
crs(ras) <- crs(aop)
ras <- projectRaster(ras, crs=crs(aop))
geol$MapUnit_Index <- factor(geol$MapUnit, labels = seq(1,9))
geol <- rasterize(geol, ras, 'MapUnit_Index')

# Plot geology to check
plot(aop, col='NA', border='black')
plot(geol, col=mrmoose::icolors('mario'), asp=1)

# Write raster
writeRaster(geol, file.path(outdir, 'COGS_eastriver_geology.tif'))

## TODO: doesn't fill full extent, so need to do some more processing on geol
geol.hr <- raster(file.path(datadir, 'Geospatial', 'Wainwright_2021_Geology', 'EastRiver_GeolGrid.tif'))
geol.hr <- projectRaster(geol.hr, crs=crs(aop))
plot(geol.hr, col=mrmoose::icolors('crayons'))

plot(geol, col=mrmoose::icolors('mario'), asp=1, add=T)
plot(geol.hr, col=mrmoose::icolors('CC'), add=T, asp=1)

geol.hr.bnd <- st_as_sfc(st_bbox(geol.hr))
geol.hr.bnd <- st_transform(geol.hr.bnd, crs(aop))
remove <- st_difference(aop, geol.hr.bnd)

geol.rem <- mask(geol, remove) 
raster::merge(geol.rem, geol.hr)
