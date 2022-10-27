library(rgdal)
library(sf)
library(terra)
library(raster)

soils.sf <- st_read('~/Downloads/gSSURGO_CO/gSSURGO_CO_AOP.shp')
soils.sf <- st_transform(soils.sf, crs='EPSG:32613')
soils.sf <- vect(soils.sf)

soils.sf <- merge(soils.sf, comps, by='MUKEY')

soils.sf2 <- soils.sf[!duplicated(soils.sf$OBJECTID.x)]

empty.rs <- rast()
ext(empty.rs) <- ext(soils.sf)
res(empty.rs) <- 10

# Make a raster of soils
length(unique(soils.sf$taxorder))
soils.ras <- terra::rasterize(soils.sf, empty.rs, field='taxsuborder')
hist(values(soils.ras))
terra::writeRaster(soils.ras, '~/Downloads/gSSURGO_CO/gSSURGO_CO_AOP.tif', overwrite=T)

plot(soils.ras)

# Join with components table
comps <- read.csv('~/Downloads/gSSURGO_CO/co_gssurgo_components.csv')

