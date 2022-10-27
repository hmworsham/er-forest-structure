# Compute histograms for topographic variables underlying flightpaths

# Load libraries
library(sf)
library(terra)
library(concaveman)

# Load helper functions
source('/Users/hmworsham/Repos/eastriver/Forest_Inventory_Dataset/Site_Selection/ss.helpers.R')

# Set up environment
rasdir <- file.path(erdir, 'RMBL-East River Watershed Forest Data', 'Data', 'Geospatial', 'Worsham_SiteSelection', '2021_Analysis_Layers', 'USGS_1-9_arcsec_DEM')

# Read in flight polygons
flights.3 <- st_read('/Volumes/GoogleDrive/My Drive/Research/RMBL/Working_Files/UAS/Output/ess_flight_polygons_v3.gpkg')

er.flights <- flights.3[4:7,]

# Read in flightlines
layers <- list.files('~/Downloads/', pattern='flightlines.kml', full.names=T)
layers <- str_replace(unlist(lapply(strsplit(layers, '/'), '[', 6)), '_flightlines.kml', '')

layers <- st_layers(layers[1])
layers

data_frame(name=layers$name, type=flatten_chr(layers$geomtype)) %>%
  count(name, type, sort=TRUE)

get.fls <- function(layer){
  layname <- str_replace(unlist(strsplit(layer, '/'))[6], '_flightlines.kml', '')
  fl <- st_read(layer, layer=layname)
  ch <- st_convex_hull(fl)
  ch <- st_transform(ch, crs(toporasters[[1]]))
  plot(ch)
  return(ch)
}

xx <- lapply(layers, get.fls)
multisf <- st_union(xx[[1]]) %>% st_union(xx[[2]]) %>% st_union(xx[[3]]) %>% st_union(xx[[4]]) %>% st_union(xx[[5]]) %>% st_union(xx[[6]]) %>% st_union(xx[[7]]) %>% st_union(xx[[8]])

plot(multisf)

convhulls <- get.fls('~/Downloads')
convhulls
sg1 <- st_read('/Volumes/GoogleDrive/My Drive/Research/RMBL/Working_Files/UAS/Output/KueppersDOE_Snodgrass_Flight1_v2_flightlines.kml', layer='KueppersDOE_Snodgrass_Flight1_v1')

sg2 <- st_read('/Volumes/GoogleDrive/My Drive/Research/RMBL/Working_Files/UAS/Output/KueppersDOE_Snodgrass_Flight2_v2_flightlines.kml', layer='KueppersDOE_Snodgrass_Flight2_v1')

sg3 <- st_read('/Volumes/GoogleDrive/My Drive/Research/RMBL/Working_Files/UAS/Output/KueppersDOE_Snodgrass_Flight3_v2_flightlines.kml', layer='KueppersDOE_Snodgrass_Flight3_v1')

# Convert flightlines to convex hull polygon surrounding flight area
sg1 <- st_convex_hull(sg1)
sg2 <- st_convex_hull(sg2)
sg3 <- st_convex_hull(sg3)

# Plot convex hulls to confirm
plot(st_convex_hull(sg1))
plot(st_convex_hull(sg2))
plot(st_convex_hull(sg3))

# Align CRS
sg1 <- st_transform(sg1, crs(toporasters[[1]]))
sg2 <- st_transform(sg2, crs(toporasters[[1]]))
sg3 <- st_transform(sg3, crs(toporasters[[1]]))
er.flights <- st_transform(er.flights, crs(toporasters[[1]]))

# Union all polygons into multipolygon
multisf <- st_union(sg1) %>% st_union(sg2) %>% st_union(sg3) %>% st_union(er.flights)

# Ingest topo rasters
topo.factors <- c('Aspect', 
                  'Curvature',
                  'DTM',
                  'DEM',
                  'Heat_Load',
                  'Slope', 
                  'TPI', 
                  'TWI')

toporasters = flatten(lapply(topo.factors, get.rasters, rasdir))

# Convert multipolygon to terra::vect for value extraction
sites = vect(multisf)

# Extract values from rasters at vector
topovals = lapply(toporasters, terra::extract, sites)

# Generate histograms of values and write to PNG
lapply(topovals, FUN=function(x) {
  png(file=paste0(names(x[2]), '_hist.png'))
  hist(x[,2], col='aquamarine', main=names(x[2]))
  dev.off()
})
