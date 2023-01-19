# enviro_gradients_for_flightpaths
# Compute tables and plots for topographic variables underlying flightpaths
# Generates figures to depict the distribution of Breckheimer et al. East River flightpaths along several geophysical gradients

# Author: Marshall Worsham
# Created: 10-06-20
# Revised: 01-18-23

# Load libraries
library(sf)
library(dplyr)
library(terra)
library(raster)
library(concaveman)
library(stringr)
library(purrr)
library(tidyr)
library(ggplot2)
library(ggthemes)

# Load helper functions
source('/Users/hmworsham/Repos/eastriver/Forest_Inventory_Dataset/Site_Selection/ss.helpers.R')

# Set up environment
erdir <- file.path('/Volumes', 'GoogleDrive', 'My Drive', 'Research', 'RMBL')
rasdir <- file.path(erdir, 'RMBL-East River Watershed Forest Data', 'Data', 'Geospatial', 'Worsham_SiteSelection', '2021_Analysis_Layers', 'USGS_1-9_arcsec_DEM')
fpdir <- file.path(erdir,  'RMBL-East River Watershed Forest Data', 'DOE ESS RMBL Forest Ecohydrology Project', 'UAS_Flightpaths', 'Geospatial_Data')
sfdir <- file.path(erdir, 'RMBL-East River Watershed Forest Data', 'Data', 'Geospatial', 'RMBL_2020_EastRiver_SDP', 'RMBL_2020_EastRiver_SDP_Boundary')
lcdir <- file.path(erdir, 'RMBL-East River Watershed Forest Data', 'Data', 'Geospatial', 'RMBL_2020_EastRiver_SDP', 'UER_landcover_1m')
lithdir <- file.path(erdir, 'RMBL-East River Watershed Forest Data', 'Data', 'Geospatial', 'Wainwright_2021_Geology')

# Ingest AOP survey area polygon
aop <- st_read(file.path(sfdir, 'SDP_Boundary.shp'))

# Ingest topo rasters
topo.factors <- c('Aspect', 
                  'Curvature',
                  'DTM',
                  'DEM',
                  'Heat_Load',
                  'Slope', 
                  'TPI', 
                  'TWI')

toporasters <- flatten(lapply(topo.factors, get.rasters, rasdir))
toporasters <- lapply(toporasters, raster)

#############################
# Clean, crop, align rasters
#############################

# Function to crop raster to aop boundary
cropfun <- function(ras, shp){
  ras <- crop(ras, extent(shp))
  ras <- mask(ras, shp)
  return(ras)
}

# Function to align rasters on same grid (resample and align) 
alignfun <- function(x, target, method='bilinear'){
  xnew = resample(x, target, method)
  ex = extent(target)
  xnew = crop(xnew, ex)
  return(xnew)
}

# Crop topos to AOP and align to each other
toporasters <- lapply(toporasters, cropfun, aop)
toporasters <- lapply(toporasters, alignfun, toporasters[[4]], 'ngb')
lapply(toporasters, res)

# toporasters <- lapply(toporasters, function(x) {
#   if(res(x)[1]==10){
#     print(res(x)[1])
#     x=aggregate(x,fact=10)}
#   else(x=x)
#   return(x)})

# Read in flightlines
layers <- list.files(file.path(fpdir, 'Version4'), pattern='v4_flightlines.kml', full.names=T)

# # Get layer names
# lapply(layers, st_layers)

# Define function to get flightlines and draw convex hull polys around them
get.fls <- function(layer){
  layname <- str_replace(unlist(strsplit(layer, '/'))[12], '_flightlines.kml', '')
  fl <- st_read(layer, layer='Waypoints')[1]$geometry
  cut <- length(fl)-1
  fl <- fl[2:cut]
  ch <- st_convex_hull(st_union(fl))
  ch <- st_transform(ch, crs(toporasters[[1]]))
  plot(ch)
  return(ch)
}

# Ingest flightlines as convex hull polygons
flines <- lapply(layers, get.fls)

# Concatenate flight polygons to a multipolygon
multipoly <- flines[[1]]
for (i in 2:length(flines)) {
  multipoly <- c(multipoly, flines[[i]])
}

# Create synthetic flightpath from points
coords <- c(
  -106.999944,38.981854,
  -106.997347,38.982717,
  -106.991755,38.975125,
  -106.994788,38.974083)
lon <- coords[c(TRUE,FALSE)]
lat <- coords[c(FALSE,TRUE)]
synth.df <- data.frame(lon, lat)
synth.poly <- st_as_sf(synth.df, 
                       coords = c("lon", "lat"), 
                       crs = 'EPSG:4326')
synth.poly <- st_convex_hull(st_union(synth.poly))
synth.poly <- st_transform(synth.poly, crs(multipoly))

# Concatenate multipoly and synthetic poly
multipoly <- c(multipoly, synth.poly)

# Alternate approach, creating a unioned polygon
# multisf <- st_union(xx[[1]]) %>% st_union(xx[[2]]) %>% st_union(xx[[3]]) %>% st_union(xx[[4]]) %>% st_union(xx[[5]]) %>% st_union(xx[[6]]) %>% st_union(xx[[7]]) %>% st_union(xx[[8]])

# Convert multipolygon to terra::vect for value extraction
#sites <-  vect(multisf)
sites <- vect(multipoly)
plot(sites)

# Extract values from rasters at vector
toporasters <- lapply(toporasters, rast)
topovals <- lapply(toporasters, terra::extract, sites)

# Clean dataframe
topovals <- data.frame(topovals)
names(topovals)[1] <- 'Flightpath'
topovals <- dplyr::select(topovals, -contains('ID'))

topovals <- topovals[,c('Flightpath',
                        'usgs_205faspect_100m',
                        #'usgs_curvature_10m',
                        'USGS_13_n39.40_w107.108_mosaic_wgs84utm13n',
                        'usgs_heatload_100m',
                        'slope',
                        'usgs_tpi_1km',
                        'usgs_twi_1km',
                        'usgs_twi_100m'
)]

names(topovals) <- c('flightpath', 
                     'Folded Aspect', 
                     #'Curvature',
                     'Elevation',
                     'Heat Load',
                     'Slope',
                     'TPI 1000 m',
                     'TWI 1000 m',
                     'TWI 100 m')

# Assign flightpath name to flightpath index after `extract` operation dropped names
topovals <- topovals %>%
  mutate(flightpath=case_when(flightpath==1~'EastRiver-1',
                                 flightpath==2~'EastRiver-2',
                                 flightpath==3~'EastRiver-3',
                                 flightpath==4~'EastRiver-4',
                                 flightpath==5~'Snodgrass-1',
                                 flightpath==6~'Snodgrass-2',
                                 flightpath==7~'Snodgrass-3',
                                 flightpath==8~'Snodgrass-4',
                                 flightpath==9~'EastRiver-5',
                              ))

topovals.long <- pivot_longer(topovals, cols=names(topovals)[2:length(topovals)])
unique(topovals.long$flightpath)

# Ingest landcover raster
lc <- raster(file.path(lcdir, 'UER_landcover_1m_v4.tif'))

# Resample to 10 m resolution using NN interpolation
lc <- projectRaster(lc, 
                    to=raster(extent(lc), crs=crs(lc), resolution=10),
                    method='ngb')
#lc <- aggregate(lc,fact=10, fun='median')
lc <- cropfun(lc, aop)
lc <- alignfun(lc, raster(toporasters[[1]]), method='ngb')

lc.forest <- lc
lc.forest[lc.forest>2 | lc.forest==0] <- NA
lc.forest[!is.na(lc.forest)] <- 1

# Convert to terra::rast type
lc <- rast(lc)
lc.forest <- rast(lc.forest)

# Mask topos with forest
toporasters.forest <- lapply(toporasters, mask, lc.forest)
topo.forest.vals <- lapply(toporasters.forest, values)

# Get dataframe of toporaster values masked by forest
topo.forest.vals <- data.frame(topo.forest.vals)

names(topo.forest.vals) <- unlist(lapply(toporasters, names))
topo.forest.vals <- topo.forest.vals[,c(
                                'usgs_205faspect_100m',
                                #'usgs_curvature_10m',
                                'USGS_13_n39.40_w107.108_mosaic_wgs84utm13n',
                                'usgs_heatload_100m',
                                'slope',
                                'usgs_tpi_1km',
                                'usgs_twi_1km',
                                'usgs_twi_100m'
                                )]

names(topo.forest.vals) <- c(
                     'Folded Aspect', 
                     #'Curvature',
                     'Elevation',
                     'Heat Load',
                     'Slope',
                     'TPI 1000 m',
                     'TWI 1000 m',
                     'TWI 100 m')
topo.forest.vals$px <- rownames(topo.forest.vals)

topo.for.vals.long <- pivot_longer(topo.forest.vals, cols=names(topo.forest.vals)[1:length(topo.forest.vals)-1])

##############################
# PFT mix under flightpaths
##############################
pftvals <- terra::extract(lc, sites)

pftvals <- pftvals %>%
  mutate(flightpath=case_when(ID==1~'EastRiver-1',
                              ID==2~'EastRiver-2',
                              ID==3~'EastRiver-3',
                              ID==4~'EastRiver-4',
                              ID==5~'Snodgrass-1',
                              ID==6~'Snodgrass-2',
                              ID==7~'Snodgrass-3',
                              ID==8~'Snodgrass-4', 
                              ID==9~'EastRiver-5',
                              ), 
         class=case_when(UER_landcover_1m_v4==1~'needle-leaf trees and shrubs',
                         UER_landcover_1m_v4==2~'deciduous trees and shrubs',
                         UER_landcover_1m_v4==3~'deciduous meadow and subshrub',
                         UER_landcover_1m_v4==4~'bare rock, soil, gravel, asphalt',
                         UER_landcover_1m_v4==5~'water',
                         UER_landcover_1m_v4==6~'snow',
                         UER_landcover_1m_v4==7~'buildings'))

# pftvals <- pftvals %>%
#   group_by(flightpath) %>%
#   mutate(npx=n()) %>%
#   ungroup()

# pftvals <- pftvals %>% 
#   group_by(flightpath, `canopy_decid_conif7_2@PERMANENT`) %>%
#   summarize(Freq=n())

pftfreqs <- xtabs(~flightpath+class, data=pftvals)

# write.table(pftfreqs, file = "~/Downloads/pftstats.csv", sep = ",", quote = FALSE, row.names = T)

######################
# Lithology
#####################

lith <- rast(file.path(lithdir, 'EastRiver_GeolGrid.tif'))

lithvals <- terra::extract(lith, sites)

lithvals <- lithvals %>%
  mutate(flightpath=case_when(ID==1~'EastRiver-1',
                              ID==2~'EastRiver-2',
                              ID==3~'EastRiver-3',
                              ID==4~'EastRiver-4',
                              ID==5~'Snodgrass-1',
                              ID==6~'Snodgrass-2',
                              ID==7~'Snodgrass-3',
                              ID==8~'Snodgrass-4',
                              ID==9~'EastRiver-5'
                              ))

# Merge lithology key
lithkey <- read.csv(file.path(lithdir, 'EastRiver_GeolGrid_Key.csv'), header=T)
lithvals <- merge(lithvals, lithkey, by.x='EastRiver_GeolGrid', by.y='Key')

lithvals.sum <- lithvals %>%
  group_by(flightpath) %>%
  mutate(npx=n()) %>%
  ungroup()

lithfreqs <- xtabs(~flightpath+Group, data=lithvals)

# write.table(lithfreqs, file = "~/Downloads/lithstats.csv", sep = ",", quote = FALSE, row.names = T)

################
# Histograms
################

# Simple histograms for each topo factor facet-wrapped
ggplot(topo.for.vals.long, aes(x=value, y=after_stat(count/sum(count)))) + 
  geom_histogram(bins=12, fill='white', color='blue3') + 
  facet_wrap('name', scales='free_x') + 
  labs(x='Value', y = 'Frequency') + 
  theme_minimal() + 
  theme(axis.title=element_text(size=16),
        axis.text=element_text(size=14),
        title=element_text(size=16, hjust=0.5),
        strip.text.x=element_text(size=14, hjust=0.5))

# Histograms per topographic factor, all flightpaths v. full domain
topo.for.vals.long$set <- 'Full forest domain'
topovals.long$set <- 'Flightpaths'
topovals.concat <- mapply(c, topo.for.vals.long, topovals.long)
topovals.concat <- data.frame(topovals.concat)
topovals.concat$value <- as.numeric(topovals.concat$value)

ggplot(topovals.concat, aes(x=value, y=after_stat(count/sum(count)))) +
  geom_histogram(data=subset(topovals.concat, set=='Full forest domain'),
                 aes(fill=set),
                 bins=18,
                 alpha =0.7) + 
  geom_histogram(data=subset(topovals.concat, set=='Flightpaths'), 
                 aes(fill=set), 
                 bins=18,
                 alpha =0.7) +
  scale_fill_manual(name='Area of consideration',
                    labels=c('Flightpaths', 'Full forest domain'),
                    values=c('steelblue3', 'firebrick1')) +
  facet_wrap('name', scales='free_x') + 
  labs(x='Value', y = 'Frequency') + 
  theme_fivethirtyeight() + 
  theme(axis.title=element_text(size=16),
        axis.text=element_text(size=14),
        title=element_text(size=16, hjust=0.5),
        strip.text.x=element_text(size=14, hjust=0.5))

# Histograms per topographic factor shaded by flightpath
ggplot(topovals.long, aes(x=value, y=after_stat(count/sum(count)), fill=as.factor(flightpath))) +
  geom_histogram(position='stack', bins=18) +
  scale_fill_brewer(palette='RdYlBu', name='Flightpath') + 
  facet_wrap("name", scales='free_x') + 
  labs(x='Value', y = 'Frequency') + 
  theme_fivethirtyeight() + 
  theme(axis.title=element_text(size=16),
        axis.text=element_text(size=14),
        title=element_text(size=16, hjust=0.5),
        strip.text.x=element_text(size=14, hjust=0.5))

# Bar plots of lithology 
lithvals.ord <- lithvals %>%
  add_count(Group) %>%
  arrange(desc(n))

ggplot(lithvals.ord, aes(x=flightpath, 
                         #group=Group, 
                         fill=factor(Group, levels=unique(Group)), 
                         order=desc(n))) +
  geom_bar() + 
  scale_fill_brewer(palette='BrBG', direction=-1, name='Lithologic group') + 
  labs(x='Flightpath', y = 'N pixels') + 
  theme_minimal() + 
  theme(axis.title=element_text(size=16),
        axis.text=element_text(size=14),
        axis.text.x=element_text(angle=60, hjust=1),
        title=element_text(size=16, hjust=0.5),
        strip.text.x=element_text(size=14, hjust=0.5))

# Bar plots of land cover 
ggplot(arrange(pftvals, UER_landcover_1m_v4), aes(x=flightpath, 
                         fill=factor(class, levels=unique(class)))) +
  geom_bar() + 
  scale_fill_manual(values = c('darkgreen',
                               'lightgreen',
                               'darkolivegreen1',
                               'burlywood',
                               'dodgerblue',
                               'ivory3'), 
                    name='Land-cover class') + 
  labs(x='Flightpath', y = 'N pixels') + 
  theme_minimal() + 
  theme(axis.title=element_text(size=16),
        axis.text=element_text(size=14),
        axis.text.x=element_text(angle=60, hjust=1),
        title=element_text(size=16, hjust=0.5),
        strip.text.x=element_text(size=14, hjust=0.5),
        asp=1)

# Generate histograms of values and write to PNG
# lapply(topovals, FUN=function(x) {
#   png(file=paste0(names(x[2]), '_hist.png'))
#   hist(x[,2], col='aquamarine', main=names(x[2]))
#   dev.off()
# })
