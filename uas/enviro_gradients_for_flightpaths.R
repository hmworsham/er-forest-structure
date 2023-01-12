# Compute histograms for topographic variables underlying flightpaths

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

# Load helper functions
source('/Users/hmworsham/Repos/eastriver/Forest_Inventory_Dataset/Site_Selection/ss.helpers.R')

# Set up environment
erdir <- file.path('/Volumes', 'GoogleDrive', 'My Drive', 'Research', 'RMBL')
rasdir <- file.path(erdir, 'RMBL-East River Watershed Forest Data', 'Data', 'Geospatial', 'Worsham_SiteSelection', '2021_Analysis_Layers', 'USGS_1-9_arcsec_DEM')
fpdir <- file.path(erdir,  'RMBL-East River Watershed Forest Data', 'DOE ESS RMBL Forest Ecohydrology Project', 'UAS_Flightpaths', 'Geospatial_Data')
sfdir <- file.path(erdir, 'RMBL-East River Watershed Forest Data', 'Data', 'Geospatial', 'RMBL_2020_EastRiver_SDP', 'RMBL_2020_EastRiver_SDP_Boundary')

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
layers <- list.files(file.path(fpdir, 'Version4'), pattern='flightlines.kml', full.names=T)

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

xx <- lapply(layers, get.fls)
multipoly <- xx[[1]]
for (i in 2:length(xx)) {
  multipoly <- c(multipoly, xx[[i]])
}

# multisf <- st_union(xx[[1]]) %>% st_union(xx[[2]]) %>% st_union(xx[[3]]) %>% st_union(xx[[4]]) %>% st_union(xx[[5]]) %>% st_union(xx[[6]]) %>% st_union(xx[[7]]) %>% st_union(xx[[8]])

# Convert multipolygon to terra::vect for value extraction
#sites <-  vect(multisf)
sites <- vect(multipoly)
plot(sites[5])

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
                        'usgs_twi_1km'
)]

names(topovals) <- c('flightpath', 
                     'Folded_Aspect', 
                     #'Curvature',
                     'Elevation',
                     'Heat_Load',
                     'Slope',
                     'TPI_1K',
                     'TWI_1K')

topovals <- topovals %>%
  mutate(flightpath=case_when(flightpath==1~'EastRiver-1',
                                 flightpath==2~'EastRiver-2',
                                 flightpath==3~'EastRiver-3',
                                 flightpath==4~'EastRiver-4',
                                 flightpath==5~'Snodgrass-1',
                                 flightpath==6~'Snodgrass-2',
                                 flightpath==7~'Snodgrass-3',
                                 flightpath==8~'Snodgrass-4'))

topovals.long <- pivot_longer(topovals, cols=names(topovals)[2:length(topovals)])
View(topovals.long)

# Ingest 
lc <- raster('~/Downloads/UER_landcover_1m_v4.tif')
lc <- aggregate(lc,fact=10, fun='mean')
lc <- cropfun(lc, aop)
lc <- alignfun(lc, raster(toporasters[[1]]), method='ngb')
lc <- rast(lc)

lc.forest <- lc
lc.forest[lc.forest>2] <- NA
lc.forest[lc.forest==0] <- NA
lc.forest[!is.na(lc.forest)] <- 1
lc.forest <- rast(lc.forest)

plot(lc.forest)

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
                                'usgs_twi_1km'
                                )]

names(topo.forest.vals) <- c(
                     'Folded_Aspect', 
                     #'Curvature',
                     'Elevation',
                     'Heat_Load',
                     'Slope',
                     'TPI_1K',
                     'TWI_1K')
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
                              ID==8~'Snodgrass-4'))

pftvals <- pftvals %>%
  group_by(flightpath) %>%
  mutate(npx=n()) %>%
  ungroup()

pftvals <- pftvals %>% 
  group_by(flightpath, `canopy_decid_conif7_2@PERMANENT`) %>%
  summarize(Freq=n())

pftfreqs <- xtabs(~flightpath+`canopy_decid_conif7_2@PERMANENT`, data=pftvals)
write.table(pftfreqs, file = "~/Downloads/pftstats.csv", sep = ",", quote = FALSE, row.names = T)

######################
# Lithology
#####################

lith <- rast(file.path(erdir, 'RMBL-East River Watershed Forest Data', 'Data', 'Geospatial', 'Wainwright_2021_Geology', 'EastRiver_GeolGrid.tif'))

lithvals <- terra::extract(lith, sites)

lithvals <- lithvals %>%
  mutate(flightpath=case_when(ID==1~'EastRiver-1',
                              ID==2~'EastRiver-2',
                              ID==3~'EastRiver-3',
                              ID==4~'EastRiver-4',
                              ID==5~'Snodgrass-1',
                              ID==6~'Snodgrass-2',
                              ID==7~'Snodgrass-3',
                              ID==8~'Snodgrass-4'))

lithvals.sum <- lithvals %>%
  group_by(flightpath) %>%
  mutate(npx=n()) %>%
  ungroup()


lithfreqs <- xtabs(~flightpath+EastRiver_GeolGrid, data=lithvals)
write.table(lithfreqs, file = "~/Downloads/lithstats.csv", sep = ",", quote = FALSE, row.names = T)

################
# Histograms
################

# Generate histograms of values
ggplot(topovals.long, aes(x=value, y=after_stat(count/sum(count)), fill=as.factor(flightpath))) +
  geom_histogram(position='stack', bins=12) +
  scale_fill_brewer(palette='RdBu', name='Flightpath') + 
  facet_wrap("name", scales='free_x') + 
  labs(x='Value', y = 'Frequency') + 
  theme_minimal() + 
  theme(axis.title=element_text(size=16),
        axis.text=element_text(size=14),
        title=element_text(size=16, hjust=0.5),
        strip.text.x=element_text(size=14, hjust=0.5),
        asp=1)

ggplot(topo.for.vals.long, aes(x=value, y=after_stat(count/sum(count)))) + 
  geom_histogram(bins=12, fill='white', color='blue3') + 
  facet_wrap('name', scales='free_x') + 
  labs(x='Value', y = 'Frequency') + 
  theme_minimal() + 
  theme(axis.title=element_text(size=16),
        axis.text=element_text(size=14),
        title=element_text(size=16, hjust=0.5),
        strip.text.x=element_text(size=14, hjust=0.5),
        asp=1)

ggplot(tv, aes(x=value, y=after_stat(count/sum(count)), fill=set)) + 
  geom_histogram(bins=12, 
                 alpha=0.7) + 
  # geom_histogram(data=topovals.long, 
  #                aes(x=value, y=after_stat(count/sum(count))), 
  #                bins=12, 
  #                fill='firebrick1',
  #                alpha=0.7) + 
  scale_fill_manual(name='Area of consideration',
                    labels=c('Forested Domain', 'Flightpaths'),
                    values=c('dodgerblue2', 'firebrick1')) +
  facet_wrap('name', scales='free_x') + 
  labs(x='Value', y = 'Frequency') + 
  theme_minimal() + 
  theme(axis.title=element_text(size=16),
        axis.text=element_text(size=14),
        title=element_text(size=16, hjust=0.5),
        strip.text.x=element_text(size=14, hjust=0.5),
        asp=1)

topo.for.vals.long$set <- 'Forested domain'
topovals.long$set <- 'Flightpaths'
tv <- bind_rows(topo.for.vals.long, topovals.long)


# Generate histograms of values and write to PNG
# lapply(topovals, FUN=function(x) {
#   png(file=paste0(names(x[2]), '_hist.png'))
#   hist(x[,2], col='aquamarine', main=names(x[2]))
#   dev.off()
# })
