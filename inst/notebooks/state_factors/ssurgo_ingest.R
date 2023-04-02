# Load libraries
library(FedData)
library(sf)
library(tidyverse)
library(raster)
library(stars)
library(viridis)

#############################
# Set up working environment
#############################
# Define directories
datadir <- file.path('/Volumes', 'GoogleDrive', 'My Drive', 'Research', 'RMBL', 'RMBL-East River Watershed Forest Data', 'Data')
sfdir <- file.path(datadir, 'Geospatial', 'RMBL_2020_EastRiver_SDP', 'RMBL_2020_EastRiver_SDP_Boundary')
soil.dir <- file.path(datadir, 'Soil')
ssurgo.dir <- file.path(soil.dir, 'SSURGO')
outdir <- file.path(ssurgo.dir, 'PROCESSED')
dir.create(outdir)

# Cleaning function
not_all_na <- function(x) any(!is.na(x)) #for data cleaning

#####################
# Ingest data
#####################

# Ingest AOP survey area polygon
aop <- st_read(file.path(sfdir, 'SDP_Boundary.shp'))
aop <- as_Spatial(aop)

# Ingest SSURGO data
SSURGO.areas.er <- get_ssurgo(template=aop, 
                              label='CO_EastRiver',
                              raw.dir=file.path(ssurgo.dir, 'RAW'), 
                              extraction.dir=file.path(ssurgo.dir, 'EXTRACTIONS', 'CO_EastRiver'))

# Check all areas ended up in set
all(unique(SSURGO.areas.er$spatial$AREASYMBOL)==c('CO654', 'CO660', 'CO661', 'CO662'))

# Assign spatial layer to variable
SSURGO.spatial <- SSURGO.areas.er$spatial
names(SSURGO.spatial) <- c('areasymbol', 'spatialver', 'musym', 'mukey')

# Transform to shapefile and set CRS to that of AOP
SSURGO.sf <- st_as_sf(SSURGO.spatial)
SSURGO.sf <- st_transform(SSURGO.sf, st_crs(aop))

# Plot to visualize
#plot(SSURGO.spatial, col=heat.colors(100))
#plot(SSURGO.spatial[SSURGO.spatial$AREASYMBOL=='CO662',])

ggplot() + 
  geom_sf(data=SSURGO.sf, aes(fill=areasymbol))

#######################################
# Dig into data layers, so to speak
#######################################

# Create horizon, component, and mapunit dataframes for later merge
horz <- SSURGO.areas.er$tabular$chorizon
comp <- SSURGO.areas.er$tabular$component
mu <- SSURGO.areas.er$tabular$mapunit

# Check number of unique cokeys and mukeys in each set
length(unique(horz$cokey)) #cokeys aren't equal (117 v 272) --> some components lack horizon-level data
length(unique(comp$cokey))
length(unique(mu$mukey)) #mukeys are equal
length(unique(comp$mukey))

# View components that lack horizon data
comp[which(!comp$cokey %in% horz$cokey),]

# Remove empty columns
horz <- horz %>% select_if(not_all_na) # 171 to 131 variables
comp <- comp %>% select_if(not_all_na) # 109 to 77 variables
mu <- mu %>% select_if(not_all_na) # 24 to 9 variables

#######################
# Horizons
#######################

mc <- left_join(mu, comp, by='mukey')
mch <- left_join(mc, horz, by='cokey')
mch$mukey <- as.character(mch$mukey)
mchs <- left_join(mch, SSURGO.sf, by='mukey')

# Plot horizon depths
mchs.sub <- mchs %>% 
  group_by(cokey) %>%
  distinct(hzname, .keep_all=T) %>%
  arrange(hzdepb.r) %>% 
  mutate(label_y=cumsum(hzdepb.r)) %>% 
  ungroup()

ggplot(mchs.sub, aes(x=as.factor(cokey), y=hzdepb.r, fill=hzdepb.r, label=hzname)) +
  geom_bar(stat='identity', position='stack', color='black') +
  geom_text(aes(y=label_y, label = hzname), vjust=-0.25, colour = "black") +
  scale_fill_gradient(low='sienna4', high='white', name='Horizon Depth (cm)') +
  ylab('Horizon Depth (cm)') + 
  xlab('Component ID') +
  theme(axis.text.x = element_text(angle = 70, hjust=1)) +
  scale_y_reverse() + 
  facet_wrap(~areasymbol, scales='free')

# Determine total soil depth per component
depth <- horz %>% 
  group_by(cokey) %>%
  summarize(total.depth = max(hzdepb.r))

# Filter out horizons below 100cm
horz.100 <- horz %>%
  filter(hzdept.r <= 100) %>%
  droplevels()

# We want only one observation per cokey to join horz data to component data
component_count <- horz.100 %>%
  group_by(cokey) %>%
  summarize(count = n()) %>%
  filter(count > 1)

# Summarize characteristics of interest with a weighted mean of included horizons
horz.100 <- horz.100 %>%
  mutate(thick = ifelse(hzdepb.r > 100, 100 - hzdept.r, 
                        hzdepb.r - hzdept.r)) %>%  
  group_by(cokey) %>%
  summarise(awc = round(weighted.mean(awc.r, thick, na.rm = TRUE), 2),
            sand = round(weighted.mean(sandtotal.r, thick, na.rm = TRUE),2),
            silt = round(weighted.mean(silttotal.r, thick, na.rm = TRUE),2),
            clay = round(weighted.mean(claytotal.r, thick, na.rm = TRUE),2),
            om = round(weighted.mean(om.r, thick, na.rm = TRUE),2),
            ksat = round(weighted.mean(ksat.r, thick, na.rm = TRUE),2),
            k = round(weighted.mean(kffact, thick, na.rm = TRUE),2),
            cec = round(weighted.mean(cec7.r, thick, na.rm = TRUE),2),
            ph = round(weighted.mean(ph1to1h2o.r, thick),2))

# Join computed weighted means with deepest soil depth
horz.100 <- left_join(horz.100, depth, by = "cokey")
head(horz.100)

######################
# Components
######################
# Get rid of component colnames we don't want
length(colnames(comp))
colnames(comp)
comp <- comp %>%
  dplyr::select(c(comppct.r, compname, majcompflag, slope.r, 
                  slopelenusle.r, runoff, tfact, wei, weg, 
                  elev.r, albedodry.r, airtempa.r, map.r, ffd.r, 
                  taxpartsize, mukey, cokey,
                  #erocl, cropprodindex
                  ))

# Join components and horizons
comp_horz <- left_join(comp, horz.100, by=c('cokey'))
dim(comp_horz)
head(comp_horz)

# Pull a test component
test <- comp_horz %>%
  filter(cokey == "23005162") #cokey full of NAs, noted from earlier slide
test

###########
# Mapunits
###########
# Get rid of mapunit columns we don't need
colnames(mu)
mu <- mu %>%
  dplyr::select(c(musym, muname, muacres, mukey))

# Join mapunits and component-horizon data
full_soil <- left_join(comp_horz, mu, by = c('mukey'))
dim(full_soil)

# Summarize variable of interest by component-percent weighted mean
# That is, for any variable of interest, assign a value to each map unit that is a mean value weighted by the % of the component occupying the map unit
full_soil_wm <- full_soil %>%
  group_by(mukey) %>%
  summarise(
    awc_wm = round(weighted.mean(awc, comppct.r, na.rm = TRUE), 2),
    sand_wm = round(weighted.mean(sand, comppct.r, na.rm = TRUE),2),
    silt_wm = round(weighted.mean(silt, comppct.r, na.rm = TRUE),2),
    clay_wm = round(weighted.mean(clay, comppct.r, na.rm = TRUE),2),
    om_wm = round(weighted.mean(om, comppct.r, na.rm = TRUE),2),
    ksat_wm = round(weighted.mean(ksat, comppct.r, na.rm = TRUE),2),
    k_wm = round(weighted.mean(k, comppct.r, na.rm = TRUE),2),
    cec_wm = round(weighted.mean(cec, comppct.r, na.rm = TRUE),2),
    ph_wm = round(weighted.mean(ph, comppct.r),2),
    td_wm = round(weighted.mean(total.depth, comppct.r, na.rm=TRUE), 2))

# Join weighted-mean component values to mapunits 
#names(SSURGO.areas.er$spatial) <- c('areasymbol', 'spatialver', 'musym', 'mukey')
soil_spatial <- merge(SSURGO.sf, full_soil_wm, by=c('mukey'))

# Function to rasterize mapunit--level estimates
rasterize.fun <- function(in.sf, ncol, nrow, target.sf) {
  ra = raster(ncol=ncol, nrow=nrow)
  extent(ra) = extent(target.sf)
  ra.ls = list()
  for(i in seq(5, length(soil_spatial)-1)){
    ra.ls[[i]] = rasterize(in.sf[i], ra, names(in.sf[i])[1])
  }
  ra.ls = ra.ls[5:(length(soil_spatial)-1)]
  for(i in seq_along(ra.ls)){
    names(ra.ls[[i]]) <- names(soil_spatial)[4+i]
    crs(ra.ls[[i]]) <- 'EPSG:32613'
  }
}

# Rasterize mapunit--level estimates
soil.rast.ls <- rasterize.fun(soil_spatial, ncol=237, nrow=208, aop)

# Plot mapunit--level estimates
par(mfcol=c(3,3), mar=c(rep(1,2), rep(3,2)))
for(i in seq(3, length(soil.rast.ls))) {
  plot(soil.rast.ls[[i]], 
       col=nifty::icolors('crayons'),
       asp=1, 
       main=names(soil.rast.ls[[i]]))
}

#######################
# Write rasters out
#######################
# Write out
lapply(soil.rast.ls, function(x) {
  writeRaster(
    x, 
    file.path(outdir, paste0(names(x), '.tif')), 
    crs='EPSG:32613',
    driver='GTiff', 
    overwrite=T)
})

# Check validity of saved tif
plot(raster(file.path(outdir, paste0(names(soil.rast.ls[[5]]),'.tif'))), asp=1)

        