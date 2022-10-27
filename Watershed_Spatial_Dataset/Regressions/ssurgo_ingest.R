# Load libraries
library(FedData)
library(sf)
library(tidyverse)
library(raster)

# Set up working environment
# Define directories
soil.dir <- file.path('/Volumes', 'GoogleDrive', 'My Drive', 'Research', 'RMBL', 'RMBL-East River Watershed Forest Data', 'Data', 'Soil')
ssurgo.dir <- file.path(soil.dir, 'SSURGO')

#sg <- st_read(file.path(ssurgo.dir, 'gSSURGO_CO', 'gSSURGO_CO.gdb'), layer='mupolygon')

# Ingest AOP survey area polygon
aop <- st_read('/Volumes/GoogleDrive/.shortcut-targets-by-id/1HZhH3KecyMfW0wQ8V8iJr7rmCNYOzF7t/RMBL-East River Watershed Forest Data/RMBL 2019/Data/Geospatial/RMBL_2020_EastRiver_SDP/RMBL_2020_EastRiver_SDP_Boundary/SDP_Boundary.shp')
aop <- as_Spatial(aop)

# Ingest SSURGO data
SSURGO.areas.er <- get_ssurgo(template=aop, 
                              label='CO_EastRiver',
                              raw.dir=file.path(ssurgo.dir, 'RAW'), 
                              extraction.dir=file.path(ssurgo.dir, 'EXTRACTIONS', 'CO_EastRiver'))

# Check all areas ended up in set
all(unique(SSURGO.areas.er$spatial$AREASYMBOL)==c('CO654', 'CO660', 'CO661', 'CO662'))

# Cleaning function
not_all_na <- function(x) any(!is.na(x)) #for data cleaning

# Plot to visualize
plot(SSURGO.areas.er$spatial, col=heat.colors(100))

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

# Remove empty columns
horz <- horz %>% select_if(not_all_na) # 171 to 131 variables
comp <- comp %>% select_if(not_all_na) # 109 to 77 variables
mu <- mu %>% select_if(not_all_na) # 24 to 9 variables

#######################
# Horizons
#######################

# Plot horizon depths
horz.sub <- horz[1:160,] %>% 
  group_by(cokey) %>%
  arrange(hzdepb.r) %>% 
  mutate(label_y=cumsum(hzdepb.r)) %>% 
  ungroup()

ggplot(horz.sub, aes(x=as.factor(cokey), y=hzdepb.r, fill=hzdepb.r, label=hzname)) +
  geom_bar(stat='identity', position='stack', color='black') +
  geom_text(aes(y=label_y, label = hzname), vjust=-0.25, colour = "black") +
  scale_fill_gradient(low='sienna4', high='white', name='Horizon Depth (cm)') +
  ylab('Horizon Depth (cm)') + 
  xlab('Component ID') +
  theme(axis.text.x = element_text(angle = 70, hjust=1)) +
  scale_y_reverse()

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

nrow(component_count)
View(horz.100[c('cokey', 'hzdepb.r', 'hzdept.r', 'sandtotal.r')])

# Summarize characteristics of interest with a weighted mean of included horizons
horz.100 <- horz.100 %>%
  mutate(thick = ifelse(hzdepb.r > 100, 100 - hzdept.r, 
                        hzdepb.r - hzdept.r)) %>%  
  group_by(cokey) %>%
  summarise(sand = round(weighted.mean(sandtotal.r, thick, na.rm = TRUE),2),
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
                  slopelenusle.r, runoff, tfact, wei, weg, erocl, 
                  elev.r, albedodry.r, airtempa.r, map.r, ffd.r, 
                  cropprodindex, taxpartsize, mukey, cokey))

# Join components and horizons
comp_horz <- left_join(comp, horz.100, by=c('cokey'))
dim(comp_horz)
head(comp_horz)

test <- comp_horz %>%
  filter(cokey == "23005162") #cokey full of NAs, noted from earlier slide
test$compname

##########
# Mapunits
##########
# Get rid of mapunit columns we don't need
colnames(mu)
mu <- mu %>%
  dplyr::select(c(musym, muname, muacres, mukey))

# Join mapunits and component-horizon data
full_soil <- left_join(comp_horz, mu, by = c('mukey'))
View(full_soil)

# 
names(SSURGO.areas.er$spatial) <- c('areasymbol', 'spatialver', 'musym', 'mukey')
soil_spatial <- merge(SSURGO.areas.er$spatial, full_soil, by=c('mukey'))
head(SSURGO.areas.er$spatial)
