# Process SSURGO soil data

#############################
# Set up working environment
#############################

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

# Configure drive auth
drive_auth(path=config$drivesa)
options(googledrive_quiet=T)

# Set number of cores
nCores <- as.integer(availableCores()-2)

# Define directories
ssurgodir <- drive_get(as_id('17rNzAu2V3Sdp9Wo-LFUd3UzYGma6FMrA'))
outdir <- file.path(config$extdata$scratch, 'SSURGO', 'processed')

# Cleaning function
not_all_na <- function(x) any(!is.na(x)) #for data cleaning

#####################
# Ingest data
#####################

# Ingest AOP survey area polygon
aop <- load.plot.sf(path=as_id(config$extdata$bndid),
                    pattern=config$extdata$bndpattern)
aop <- as_Spatial(aop)

# Ingest area mask
full.mask <- rast(file.path(config$extdata$scratch, 'tifs', 'fullmask_5m.tif'))
full.mask <- classify(full.mask, rcl=matrix(c(1,NA, NA, 1), ncol=2))

# Ingest SSURGO data
# Data for a soil survey area includes a tabular component and a spatial component.
# The tabular component is typically imported into a database for querying,
# reporting and analysis. The spatial component is typically viewed and analyzed
# using a Geographic Information System (GIS).

SSURGO.areas.er <- get_ssurgo(template=aop,
                              label='CO_EastRiver',
                              raw.dir=file.path(config$extdata$scratch, 'SSURGO', 'raw'),
                              extraction.dir=file.path(config$extdata$scratch, 'SSURGO', 'extractions'))

# This returns a list whose elements are:
# 1. `spatial`: sf object containing map unit level information
# 2.`tabular`: list of data.frames containing map unit, component, and horizon data

# Check all areas ended up in set
all(unique(SSURGO.areas.er$spatial$AREASYMBOL)==c('CO654', 'CO660', 'CO661', 'CO662'))

#######################################
# EDA: Dig into data layers
#######################################

# Map units are typically made up of one or more named soils. Other miscellaneous
# land types or areas of water may be included. These entities and their percent
# compositions make up the map unit components and define the map unit composition.
# Soil components are typically composed of multiple horizons (layers).
# Component attributes must be aggregated to a map unit level for map visualization.
# Horizon attributes must be aggregated to the component level, before components
# are aggregated to the map unit level. Horizon attributes may be aggregated for
# the entire soil profile or for a specific depth range. One may only be
# interested in the value of a horizon attribute for the surface layer.

# In this dataset, mukey gives a unique identifier to each mapping unit.
# The maps outline areas called map units. The map units describe soils and other
# components that have unique properties, interpretations, and productivity.
# More than one polygon may be associated with a map unit.
# There should be ONLY ONE unique musym (map unit SYMBOL, a short form of
# map unit NAME) per map unit!

# Assign spatial layer to variable
SSURGO.sf <- SSURGO.areas.er$spatial
names(SSURGO.sf) <- c('areasymbol', 'spatialver', 'musym', 'mukey', 'geom')

# Set CRS to that of AOP
SSURGO.sf <- st_transform(SSURGO.sf, st_crs(aop))

# Plot map units
ggplot() +
  geom_sf(data=SSURGO.sf, aes(fill=areasymbol), col='grey90') +
  scale_fill_viridis(option='C', discrete=T) +
  geom_sf(data=st_as_sf(aop), col='black', fill=NA, inherit.aes=F) # by soil survey area ('areasymbol')

ggplot() +
  geom_sf(data=SSURGO.sf, aes(fill=mukey)) +
  scale_fill_viridis(option='mako', discrete=T) +
  geom_sf(data=st_as_sf(aop), col='black', fill=NA, inherit.aes=F) # by mukey

#######################################
# Spatial data
#######################################

# Check length of musym and mukey in spatial data are equal
length(SSURGO.sf$mukey)==length(SSURGO.sf$musym)
nrow(SSURGO.sf)
any(is.na(SSURGO.sf$mukey))
any(is.na(SSURGO.sf$musym))
# There are the same total number of musyms and mukeys
# None are NA

# Summarize number of polygons per musym
SSURGO.sf %>%
  group_by(musym) %>%
  summarise(n=n())
# Some musym

# Check number of unique musyms and muykeys in spatial data
assertthat::are_equal(length(unique(SSURGO.sf$musym)), length(unique(SSURGO.sf$mukey)))
length(unique(SSURGO.sf$musym))
length(unique(SSURGO.sf$mukey))
# There are 85 unique musyms and 91 unique mukeys, which means more than one
#  mukey can be associated with a single musym

# Summarize number of polygons per musym and mukey
mukeys.by.musym <- SSURGO.sf %>%
  group_by(musym) %>%
  summarise(n=length(unique(mukey)))
# A few musyms are associated with more than one map unit (mukey)
# This is ok, but there shouldn't be more than one musym associated with
# any one mukey

# Map unit symbols attached to multiple mukeys:
multi.mukeys <- SSURGO.sf[SSURGO.sf$musym %in% mukeys.by.musym[mukeys.by.musym$n>1,]$musym,]
multi.mukeys.munames <- multi.mukeys %>%
  left_join(SSURGO.areas.er$tabular$mapunit, by='musym')

# Are there any mukeys that have multiple musyms?
musyms.by.mukey <- SSURGO.sf %>%
  group_by(mukey) %>%
  summarise(n=length(unique(musym)))
unique(musyms.by.mukey$n)
# No. That's good.

# But note, again, that there are some mukeys representing multiple polygons,
# which may or may not be contiguous, e.g.:
plot(musyms.by.mukey[musyms.by.mukey$mukey=='3084845', 'mukey'])

################################################
# Tabular data: MAPUNITS, COMPONENTS, HORIZONS
################################################

# The field “mukey” uniquely identifies a map unit.
# The field “cokey” uniquely identifies a map unit component.
# The field "chkey" uniquely identifies a horizon record.

# Create horizon, component, and mapunit dataframes for later merge
mu <- SSURGO.areas.er$tabular$mapunit
comp <- SSURGO.areas.er$tabular$component
horz <- SSURGO.areas.er$tabular$chorizon

# MAPUNITS

# Check number of unique mukeys in tabular data
length(unique(mu$mukey))
length(mu$mukey)==length(unique(SSURGO.sf$mukey)) # matches number of mukeys in spatial data
length(unique(mu$mukey))==length(unique(comp$mukey)) # matches number of mukeys in component data

# COMPONENTS

# Maps are linked in the database to information about the component soils
# and their properties for each map unit. Each map unit may contain one to
# three major components and some minor components. The map units are typically
# named for the major components.

# Components record identifiers:
#  - mukey: map unit identifier
#  - cokey: component identifier

# Check number of cokeys in component data
length(comp$cokey) == length(unique(comp$cokey)) # All cokeys in comp are unique

# Check number of cokeys per mapunit
cokeys.by.mukey <- comp %>%
  group_by(mukey) %>%
  summarise(n=n())
# Some map units have more than one component; range is 1 to 6 cokeys per mukey
# NOTE: this means that if we eventually want to summarize comp data to mu
# level, we'll need to weight components by % occupancy in aggregation step

# HORIZONS

# In most cases, component records are linked to more granular, depth explicit
# horizon-scale information. Horizon attributes must be aggregated to component
# level before components can be aggregated to the map unit level. Horizon
# attributes may be aggregated for an entire soil profile or for a specific
# depth range.

# It seems to be the case that the detailed soil properties I'm interested in
# exist only in the horizon-level dataframes; where horizon information is not
# available, the soil characteristics are not available. Need to confirm this.

# Horizon record identifiers:
#  - cokey: component identifier
#  - chkey: component-horizon identifier
#  - hzname: concatenated string of four kinds of symbols (five data elements) used to distinguish different kinds of layers in the soil

# Check number of unique chkeys in horizon data
length(unique(horz$chkey)) == length(horz$chkey) # All chkeys are unique

# Check number of unique horizon names
length(unique(horz$hzname)) # There are only 50 horizon names

# Check number of unique cokeys in horizon data
length(unique(horz$cokey)) # cokeys aren't equal (274 - 117 = 157)

# 157 components lack horizon-level data.
# Does this mean they don't have soil characteristic data at all? Or that
# it's not disaggregated to horizon level?

# Examine components that lack horizon data
no.horz <- comp[which(!comp$cokey %in% horz$cokey),]

no.horz %>%
  group_by(majcompflag) %>%
  summarise(n=n())
# It looks like most of the components lacking horizon data are non-major
# components (20 major vs 137 non major), and that the vast majority of map
# units have at least one major component with horizon data

# Are there any map units that lack horizon data ENTIRELY?
comp.horz <- left_join(comp, horz, by='cokey') %>%
  dplyr::select(c(cokey, compname, comppct.r, majcompflag, mukey,
                  chkey, hzname, hzdept.r)) %>%
  mutate(mukey=as.character(mukey))

comp.horz.nohorz <- comp.horz %>%
  group_by(mukey) %>%
  summarise(nohorz=all(is.na(chkey))) %>%
  filter(nohorz==T)

summary(comp.horz.nohorz$nohorz)
# Yes. 12 map units have no horizon information at all

comp.horz.mu <- left_join(SSURGO.sf, comp.horz.nohorz, by='mukey')
comp.horz.chars <- comp.horz[comp.horz$mukey %in% comp.horz.nohorz$mukey,]
plot(comp.horz.mu['nohorz'])
# These all appear to be rock (i.e. mountaintops or talus), water, or aquolls
# in perennially wet zones
# Interpretation: it's ok not to have horizon data for these map units in the
# final data product!

# Identify empty columns
horz.empty <- which(apply(horz, 2, not_all_na)==F)
comp.empty <- which(apply(comp, 2, not_all_na)==F)
mu.empty <- which(apply(mu, 2, not_all_na)==F)

# Remove empty columns
horz <- horz %>% select_if(not_all_na) # 171 to 131 variables
comp <- comp %>% select_if(not_all_na) # 109 to 77 variables
mu <- mu %>% select_if(not_all_na) # 24 to 9 variables

################################################
# JOINS
################################################

# HORIZONS

mc <- left_join(mu, comp, by='mukey')
mch <- left_join(mc, horz, by='cokey')
mch$mukey <- as.character(mch$mukey)
mch.sf <- left_join(mch, SSURGO.sf, by='mukey')

mch %>%
  group_by(mukey) %>%
  summarise(n=n())

# Plot horizon depths
mch.sf.sub <- mch.sf %>%
  group_by(mukey, cokey) %>%
  distinct(hzname, .keep_all=T) %>%
  arrange(hzdepb.r) %>%
  mutate(label_y=cumsum(hzdepb.r),
         label_x=paste0(cokey, '&', musym.x)) %>%
  ungroup()

ggplot(mch.sf.sub, aes(x=label_x, y=hzdepb.r, fill=hzdepb.r, label=hzname)) +
  geom_bar(stat='identity', position='stack', color='black') +
  geom_text(aes(y=label_y, label = hzname), vjust=-0.25, colour = "black", size=1.8) +
  scale_fill_gradient(low='sienna4', high='white', name='Horizon Depth (cm)') +
  guides(x = ggh4x::guide_axis_nested(delim = "&")) +
  ylab('Horizon Depth (cm)') +
  xlab('Component ID') +
  theme(axis.text.x = element_text(angle = 60, hjust=1, vjust=1)) +
  scale_y_reverse() +
  facet_wrap(~areasymbol, scales='free')

# Determine total soil depth per component
depth <- horz %>%
  group_by(cokey) %>%
  summarise(total.depth = max(hzdepb.r))

# Filter out horizons below 100cm
horz.100 <- horz %>%
  filter(hzdept.r <= 100) %>%
  droplevels() %>%
  mutate(thick = ifelse(hzdepb.r > 100, 100 - hzdept.r,
                        hzdepb.r - hzdept.r)) %>%
  group_by(cokey) %>%
  mutate(total.depth=sum(thick),
         thick.pct=thick/total.depth) %>%
  ungroup()

# Filter out horizons below 30cm for OM
horz.30 <- horz %>%
  filter(hzdept.r <= 30) %>%
  droplevels() %>%
  mutate(thick = ifelse(hzdepb.r > 30, 30 - hzdept.r,
                        hzdepb.r - hzdept.r)) %>%
  group_by(cokey) %>%
  mutate(total.depth=sum(thick),
         thick.pct=thick/total.depth) %>%
  ungroup()

# We want only one observation per cokey to join horz data to component data
# horz.per.comp <- horz.100 %>%
#   group_by(cokey) %>%
#   summarize(count = n()) %>%
#   filter(count > 1)

# Summarize characteristics of interest with a weighted mean of included horizons
horz.vars.100 <- horz.100 %>%
  group_by(cokey) %>%
  summarise(awc = round(weighted.mean(awc.r, thick.pct, na.rm=T), 2),
            sand = round(weighted.mean(sandtotal.r, thick.pct, na.rm=T),2),
            silt = round(weighted.mean(silttotal.r, thick.pct, na.rm=T),2),
            clay = round(weighted.mean(claytotal.r, thick.pct, na.rm=T),2),
            ksat = round(weighted.mean(ksat.r, thick.pct, na.rm=T),2),
            # k = round(weighted.mean(kffact, thick.pct, na.rm=T),2),
            cec = round(weighted.mean(cec7.r, thick.pct, na.rm=T),2),
            ph = round(weighted.mean(ph1to1h2o.r, thick.pct, na.rm=TRUE),2))

horz.vars.30 <- horz.30 %>%
  group_by(cokey) %>%
  summarise(om = round(weighted.mean(om.r, thick.pct, na.rm=T),2))

comp.pm <- SSURGO.areas.er$tabular$copmgrp %>%
  dplyr::select(c(pmgroupname, cokey))

horz.vars <- full_join(horz.vars.100, horz.vars.30, by='cokey') %>%
  full_join(comp.pm, by='cokey')

# Join computed weighted means with deepest soil depth
horz.vars <- horz.vars %>%
  left_join(depth, by = "cokey")

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
                  taxpartsize, mukey, cokey
                  ))

# Join components and horizons
comp.horz <- left_join(comp, horz.vars, by=c('cokey'))
dim(comp.horz)
head(comp.horz)
names(comp.horz)

comp.horz.completeness <- comp.horz %>%
  group_by(mukey) %>%
  summarise(across(awc:total.depth, ~(sum(!is.na(.)))))

# Pull a test component
test <- comp.horz %>%
  filter(cokey == "23737111") #cokey full of NAs, noted from earlier slide

###########
# Mapunits
###########

# Get rid of mapunit columns we don't need
colnames(mu)
mu <- mu %>%
  dplyr::select(c(musym, muname, muacres, mukey))

# Join mapunits and component-horizon data
full.soil <- left_join(comp.horz, mu, by = c('mukey'))
dim(full.soil)

# Summarize variable of interest by component-percent weighted mean
# That is, for any variable of interest, assign a value to each map unit that is a mean value weighted by the % of the component occupying the map unit
full.soil.wm <- full.soil %>%
  group_by(mukey) %>%
  summarise(
    awc_wm = round(weighted.mean(awc, comppct.r, na.rm=T), 2),
    sand_wm = round(weighted.mean(sand, comppct.r, na.rm=T),2),
    silt_wm = round(weighted.mean(silt, comppct.r, na.rm=T),2),
    clay_wm = round(weighted.mean(clay, comppct.r, na.rm=T),2),
    om_wm = round(weighted.mean(om, comppct.r, na.rm=T),2),
    ksat_wm = round(weighted.mean(ksat, comppct.r, na.rm=T),2),
    cec_wm = round(weighted.mean(cec, comppct.r, na.rm=T),2),
    ph_wm = round(weighted.mean(ph, comppct.r, na.rm=TRUE),2),
    td_wm = round(max(total.depth, comppct.r, na.rm=TRUE), 2))

# Join weighted-mean component values to mapunits
#names(SSURGO.areas.er$spatial) <- c('areasymbol', 'spatialver', 'musym', 'mukey')
soil.spatial <- merge(SSURGO.sf, full.soil.wm, by=c('mukey'))

# Plot
soil.spatial.smooth %>%
  pivot_longer(cols=awc_wm:td_wm,
               names_to='var',
               values_to='est') %>%
  # ggplot() +
  # geom_sf(aes(fill=est)) +
  # scale_fill_viridis(option='F') +
  # theme_minimal() +
  # coord_sf() +
  # facet_wrap(~var)
  split(.$var) %>%
  map(~ ggplot(., aes(fill = est)) +
        geom_sf() +
        facet_wrap(~var) +
        scale_fill_viridis_c() +
        theme_linedraw()) %>%
  cowplot::plot_grid(plotlist = .)

# Smooth neighboring values
tch <- st_touches(soil.spatial, soil.spatial)

soil.spatial.smooth <- lapply(1:nrow(soil.spatial), \(x) {
  ngbs <- unlist(tch[x])
  wts <- rep(0.5/length(ngbs), length(ngbs))
  sfobs <- soil.spatial[c(x, ngbs),] %>%
    summarise(across(awc_wm:td_wm, \(j) weighted.mean(j, w=c(0.5, wts), na.rm=T)))
  sfobs
})

soil.spatial.smooth <- do.call('rbind', soil.spatial.smooth)

# Function to rasterize mapunit--level estimates
rasterize.fun <- function(in.sf, ncol, nrow, reso, target.sf) {
  ra = rast(ncol=ncol, nrow=nrow)
  ext(ra) = ext(target.sf)
  res(ra) = reso
  ra.ls = list()
  for(i in seq(1, length(in.sf)-1)){
    ra.ls[[i]] = rasterize(in.sf[i], ra, names(in.sf[i])[1])
  }
  ra.ls = ra.ls[1:(length(in.sf)-1)]
  for(i in seq_along(ra.ls)){
    names(ra.ls[[i]]) <- names(in.sf)[i]
    crs(ra.ls[[i]]) <- 'EPSG:32613'
  }
  return(ra.ls)
}

# Rasterize mapunit--level estimates
soil.rast.smooth.ls <- rasterize.fun(soil.spatial.smooth, ncol=2370, nrow=2080, reso=100, aop)
plot(soil.rast.smooth.ls[[1]], col=viridis(40))
plot(full.mask, col=c('white', NA), add=T)

# Plot mapunit--level estimates
par(mfcol=c(3,3), mar=c(rep(1,2), rep(3,2)))
for(i in 1:length(soil.rast.smooth.ls)) {
  plot(soil.rast.smooth.ls[[i]],
       col=viridis(40),
       asp=1,
       main=names(soil.rast.smooth.ls[[i]]))
  plot(full.mask, col=c('white', NA), add=T)
}

#######################
# Write rasters out
#######################
# Write out
lapply(soil.rast.smooth.ls, function(x) {
  writeRaster(
    x,
    file.path(outdir, paste0('ssurgo_', names(x), '.tif')),
    overwrite=T)
})

# Check validity of saved tif
dev.off()
plot(rast(file.path(outdir, 'ssurgo_om_wm.tif')), col=viridis(40), asp=1)
