# Script for fitting GAM to forest structure & explanatory data

# Load libraries
library(mgcv)
library(ggplot2)
library(corrplot)
library(terra)
library(tidyverse)
library(sf)
library(visreg)
library(psych)
library(raster)
library(RColorBrewer)
library(viridis)
library(lme4)

#############################
# Set up working environment
#############################

# Define directories
datadir <- file.path('/Volumes', 'GoogleDrive', 'My Drive', 'Research', 'RMBL', 'RMBL-East River Watershed Forest Data', 'Data')
strdir <- file.path(datadir, 'LiDAR', 'tifs')
wsdir <- file.path(getwd(), 'Watershed_Spatial_Dataset', 'Source', fsep = '/')
rasdir <- file.path(datadir, 'Geospatial', 'Worsham_SiteSelection', '2021_Analysis_Layers', 'USGS_1-9_arcsec_DEM')
sfdir <- file.path(datadir, 'Geospatial', 'RMBL_2020_EastRiver_SDP', 'RMBL_2020_EastRiver_SDP_Boundary')
geodir <- file.path(datadir, 'Geospatial', 'Colorado_Geological_Survey')
scondir <- file.path(datadir, 'Geospatial', 'Uhlemann_2021_RESubsurfaceResistivityMap')
soildir <- file.path('~/Downloads/gSSURGO_CO')
snowdir <- file.path(datadir, 'Geospatial', 'ASO_Snow', 'Processed')
list.files(strdir)

#############################
# Ingest data
#############################

# Function to ingest rasters
get.rasters <- function(x, dir){
  xpath = file.path(dir, x)
  xtif = list.files(xpath, pattern = 'tif$', full.names = T)
  xras = lapply(xtif, raster)
  return(xras)
}

#### AOP ####
# Ingest AOP survey area polygon
aop <- st_read(file.path(sfdir, 'SDP_Boundary.shp'))

#### Forest structure ####
# Ingest forest structure rasters
dnsty <- raster(file.path(strdir, 'stand_density_100mx.tif'))
height <- raster(file.path(strdir, 'height_90pctl.tif'))
diam <- raster(file.path(strdir, 'mean_diam_100m.tif'))
response <- list('density'=dnsty, 'height'=height, 'diam'=diam)

# Check forest structure rasters
crs(dnsty)
values(dnsty)
plot(dnsty, col=viridis(9))

#### Topographic variables ####
# Define topographic factors to ingest
topo.factors <- c('Aspect', 
                  'Curvature',
                  'DTM',
                  'DEM',
                  'Slope', 
                  'TPI', 
                  'TWI', 
                  'Heat_Load'
                  #'Flow_Accumulation',
                  #'Solar_Radiation'
                  )

# Ingest topos
topos <- flatten(lapply(topo.factors, get.rasters, rasdir))

#### Geology ####
# Ingest geology, project to AOP CRS and mask to AOP boundary
geol <- raster(file.path(geodir, 'Processed', 'COGS_eastriver_geology.tif'))
geol <- projectRaster(geol, crs=crs(aop))

#### SSURGO soil ####
#soil <- soils.ras
#plot(soil)
soil <- soil.rast.ls

#### ASO snow ####
snow <- raster(file.path(snowdir, 'mean_swe_18-22.tif'))
snow.delta <- raster(file.path(snowdir, 'mean_delta_swe.tif'))

#### Soil resistivity ####
# resist <- read.csv(file.path(scondir, 'AEM_Res_19m_Kriging.csv'))
# resist[resist==-999.9] <- NA
# sresist <- raster(xmn=min(resist$X), xmx=max(resist$X), ymn=min(resist$Y), ymx=max(resist$Y), res=100, crs="+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs")
# sresist <- rasterize(resist[, c('X', 'Y')], sresist, resist[, 'Res'], fun=mean)

# Append all explainers to one list
explainers <- append(topos, c(geol, soil, snow, snow.delta))
sapply(explainers, names)
length(explainers) == 28

# Clean up names of explainers
varnames <- c('adj_southness_205',
              'folded_aspect_205',
              'aspect',
              'aspect_10m',
              'cos_aspect',
              'sin_aspect',
              'curvature',
              'elevation', 
              'slope',
              'slope_10m',
              'tpi_1km',
              'tpi_2km',
              'twi_100m',
              'twi_1km',
              'heat_load',
              'geology',
              'awc',
              'sand',
              'silt',
              'clay',
              'om',
              'ksat',
              'k',
              'cec',
              'ph',
              'td',
              'swe',
              'delta_swe'
              )

for(i in seq_along(explainers)){
  names(explainers[[i]]) <- varnames[i]
}

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

# Crop forest structure variables to AOP
response <- lapply(response, cropfun, aop)
#dnsty <- cropfun(dnsty, aop)
#diam <- cropfun(diam, aop)
#height <- cropfun(height, aop)

# Crop explainers to AOP and align to response
explainers <- lapply(explainers, cropfun, aop)
explainers <- lapply(explainers, alignfun, response[[1]], 'ngb')

par(mfcol=c(5,6), mar=rep(1,4))
for(i in seq_along(explainers)) {
  opt <- rep(LETTERS[1:6],5)
  plot(explainers[[i]], col=viridis(10, option=opt[i]), main=names(explainers[[i]]), asp=1)
}

#############################
# Extract values for modeling
#############################
# Function to extract values from rasters
getvals <- function(ras) return(values(ras))

# Extract forest structure values
re.vals <- lapply(response, getvals)

# Histogram of structure data
par(mfcol=c(1, length(re.vals)), mar=rep(3,4))
for(i in seq_along(re.vals)){
  hist(re.vals[[i]], 
       c='grey50', 
       breaks=20, 
       border='white', 
       main=str_to_upper(names(re.vals)[i]),
       sub='100m pixel',
       xlab=names(re.vals)[i],
       ylab='count')
}

# Define variables we want to use in model
#target.vars <- c('folded_aspect_205', 'swe')
target.vars <- c('folded_aspect_205',
                #'curvature',
                'elevation', 
                'slope',
                'tpi_1km',
                'twi_100m',
                'heat_load',
                'geology',
                'awc',
                #'sand',
                #'silt',
                'om',
                'k',
                #'cec',
                'td',
                'swe',
                'delta_swe')

# Isolate the variables selected in the list above
sapply(explainers, names)
explainers.sub <- explainers[sapply(explainers, names) %in% target.vars]

par(mfcol=c(5,3), mar=rep(2,4))
lapply(explainers.sub, plot, col=viridis(12, option='C'), main=names(explainers.sub), asp=1)

# Extract values from explainers
ex.vals <- sapply(explainers.sub, getvals)

# Assemble dataframe for modeling
vars <- data.frame(re.vals, ex.vals)
names(vars) <- c(names(re.vals), target.vars)
vars <- vars[vars$density>100,]
vars <- vars[!is.na(vars$density),]

#############################
# Rescale variables
#############################

m <- mean(elevation, na.rm=T)
s <- sd(elevation, na.rm=T)

el.rescaled <- vars$elevation*s + m

vars$geology <- as.factor(vars$geology)

#vars$soil <- as.factor(vars$soil)

#geol2 <- as.integer(geol)
#geol2 <- as.factor(geol2)

#gdc <- data.frame(dummy.code(geol2))

#vars <- cbind(vars, gdc, deparse.level = 0)
#vars <- vars[vars$height > 1,]
vars2 <- vars[!names(vars) %in% 'geology']
vars2 <- data.frame(scale(vars2))
#vars <- cbind(vars2, vars[8:21])
vars <- cbind(vars2, vars[names(vars) %in% 'geology'])

#############################
# Correlation matrix
#############################
corvars <- vars2[4:length(vars2)]
corvars <- na.omit(corvars)
varcorr <- cor(corvars)
par(mfcol=c(1,1))
corrplot(varcorr, method='number', type = 'upper', bg= 'grey30', tl.col = "black", diag=F, tl.pos='td')

pairs.panels(varcorr,
             smooth = F,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "kendall", # Correlation method (also "spearman" or "kendall")
             pch = '.',           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

#############################
# Simple linear model
#############################
mod_lm <- lm(diam ~ folded_aspect_205+swe, data=vars)

mod_lm <- lm(diam ~ elevation+slope+folded_aspect_205+tpi_1km+twi_100m+awc+om+k+geology, data=vars)
summary(mod_lm)
plot(mod_lm)
# mod_lmm <- lm(height ~
#                 elevation +
#                 slope + 
#                 aspect + 
#                 tpi +
#                 twi + 
#                 tpi*elevation + 
#                 aspect*elevation +
#                 slope*elevation + 
#                 twi*elevation +
#                 slope*aspect + 
#                 geol
#                 # X20 +
#                 # X21 +
#                 # X33 +
#                 # X34 +
#                 # X28 +
#                 # X22 +
#                 # X25 +
#                 # X31 +
#                 # X35 +
#                 # X20 +
#                 # X32 +
#                 # X26 +
#                 # X30 +
#                 # X27
#                 , data=vars)
# 
# summary(mod_lmm)

#############################
# Generalized additive model
#############################

# GAM 1 - couple of explainers
mod_gam1 <- gam(density ~ s(swe, bs='cc') + s(folded_aspect_205, bs='cs'), data=vars)
summary(mod_gam1)
par(mfcol=c(2,1), mar=rep(4,4))
visreg(mod_gam1)

vis.gam(mod_gam1, 
        view=c('swe','folded_aspect_205'), 
        type='response', 
        plot.type='persp', 
        phi=10, 
        theta=24, 
        border=NA, 
        color='heat', 
        zlab='stem density')

# GAM 2 - many explainers
mod_gam2 <- gam(density ~ 
                  s(elevation, bs='cc') + 
                  s(folded_aspect_205, bs='cc') + 
                  s(slope, bs='cc') + 
                  tpi_1km +
                  twi_100m +
                  heat_load +
                  s(elevation, by=folded_aspect_205) +
                  s(elevation, by = tpi_1km) +
                  s(folded_aspect_205, by = tpi_1km) +
                  geology +
                  awc + 
                  om +
                  k + 
                  #cec + 
                  td + 
                  swe + 
                  delta_swe,
                data=vars)

plot(mod_gam2)
#visreg(mod_gam2)
plot(mod_gam2, page = 1, scheme = 2)

summary(mod_gam2)
termplot(mod_gam2, all.terms=T)

AIC(mod_lm)
AIC(mod_gam2)

summary(mod_lm)$sp.criterion
summary(mod_gam2)$sp.criterion

visreg2d(mod_gam2, xvar='elevation', yvar='aspect', phi=30, theta=30, n.grid=500, border=NA)

vis.gam(mod_gam2, view=c('elevation','tpi'), type='response', plot.type='persp', phi=18, theta=48, border=NA, color='gray', zlab='90th pctl height')

summary(mod_gam2)
summary(mod_lm)$r.sq
summary(mod_gam1)$r.sq

#############################
# Linear mixed effects model
#############################

mod_lme <- lmer(density ~ elevation+slope+folded_aspect_205+tpi_1km+twi_100m+(1|geology), data=vars)
summary(mod_lme)
confint(mod_lme)
ranef(mod_lme)$geol
coef(mod_lme)$geol
vars %>%
  ggplot(aes(x=elevation, y=density)) + 
  geom_point(aes(color=geology), alpha=.50) + 
  scale_color_brewer(type='div')

?scale_fill_brewer
