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


datadir <- file.path('/Volumes', 'GoogleDrive', 'My Drive', 'Research', 'RMBL', 'RMBL-East River Watershed Forest Data', 'Data')
strdir <- file.path(datadir, 'LiDAR', 'tifs')
wsdir <- file.path(getwd(), 'Watershed_Spatial_Dataset', 'Source', fsep = '/')
rasdir <- file.path(datadir, 'Geospatial', 'Worsham_SiteSelection', '2021_Analysis_Layers', 'USGS_1-9_arcsec_DEM')
sfdir <- file.path(datadir, 'Geospatial', 'RMBL_2020_EastRiver_SDP', 'RMBL_2020_EastRiver_SDP_Boundary')
geodir <- file.path(datadir, 'Geospatial', 'Wainwright_2021_Geology')
scondir <- file.path(datadir, 'Geospatial', 'Uhlemann_2021_RESubsurfaceResistivityMap')
soildir <- file.path('~/Downloads/gSSURGO_CO')

aop <- st_read(file.path(sfdir, 'SDP_Boundary.shp'))

topo.factors <- c('Aspect', 
                  'Curvature',
                  'DTM',
                  'DEM',
                  #'Flow_Accumulation',
                  'Slope', 
                  #'Solar_Radiation', 
                  'TPI', 
                  'TWI'
)

dnsty <- raster(file.path(strdir, 'stand_density_100mx.tif'))
height <- raster(file.path(strdir, 'height_90pctl.tif'))
diam <- raster(file.path(strdir, 'mean_diam_100m.tif'))
crs(dnsty)
values(dnsty)
plot(dnsty)

get.rasters <- function(x, dir){
  xpath = file.path(dir, x)
  xtif = list.files(xpath, pattern = 'tif$', full.names = T)
  xras = lapply(xtif, raster)
  return(xras)
}

topos <- flatten(lapply(topo.factors, get.rasters, rasdir))

geol <- raster(file.path(geodir, 'EastRiver_GeolGrid.tif'))
geol <- projectRaster(geol, crs=crs(aop))
geolm <- mask(geol, aop)

plot(aop$geometry, col='NA', border='black', add=T)
plot(geolm, col=viridis(20), add=T)

soil <- soils.ras
plot(soil)

# resist <- read.csv(file.path(scondir, 'AEM_Res_19m_Kriging.csv'))
# resist[resist==-999.9] <- NA
# sresist <- raster(xmn=min(resist$X), xmx=max(resist$X), ymn=min(resist$Y), ymx=max(resist$Y), res=100, crs="+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs")
# sresist <- rasterize(resist[, c('X', 'Y')], sresist, resist[, 'Res'], fun=mean)

cropfun <- function(ras, shp){
  ras <- crop(ras, extent(shp))
  ras <- mask(ras, shp)
  return(ras)
}

alignfun <- function(x, target, method='bilinear'){
  xnew = resample(x, target, method)
  ex = extent(target)
  xnew = crop(xnew, ex)
  return(xnew)
}

dnsty <- cropfun(dnsty, aop)
diam <- cropfun(diam, aop)
height <- cropfun(height, aop)

explainers <- topos
explainers <- sapply(explainers, cropfun, aop)
explainers <- sapply(explainers, alignfun, diam)

length(explainers)

geol  <- cropfun(geol, aop)
geol <- alignfun(geol, diam, 'ngb')

soil <- raster(soil)
soil <- cropfun(soil, aop)
soil <- alignfun(soil, diam, 'ngb')
explainers[[15]] <- geol
explainers[[16]] <- soil

dnsty <- values(dnsty)
diam <- values(diam)
height <- values(height)

elevation <- values(explainers[[8]])
aspect <- values(explainers[[2]])
#aspect <- cos(aspect*0.0174533)
slope <- values(explainers[[9]])
tpi <- values(explainers[[11]])
twi <- values(explainers[[13]])
geol <- values(explainers[[15]])
soil <- values(explainers[[16]])

#sresist <- values(explainers[[9]])

vars <- data.frame(
  dnsty,
  elevation,
  aspect,
  #slope,
  tpi,
  #twi,
 geol, 
 soil
)

m <- mean(elevation, na.rm=T)
s <- sd(elevation, na.rm=T)

el.rescaled <- vars$elevation*s + m

#hist(vars$density, c='navy', breaks=16, border='white', main='Stand Density Frequency Distribution, 100m pixel')
vars$geol <- as.factor(vars$geol)
vars$soil <- as.factor(vars$soil)
#geol2 <- as.integer(geol)
#geol2 <- as.factor(geol2)

#gdc <- data.frame(dummy.code(geol2))

#vars <- cbind(vars, gdc, deparse.level = 0)
#vars <- vars[vars$height > 1,]

vars2 <- data.frame(scale(vars[1:4]))
#vars <- cbind(vars2, vars[8:21])
vars <- cbind(vars2, vars[c(5,6)])


corvars <- vars[2:6]
corvars <- na.omit(corvars)

varcorr <- cor(corvars)

corrplot(varcorr, method='number', type = 'upper', tl.col = "black", diag=F, tl.pos='td')

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

# mod_lm <- lm(diam ~ elevation+slope+aspect+tpi+twi, data=vars)
# 
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

mod_gam1 <- gam(dnsty ~ s(slope, bs='cr'), data=vars)

mod_gam2 <- gam(height ~ 
                  s(elevation, bs='cc') + 
                  s(aspect, bs='cc') + 
                  #s(slope, bs='cc') + 
                  tpi +
                  #twi +
                  s(elevation, by=aspect) +
                  s(elevation, by = tpi) +
                  s(aspect, by = tpi) +
                  geol +
                  soil,
                data=vars)

plot(mod_gam2)
visreg(mod_gam2)
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

anova(mod_lm, mod_gam1, test="Chisq")


install.packages('lme4')
library(lme4)

mod_lme <- lmer(dnsty ~ elevation+slope+aspect+tpi+twi+(1|geol), data=vars)
summary(mod_lme)
confint(mod_lme)
ranef(mod_lme)$geol
coef(mod_lme)$geol
plot(vars$geol, vars$dnsty)

