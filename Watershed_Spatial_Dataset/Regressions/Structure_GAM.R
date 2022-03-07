library(mgcv)
library(ggplot2)
library(corrplot)
library(terra)
library(tidyverse)
library(sf)
library(visreg)
library(psych)
library(raster)


strdir <- '~/Desktop/tifs'
wsdir <- file.path(getwd(), 'Watershed_Spatial_Dataset', 'Source', fsep = '/')
rasdir <- file.path('/Volumes', 'GoogleDrive', 'My Drive', 'Research', 'RMBL', 'RMBL-East River Watershed Forest Data', 'Data', 'Geospatial', 'Worsham_2021_SiteSelection', '2021_Analysis_Layers', 'USGS_1-9_arcsec_DEM')
sfdir <- file.path('/Volumes/GoogleDrive/My Drive/Research/RMBL/RMBL-East River Watershed Forest Data/Data/Geospatial/RMBL_2020_EastRiver_SDP/RMBL_2020_EastRiver_SDP_Boundary')
geodir <- file.path('/Volumes', 'GoogleDrive', 'My Drive', 'Research', 'RMBL', 'RMBL-East River Watershed Forest Data', 'Data', 'Geospatial', 'Wainwright_2021_Geology')
scondir <- file.path('/Volumes', 'GoogleDrive', 'My Drive', 'Research', 'RMBL', 'RMBL-East River Watershed Forest Data', 'Data', 'Geospatial', 'Uhlemann_2021_RESubsurfaceResistivityMap')

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

density <- raster(file.path(strdir, 'stand_density_100mx.tif'))
height <- raster(file.path(strdir, 'mean_height_100m.tif'))
crs(density)
values(density)
plot(density)

get.rasters <- function(x, dir){
  xpath = file.path(dir, x)
  xtif = list.files(xpath, pattern = 'tif$', full.names = T)
  xras = lapply(xtif, raster)
  return(xras)
}

topos <- flatten(lapply(topo.factors, get.rasters, rasdir))
library(RColorBrewer)
geol <- raster(file.path(geodir, 'NEW_GeolGrid_Final1.tif'))
geolm <- mask(geol, aop)
plot(aop$geometry, col='NA', border='white')

plot(geolm, col=viridis(20))

resist <- read.csv(file.path(scondir, 'AEM_Res_19m_Kriging.csv'))
resist[resist==-999.9] <- NA
sresist <- raster(xmn=min(resist$X), xmx=max(resist$X), ymn=min(resist$Y), ymx=max(resist$Y), res=100, crs="+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs")
sresist <- rasterize(resist[, c('X', 'Y')], sresist, resist[, 'Res'], fun=mean)

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

density <- cropfun(density, aop)

explainers <- topos
explainers <- sapply(explainers, cropfun, aop)
explainers <- sapply(explainers, alignfun, density)

geol  <- cropfun(geol, aop)
geol <- alignfun(geol, density, 'ngb')
explainers[[8]] <- geol
explainers[[9]] <- sresist

density <- values(density)
elevation <- values(explainers[[3]])
aspect <- values(explainers[[1]])
aspect <- sin(aspect*0.0174533)
slope <- values(explainers[[4]])
tpi <- values(explainers[[5]])
twi <- values(explainers[[7]])
geol <- values(explainers[[8]])
sresist <- values(explainers[[9]])

vars <- data.frame(
  density,
  elevation,
  aspect,
  slope,
  tpi,
  twi,
  geol
)
#hist(vars$density, c='navy', breaks=16, border='white', main='Stand Density Frequency Distribution, 100m pixel')
vars <- vars[vars$density > 100,]
vars <- data.frame(scale(vars))

vars$geol <- as.factor(vars$geol)
gdc <- dummy.code(vars$geol)

vars <- na.omit(vars)
varcorr <- cor(vars)
corrplot(varcorr, method='number', type = 'upper',   tl.col = "black", diag=F, tl.pos='td')

pairs.panels(varcorr,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "pearson", # Correlation method (also "spearman" or "kendall")
             pch = '.',           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

mod_lm <- gam(density ~elevation+slope+aspect+tpi+twi+geol, data=vars)
mod_gam1 <- gam(density ~ s(slope, bs='cr'), data=vars)

mod_gam2 <- gam(density ~ s(elevation) + s(aspect) + s(slope) + s(tpi) + s(twi) + s(geol))

visreg(mod_gam2)
summary(mod_lm)
summary(mod_gam2)

AIC(mod_lm)
AIC(mod_gam1)

summary(mod_lm)$sp.criterion
summary(mod_gam2)$sp.criterion
visreg2d(mod_gam2, xvar='elevation', yvar='twi', phi=30, theta=30, n.grid=500, border=NA))

vis.gam(mod_gam2, type='response', plot.type='persp', phi=30, theta=30, border=NA)

summary(mod_gam2)
summary(mod_lm)$r.sq
summary(mod_gam1)$r.sq

anova(mod_lm, mod_gam1, test="Chisq")

install.packages('visibly')


?corrplot
