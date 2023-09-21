# Script for preparing datasets for modeling work

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

#############################
# Set up working environment
#############################

# TODO: refactor to use googledrive ingest

# Define directories
datadir <- file.path('/Volumes', 'GoogleDrive', 'My Drive', 'Research', 'RMBL', 'RMBL-East River Watershed Forest Data', 'Data')
strdir <- file.path(datadir, 'LiDAR', 'tifs')
wsdir <- file.path(getwd(), 'Watershed_Spatial_Dataset', 'Source', fsep = '/')
rasdir <- file.path(datadir, 'Geospatial', 'Worsham_SiteSelection', '2021_Analysis_Layers', 'USGS_1-9_arcsec_DEM')
sfdir <- file.path(datadir, 'Geospatial', 'RMBL_2020_EastRiver_SDP', 'RMBL_2020_EastRiver_SDP_Boundary')
geodir <- file.path(datadir, 'Geospatial', 'Colorado_Geological_Survey')
scondir <- file.path(datadir, 'Geospatial', 'Uhlemann_2021_RESubsurfaceResistivityMap')
soildir <- file.path(datadir, 'Soil', 'SSURGO', 'Processed')
snowdir <- file.path(datadir, 'Geospatial', 'ASO_Snow', 'Processed')
bcmdir <- file.path(datadir, 'Geospatial', 'BCM_CO', 'UCRB_BCM')

#############################
# Ingest data
#############################

#### AOP ####
# Ingest AOP survey area polygon
aop <- st_read(file.path(sfdir, 'SDP_Boundary.shp'))

#### Forest structure ####
# Ingest forest structure rasters
dnsty <- raster(file.path(strdir, 'stand_density_100m_fromtrees.tif'))
height <- raster(file.path(strdir, 'height_90pctl.tif'))
diam <- raster(file.path(strdir, 'mean_diam_100m.tif'))
# ba <- raster(file.path(strdir, 'basal_area_100m.tif'))
response <- list(
  'density'=dnsty,
  'height'=height,
  'diam'=diam #,
  # 'ba'=ba
  )

# Check forest structure rasters
lapply(response, crs)
lapply(response, plot, col=viridis(9))

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
#soil <- soil.rast.ls
# list.files(file.path(ssurgo.dir))
soil <- lapply(list.files(soildir, full.names=T, pattern='tif$'), raster)
lapply(soil, plot)

#### ASO snow ####
snow <- raster(file.path(snowdir, 'mean_swe_18-22.tif'))
snow.delta <- raster(file.path(snowdir, 'mean_delta_swe.tif'))

#### BCM ####
bcm.aet <- raster(file.path(bcmdir, 'aet1985to2012wy_meanTotalAnnual_x100i.tif'))
bcm.cwd <- raster(file.path(bcmdir, 'cwd1985to2012wy_meanTotalAnnual_x100i.tif'))

#### Soil resistivity ####
# resist <- read.csv(file.path(scondir, 'AEM_Res_19m_Kriging.csv'))
# resist[resist==-999.9] <- NA
# sresist <- raster(xmn=min(resist$X), xmx=max(resist$X), ymn=min(resist$Y), ymx=max(resist$Y), res=100, crs="+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs")
# sresist <- rasterize(resist[, c('X', 'Y')], sresist, resist[, 'Res'], fun=mean)

# Append all explainers to one list
explainers <- append(topos, c(geol, soil, snow, snow.delta))
sapply(explainers, names)
length(explainers) == 31

# Clean up names of explainers
varnames <- c('adj_southness_205',
              'folded_aspect_205',
              'aspect',
              'aspect_10m',
              'cos_aspect',
              'sin_aspect',
              'curvature',
              'elevation',
              'elevation2',
              'elevation3',
              'elevation4',
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
                'ksat',
                #'cec',
                'td',
                'swe')
                #'delta_swe')

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

vars$ba <- pi*(vars$density/2)^2*(10^-4)

#############################
# Rescale variables
#############################

m <- mean(elevation, na.rm=T)
s <- sd(elevation, na.rm=T)

el.rescaled <- vars$elevation*s + m

#vars$soil <- as.factor(vars$soil)

#geol2 <- as.integer(geol)
#geol2 <- as.factor(geol2)

#gdc <- data.frame(dummy.code(geol2))

#vars <- cbind(vars, gdc, deparse.level = 0)
#vars <- vars[vars$height > 1,]
vars$geology <- as.factor(vars$geology)
vars2 <- vars[!names(vars) %in% 'geology']
vars2 <- data.frame(scale(vars2))
#vars <- cbind(vars2, vars[8:21])
vars <- cbind(vars2, vars[names(vars) %in% 'geology'])

# vars$geology <-
#   case_when(vars$geology==1~'KJde',
#             vars$geology==2~'Km',
#             vars$geology==3~'Kmv',
#             vars$geology==4~'Pm',
#             vars$geology==5~'PPm',
#             vars$geology==6~'Qd',
#             vars$geology==7~'Ql',
#             vars$geology==8~'Tmi',
#             vars$geology==9~'Two'
#             )

vars$geology <-
  case_when(vars$geology==1~'Dakota Sandstone',
            vars$geology==2~'Mancos Shale',
            vars$geology==3~'Mesa Verde Formation (Sand/Silt/Coal)',
            vars$geology==4~'Gothic Formation (Sand/Shale)',
            vars$geology==5~'Maroon Formation (Red Sand/Mud/Conglomerate)',
            vars$geology==6~'Glacial Drift',
            vars$geology==7~'Landslide Deposits',
            vars$geology==8~'Middle-Tertiary Granodioritic Laccoliths',
            vars$geology==9~'Wasatch Formation (Claystone-Shale)'
  )

vars$geology <- as.factor(vars$geology)

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
plot(vars$diam, vars$elevation)
plot(mod_lm)
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
                  s(tpi_1km, bs='cc') +
                  #s(twi_100m, bs='cc') +
                  s(heat_load, bs='cc') +
                  s(elevation, by=folded_aspect_205) +
                  s(elevation, by = tpi_1km) +
                  s(elevation, by = swe) +
                  s(folded_aspect_205, by = tpi_1km) +
                  #s(elevation, by=geology) +
                  #s(folded_aspect_205, by=geology) +
                  #s(tpi_1km, by=geology) +
                  geology +
                  s(awc, bs='cc') +
                  s(om, bs='cc') +
                  #s(k, bs='cc') +
                  #cec +
                  s(td, bs='cc') +
                  s(swe, bs='cc'),
                data=vars)

View(vars)

dens_gam <- gam(density ~
                  s(elevation, bs='cc') +
                  s(folded_aspect_205, bs='cc') +
                  s(slope, bs='cc') +
                  s(tpi_1km, bs='cc') +
                  #s(twi_100m, bs='cc') +
                  s(heat_load, bs='cc') +
                  s(elevation, by=folded_aspect_205) +
                  s(elevation, by = tpi_1km) +
                  s(elevation, by = swe) +
                  s(folded_aspect_205, by = tpi_1km) +
                  #s(elevation, by=geology) +
                  #s(folded_aspect_205, by=geology) +
                  #s(tpi_1km, by=geology) +
                  geology +
                  s(awc, bs='cc') +
                  s(om, bs='cc') +
                  s(ksat, bs='cc') +
                  #cec +
                  s(td, bs='cc') +
                  s(swe, bs='cc'),
                data=vars)

summary(dens_gam)

ht_gam <- gam(height ~
                  s(elevation, bs='cc') +
                  s(folded_aspect_205, bs='cc') +
                  s(slope, bs='cc') +
                  s(tpi_1km, bs='cc') +
                  #s(twi_100m, bs='cc') +
                  s(heat_load, bs='cc') +
                  s(elevation, by=folded_aspect_205) +
                  s(elevation, by = tpi_1km) +
                  s(elevation, by = swe) +
                  s(folded_aspect_205, by = tpi_1km) +
                  #s(elevation, by=geology) +
                  #s(folded_aspect_205, by=geology) +
                  #s(tpi_1km, by=geology) +
                  geology +
                  s(awc, bs='cc') +
                  s(om, bs='cc') +
                  s(ksat, bs='cc') +
                  #cec +
                  s(td, bs='cc') +
                  s(swe, bs='cc'),
                data=vars)

diam_gam <- gam(diam ~
                  s(elevation, bs='cc') +
                  s(folded_aspect_205, bs='cc') +
                  s(slope, bs='cc') +
                  s(tpi_1km, bs='cc') +
                  s(twi_100m, bs='cc') +
                  s(heat_load, bs='cc') +
                  s(elevation, by=folded_aspect_205) +
                  s(elevation, by = tpi_1km) +
                  s(elevation, by = swe) +
                  s(folded_aspect_205, by = tpi_1km) +
                  #s(elevation, by=geology) +
                  #s(folded_aspect_205, by=geology) +
                  #s(tpi_1km, by=geology) +
                  geology +
                  s(awc, bs='cc') +
                  s(om, bs='cc') +
                  s(ksat, bs='cc') +
                  #cec +
                  s(td, bs='cc') +
                  s(swe, bs='cc'),
                data=vars)

ba_gam <- gam(ba ~
                  s(elevation, bs='cc') +
                  s(folded_aspect_205, bs='cc') +
                  s(slope, bs='cc') +
                  s(tpi_1km, bs='cc') +
                  #s(twi_100m, bs='cc') +
                  s(heat_load, bs='cc') +
                  s(elevation, by=folded_aspect_205) +
                  s(elevation, by = tpi_1km) +
                  s(elevation, by = swe) +
                  s(folded_aspect_205, by = tpi_1km) +
                  #s(elevation, by=geology) +
                  #s(folded_aspect_205, by=geology) +
                  #s(tpi_1km, by=geology) +
                  geology +
                  s(awc, bs='cc') +
                  s(om, bs='cc') +
                  s(ksat, bs='cc') +
                  #cec +
                  s(td, bs='cc') +
                  s(swe, bs='cc'),
                data=vars)

summary(mod_gam2)
summary(dens_gam)
summary(ht_gam)
summary(diam_gam)
summary(ba_gam)
coef(dens_gam)
plot.gam(dens_gam)
plot.gam(ht_gam)
plot.gam(diam_gam)
plot.gam(ba_gam)


names(mod_gam2$coefficients)
par(mfcol=c(12,2), mar=c(rep(1,2), rep(1,2)))
plot.gam(mod_gam2, scheme=1, ylim=c(-5,5))

varnms <- c('Elevation',
  'Folded Aspect',
  'Slope',
  'TPI',
  'Heat Load',
  'Elevation:Folded Aspect',
  'Elevation:TPI',
  'Elevation:SWE',
  'Folded Aspect:TPI',
  'Elevation:KJde',
  'Elevation:Km',
  'Elevation:Kmv',
  'Elevation:Pm',
  'Elevation:PPm',
  'Elevation:Qd',
  'Elevation:Ql',
  'Elevation:Tmi',
  'Elevation:Two',
  'Soil AWC',
  'Soil Percent OM',
  'Soil k',
  'Soil Total Depth',
  'SWE')
?mar
par(mfcol=c(12,4), mar=c(4,2,4,1), lwd=2)
for(i in 19:22){
  plot.gam(mod_gam2,
           scheme=1,
           ylim=c(-5,5),
           select=i,
           main=varnms[i],
           ylab='Stand density (stems/ha)',
           xlab=paste('Standardized', varnms[i]),
           cex.lab=2,
           cex.main=2.5)
}

ggplot(vars, aes(x=density, y=elevation, color=geology)) +
  geom_point() +
  geom_abline()

visreg(mod_gam2)

termplot(dens_gam)

AIC(mod_lm)
AIC(mod_gam2)

summary(mod_lm)$sp.criterion
summary(mod_gam2)$sp.criterion

g <- list(
visreg2d(dens_gam, xvar='elevation', yvar='swe', plot.type='gg') +
  scale_fill_viridis(name='density') +
  theme_minimal(base_size = 16) +
  theme(legend.title=element_text(size=14), legend.position = 'bottom'),
visreg2d(ht_gam, xvar='elevation', yvar='ksat', plot.type='gg') +
  scale_fill_viridis(name='height') +
  theme_minimal(base_size = 16) +
  theme(legend.title=element_text(size=14), legend.position = 'bottom'),
visreg2d(diam_gam, xvar='elevation', yvar='heat_load', plot.type='gg') +
  scale_fill_viridis(name='dbh') +
  theme_minimal(base_size = 16) +
  theme(legend.title=element_text(size=14), legend.position = 'bottom'),
visreg2d(ba_gam, xvar='elevation', yvar='om', plot.type='gg') +
  scale_fill_viridis(name='basal area') +
  theme_minimal(base_size = 16) +
  theme(legend.title=element_text(size=14), legend.position = 'bottom')
)
g
g + facet_grid()
library(gridExtra)
marrangeGrob(g, nrow=2, ncol=2)
?marrangeGrob()
mod_gam2$coefficients
par(mfcol=c(1,3), mar=c(2,2,4,2))
vis.gam(mod_gam2, view=c('elevation','swe'), type='response', plot.type='persp', phi=20, theta=48, border=NA, color='topo', zlab='90th pctl height', contour.col='black', main='Density v Elevation v TPI', xlab='Standardized elevation', ylab='standardized TPI', cex.lab=2, cex.main=2.5)

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
