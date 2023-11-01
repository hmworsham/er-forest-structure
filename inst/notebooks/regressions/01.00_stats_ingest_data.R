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
strdir <- file.path(datadir, 'LiDAR', 'tifs', 'conifer_masked')
rasdir <- file.path(datadir, 'Geospatial', 'Worsham_SiteSelection', '2021_Analysis_Layers', 'USGS_1-9_arcsec_DEM')
sfdir <- file.path(datadir, 'Geospatial', 'RMBL_2020_EastRiver_SDP', 'RMBL_2020_EastRiver_SDP_Boundary')
geodir <- file.path(datadir, 'Geospatial', 'Colorado_Geological_Survey')
scondir <- file.path(datadir, 'Geospatial', 'Uhlemann_2021_RESubsurfaceResistivityMap')
soildir <- file.path(datadir, 'Soil', 'SSURGO', 'Processed')
snowdir <- file.path(datadir, 'Geospatial', 'ASO_Snow', 'Processed')
bcmdir <- file.path(datadir, 'Climate', 'BCM_CO', 'UCRB_BCM')
naipdir <- file.path(datadir, 'Geospatial', 'USDA_2019_NAIP_Ortho', 'NAIP_ortho_1-1_hn_s_co051_2019')

#############################
# Ingest data
#############################

#### AOP ####

# Ingest AOP survey area polygon
aop <- st_read(file.path(sfdir, 'SDP_Boundary.shp'))

#### Forest structure ####

# Ingest NAIP true color
# naip <- stack(file.path(naipdir, 'aop_naip_ortho_3m.tif'))

# Ingest forest structure rasters
dnsty <- raster(file.path(strdir, 'density_100m_conifmask.tif'))
height <- raster(file.path(strdir, 'height_90pctl_100m_conifmask.tif'))
height.skew <- raster(file.path(strdir, 'height_skew_100m_conifmask.tif'))
diam <- raster(file.path(strdir, 'diam_qmd_100m_conifmask.tif'))
ba <- raster(file.path(strdir, 'ba_100m_conifmask.tif'))


response <- list(
  'density'=dnsty,
  'height'=height,
  'height.skew'=height.skew,
  'diam'=diam,
  'ba'=ba
  )

# Check forest structure rasters
#lapply(response, crs)
#lapply(response, plot, col=viridis(9))

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
geol <- as.factor(geol)

#### SSURGO soil ####

#soil <- soil.rast.ls
# list.files(file.path(ssurgo.dir))
soil <- lapply(list.files(soildir, full.names=T, pattern='tif$'), raster, crs=crs(aop))

#### ASO snow ####
snow <- raster(file.path(snowdir, 'mean_swe_18-22.tif'))
snow.delta <- raster(file.path(snowdir, 'mean_delta_swe.tif'))

#### BCM ####
bcm.aet <- raster(file.path(bcmdir, 'aet1985to2012wy_meanTotalAnnual_x100i.tif'))
bcm.cwd <- raster(file.path(bcmdir, 'cwd1985to2012wy_meanTotalAnnual_x100i.tif'))

bcm.aet <- raster::projectRaster(bcm.aet, crs=crs(aop))
bcm.cwd <- raster::projectRaster(bcm.cwd, crs=crs(aop))

#### Soil resistivity ####
# resist <- read.csv(file.path(scondir, 'AEM_Res_19m_Kriging.csv'))
# resist[resist==-999.9] <- NA
# sresist <- raster(xmn=min(resist$X), xmx=max(resist$X), ymn=min(resist$Y), ymx=max(resist$Y), res=100, crs="+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs")
# sresist <- rasterize(resist[, c('X', 'Y')], sresist, resist[, 'Res'], fun=mean)

# Append all continuous explainers to one list
explainers <- append(topos, c(soil, snow, snow.delta, bcm.aet, bcm.cwd))
#sapply(explainers, names)
length(explainers) == 32

# Clean up names of explainers
varnames <- c('adj_southness_205',
              'folded_aspect_205',
              'aspect',
              'aspect_10m',
              'cos_aspect', #5
              'sin_aspect',
              'curvature',
              'elevation_10m',
              'elevation_100m',
              'elevation_1km', #10
              'elevation_30m',
              'slope',
              'slope_10m',
              'tpi_1km',
              'tpi_2km', #15
              'twi_100m',
              'twi_1km',
              'heat_load',
              'awc', # 20
              'cec',
              'clay',
              'k',
              'ksat',
              'om', # 25
              'pH',
              'sand',
              'silt',
              'td',
              'swe', #30
              'delta_swe',
              'aet',
              'cwd'
              )

for(i in seq_along(explainers)){
  names(explainers[[i]]) <- varnames[i]
}

#############################
# Clean, crop, align rasters
#############################

# Crop forest structure variables to AOP
response <- lapply(response, cropfun, aop)

# Crop geology to AOP and align to response
geol <- cropfun(geol, aop)
geol <- alignfun(geol, response[[1]], method='ngb')
names(geol) <- 'geology'

# Crop continuous explainers to AOP and align to response
explainers <- lapply(explainers, cropfun, aop)
explainers <- lapply(explainers, alignfun, response[[1]], 'bilinear')

# Crop NAIP to AOP and align to response
# naip <- cropfun(naip, aop)
# naip <- alignfun(naip, response[[1]], 'bilinear')

# Add geol to explainers
explainers <- c(explainers, geol)

#############################
# Extract values for modeling
#############################

# Extract forest structure values
re.vals <- lapply(response, getvals)

# Histogram of structure data
opar <- par()
par(mfcol=c(1, length(re.vals)), mar=rep(2.5,4))
res.labs <- c(density='Density', height='Height',
              'height.skew'='Height skew', diam='QMD', ba='BA')
for(i in seq_along(re.vals)){
  hist(re.vals[[i]],
       c='grey50',
       breaks=20,
       border='white',
       main=(res.labs[i]),
       sub='100m pixel',
       xlab=names(re.vals)[i],
       ylab='count',
       cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)
}
par(opar)

# Define variables we want to use in model
#target.vars <- c('folded_aspect_205', 'swe')
target.vars <- c(#'folded_aspect_205',
                'elevation_10m',
                'slope',
                'tpi_1km',
                #'twi_100m',
                'heat_load',
                'awc',
                'om',
                #'cec',
                'k',
                #'ksat',
                'td',
                'swe',
                'delta_swe',
                'aet',
                'cwd',
                'geology')

# Isolate the variables selected in the list above
explainers.sub <- explainers[sapply(explainers, names) %in% target.vars]

# Extract values from explainers
ex.vals <- sapply(explainers.sub, getvals)

# Assemble dataframe for modeling
vars <- data.frame(re.vals, ex.vals)
names(vars) <- c(names(re.vals), target.vars)

# Filter out any values where stand density <=100 trees / ha
vars[vars$density>100,]
vars <- vars[vars$density>100,]
vars <- vars[!is.na(vars$density),]

# Hang on to the unscaled variable values
vars.unscaled <- vars

#############################
# Rescale variables
#############################

# Exclude geology to rescale continuous variables
noscale <- c('geology', 'density', 'height', 'height.skew', 'diam', 'ba')
vars.tmp <- vars[!names(vars) %in% noscale]
vars.tmp <- data.frame(scale(vars.tmp))
vars <- cbind(vars[names(vars) %in% noscale], vars.tmp)

###############################
# Deal with geology / factors
###############################

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
