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
values(diam)[values(diam)==1] <- NA ## TODO: REMOVE THIS ONCE COERCED IN makerasters.R
ba <- pi*(diam/2)**2 ## TODO: REMOVE THIS ONCE BA COMPUTED IN makerasters.R
# ba <- raster(file.path(strdir, 'basal_area_100m.tif'))
response <- list(
  'density'=dnsty,
  'height'=height,
  'diam'=diam,
  'ba'=ba
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

bcm.aet <- raster::projectRaster(bcm.aet, crs=crs(aop))
bcm.cwd <- raster::projectRaster(bcm.cwd, crs=crs(aop))

#### Soil resistivity ####
# resist <- read.csv(file.path(scondir, 'AEM_Res_19m_Kriging.csv'))
# resist[resist==-999.9] <- NA
# sresist <- raster(xmn=min(resist$X), xmx=max(resist$X), ymn=min(resist$Y), ymx=max(resist$Y), res=100, crs="+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs")
# sresist <- rasterize(resist[, c('X', 'Y')], sresist, resist[, 'Res'], fun=mean)

# Append all explainers to one list
explainers <- append(topos, c(geol, soil, snow, snow.delta, bcm.aet, bcm.cwd))
sapply(explainers, names)
length(explainers) == 33

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

# Crop explainers to AOP and align to response
explainers <- lapply(explainers, cropfun, aop)
explainers <- lapply(explainers, alignfun, response[[1]], 'ngb')

opar <- par()
par(mfcol=c(5,6), mar=rep(1,4))
for(i in seq_along(explainers)) {
  opt <- rep(LETTERS[1:6],5)
  plot(explainers[[i]], col=viridis(10, option=opt[i]), main=names(explainers[[i]]), asp=1)
}
par(opar)

#############################
# Extract values for modeling
#############################

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
par(opar)

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
                'swe',
                #'delta_swe',
                'aet',
                'cwd')

# Isolate the variables selected in the list above
sapply(explainers, names)
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

#############################
# Rescale variables
#############################

# Exclude geology to rescale continuous variables
noscale <- c('geology', 'density', 'height', 'diam', 'ba')
vars.tmp <- vars[!names(vars) %in% noscale]
vars.tmp <- data.frame(scale(vars.tmp))
vars <- cbind(vars[names(vars) %in% noscale], vars.tmp)

###############################
# Deal with geology / factors
###############################

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
