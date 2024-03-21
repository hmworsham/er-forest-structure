# Script for preparing datasets for modeling work

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

# Set number of cores
nCores <- as.integer(availableCores()-2)

#############################
# Ingest data
#############################

# Define directories
strdir <- drive_get(as_id('17EvPewC6rBMcLFzVGUOv-Olaiigt3aEY'))
topodir <- drive_get(as_id('1irxlcVuAe4T8awS9eDMwfxf2eaN1EQh2'))
#soildir <- drive_get(as_id('12917m24buDgeRndFYr0ugJUgAk7V5DBo')) # POLARIS
soildir <- drive_get(as_id('17rDFA_kpTxNn6738fRINoq4Xiav6BDdn')) # SSURGO
snowdir <- drive_get(as_id('1WFoOA13bZtchE1DETE8l3CaEgV2GEY0Z'))
bcmdir <- drive_get(as_id('10Pe66lYwxER6km16oOubKXJ-Csp1wLpi'))

#############################
# Ingest data
#############################

#### AOP ####

# Ingest AOP survey area polygon
aop <- load.plot.sf(path=as_id(config$extdata$bndid),
                    pattern=config$extdata$bndpattern)

#### NAIP ####

# Ingest NAIP true color
naipfile <- drive_download(
  as_id('11rc7aWJzDC6lw-RyaNkoJxzq4iwFmxCT'),
  path=file.path(tempdir(), 'aop_naip_ortho_3m.tif'),
  overwrite = T)$local_path

naip <- rast(naipfile)

#### Forest structure ####

# List and download forest structure rasters
strfiles <- drive_ls(strdir)
strfiles <- unlist(lapply(1:nrow(strfiles), \(x) {
  tf <- drive_download(
    as_id(strfiles$id[x]),
    path=file.path(tempdir(), strfiles$name[x]),
    overwrite=T)
  tf$local_path
  }))

# Ingest forest structure rasters
dnsty <- rast(strfiles[grep('density_100m_masked.tif', strfiles)])
height <- rast(strfiles[grep('height_95pctl_100m_masked.tif', strfiles)])
height.skew <- rast(strfiles[grep('height_skew_100m_masked.tif', strfiles)])
diam <- rast(strfiles[grep('diam_qmd_100m_masked.tif', strfiles)])
ba <- rast(strfiles[grep('ba_100m_masked.tif', strfiles)])

# Ingest forest species composition rasters
abla <- rast(strfiles[grep('density_abla_100m_masked.tif', strfiles)])
pien <- rast(strfiles[grep('density_pien_100m_masked.tif', strfiles)])
pico <- rast(strfiles[grep('density_pico_100m_masked.tif', strfiles)])

response <- list(
  'density'=dnsty,
  'height'=height,
  'height.skew'=height.skew,
  'diam'=diam,
  'ba'=ba,
  'abla_density'=abla,
  'pien_density'=pien,
  'pico_density'=pico
  )

for(i in seq_along(response)){
  names(response[[i]]) <- names(response)[i]
}

#### Topographic variables ####

# Define topographic factors to ingest
topo.factors <- c('usgs_205faspect_100m.tif',
                  'usgs_curvature_10m.tif',
                  'usgs_dem_100m.tif',
                  'usgs_slope_100m.tif',
                  'usgs_tpi_1km.tif',
                  'usgs_twi_100m.tif',
                  'usgs_heatload_100m.tif'
                  )

# Ingest topos
topofiles <- drive_ls(topodir, pattern='.tif$', recursive=T)
topofiles <- topofiles[topofiles$name %in% topo.factors,]
topofiles <- unlist(lapply(1:nrow(topofiles), \(x) {
    tf <- drive_download(
      as_id(topofiles$id[x]),
      path=file.path(tempdir(), topofiles$name[x]),
      overwrite=T)
    tf$local_path
  }))

topos <- lapply(topofiles, rast)

#### Geology ####

# Ingest geology, project to AOP CRS and mask to AOP boundary
geofile <- drive_download(
  as_id('1XXMHN4mvchUPCedWzgS525Eyrp9e-rXy'),
  path=file.path(tempdir(), 'COGS_eastriver_geology.tif'),
  overwrite = T)$local_path

geol <- rast(geofile)
geol <- as.factor(geol)

#### Soil ####

soilfiles <- drive_ls(soildir)
soilfiles <- unlist(lapply(1:nrow(soilfiles), \(x) {
  stif <- drive_download(
    as_id(soilfiles$id[x]),
    path=file.path(tempdir(), soilfiles$name[x]),
    overwrite=T)
  stif$local_path
}))

soil <- lapply(soilfiles, rast)

#### ASO snow ####
snowfiles <- drive_ls(snowdir)
snowfiles <- snowfiles[snowfiles$name %in% c('mean_swe_18-22.tif', 'mean_delta_swe_18-22.tif'),]
snowfiles <- unlist(lapply(1:nrow(snowfiles), \(x) {
  stif <- drive_download(
    as_id(snowfiles$id[x]),
    path=file.path(tempdir(), snowfiles$name[x]),
    overwrite=T)
  stif$local_path
}))

snow <- lapply(snowfiles, rast)

#### BCM ####
bcmfiles <- drive_ls(bcmdir)
bcmfiles <- bcmfiles[bcmfiles$name %in% c('aet1985to2012wy_meanTotalAnnual_x100i.tif',
                                          'cwd1985to2012wy_meanTotalAnnual_x100i.tif'),]
bcmfiles <- unlist(lapply(1:nrow(bcmfiles), \(x) {
  stif <- drive_download(
    as_id(bcmfiles$id[x]),
    path=file.path(tempdir(), bcmfiles$name[x]),
    overwrite=T)
  stif$local_path
}))

bcm <- lapply(bcmfiles, rast)
bcm <- lapply(bcmfiles, \(x) terra::project(rast(x), crs(aop)))

# Append all continuous explanatory variables to one list
explainers <- append(topos, c(soil, snow, bcm))
sapply(explainers, names)
length(explainers) == 20

varnames <- read.csv(file.path(config$data$int, 'explainer_names_table.csv'),
                     row.names=1)

for(i in seq_along(explainers)){
  names(explainers[[i]]) <- varnames[varnames$rastnames==names(explainers[[i]]), 'varnames']
}

names(geol) <- 'geology'

#############################
# Clean, crop, align rasters
#############################

# Crop forest structure variables to AOP
response <- lapply(response, cropfun, aop)

# Crop geology to AOP and align to response
geol <- cropfun(geol, aop)
geol <- alignfun(geol, response[[1]], method='near')
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

# Extract values from explainers
ex.vals <- lapply(explainers, getvals)

# Assemble dataframe for modeling
vars <- data.frame(re.vals, ex.vals)

# Get coordinates and add to explainers
xy <- data.frame(xyFromCell(response[[1]], 1:prod(dim(response[[1]]))))
vars <- data.frame(vars, xy)

# Filter out any values where stand density <=100 trees / ha
vars <- vars[vars$density>100,]
vars <- vars[!is.na(vars$density),]

# Hang on to the unscaled variable values
vars.unscaled <- vars

#############################
# Rescale variables
#############################

# Exclude geology and response to rescale continuous explanatory variables
noscale <- c('geology', 'density', 'height',
             'height.skew', 'diam', 'ba',
             'abla_density', 'pien_density', 'pico_density')

vars.tmp <- vars[!names(vars) %in% noscale]
vars.tmp <- data.frame(scale(vars.tmp))
vars <- cbind(vars[names(vars) %in% noscale], vars.tmp)

###############################
# NaN to NA
###############################

vars <- vars %>%
  mutate_all(~ifelse(is.nan(.), NA, .))

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
