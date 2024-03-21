# Process POLARIS soils data

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

# Specify directories
polarisdir <- file.path(config$extdata$scratch, 'POLARISOut')

##########################################################
# Download POLARIS data at grid intersecting East River
##########################################################

erw <- data.frame('ID'=c('ER-SW', 'ER-NW', 'ER-NE', 'ER-SE'),
                  'lat'=c('38.885', '39.015', '39.015', '38.885'),
                  'long'=c('-107.05', '-107.05', '-106.925', '-106.925'))
erw.sf <- st_as_sf(erw, coords=c('long', 'lat'))

aop <- load.plot.sf(path=as_id(config$extdata$bndid),
                    pattern=config$extdata$bndpattern)
aop <- st_transform(aop, 'EPSG:4326')

polaris.src <- xplot(erw)

xy <- XPolaris::ximages(locations=erw,
              statistics='mean',
              variables='alpha',
              layersdepths='5_15',
              localPath=tempdir())

pullvars <- c('ph', 'om', 'sand', 'clay', 'bd',
                        'hb', 'n', 'alpha', 'ksat', 'lambda',
                        'theta_r', 'theta_s')

polaris.get <- XPolaris::ximages(locations=erw,
                       statistics=c('mean', 'p50', 'p5', 'p95'),
                       variables=pullvars,
                       layersdepths=c('0_5', '5_15', '15_30', '30_60', '60_100'),
                       localPath=file.path(config$extdata$scratch))

########################
# Check POLARIS files
########################

polfiles <- list.files(polarisdir, recursive = T, full.names=T)
poldirs <- unique(dirname(polfiles))

for(i in poldirs) {
  x=print(i)
  print(length(list.files(i)))
}

polfiles.df <- data.frame(do.call(rbind, str_split(polfiles, '/')))

########################
# Mosaic POLARIS files
########################
poldirs.mean <- poldirs[grepl('mean', poldirs)]
poldirs.median <- poldirs[grepl('p50', poldirs)]

lapply(poldirs.median, \(x){
  xras=lapply(list.files(x, full.names=T), rast)
  xmos=do.call(mosaic, xras)
  outpath=paste0(paste(c(unlist(strsplit(x, '/'))[7:9], names(xmos)), collapse="_"), '.tif')
  writeRaster(xmos, file.path(config$extdata$scratch, 'POLARIS', 'median', outpath))
})


###############################
# Compute depth-weighted means
###############################

pol.mean.files <- list.files(file.path(config$extdata$scratch, 'POLARIS', 'mean'), full.names=T)

for(i in pullvars) {

  wmf <- function(x,w) {
    sum(x*w)/sum(w)}

  fs <- pol.mean.files[grepl(paste0('_', i, '_'), pol.mean.files)]

  if(i %in% c('ksat', 'hb', 'alpha')) {
    rs <- rast(fs)
    rs <- 10^rs
    rs.wm <- wmf(rs, w=c(.05, .10, .15, .3, .4))
    rs.wm <- log(rs.wm, 10)
  }

  else if(i %in% c('om')) {
    rs <- rast(fs[c(1,4,2)])
    rs <- 10^rs
    rs.wm <- wmf(rs, w=c(.05, .10, .15))
    rs.wm <- log(rs.wm, 10)
  }

  else{
    rs <- rast(fs)
    rs.wm <- wmf(rs, w=c(0.5, .10, .15, .3, .4))
  }

  rs.wm <- project(rs.wm, 'EPSG:32613')
  names(rs.wm) <- i
  outpath <- paste0('dwm_', i, '.tif')
  writeRaster(rs.wm, file.path(config$extdata$scratch, 'POLARIS', 'dwm', outpath), overwrite=T)

}
