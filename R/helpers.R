# Forest structure helpers

#' Load packages
#' @description Loads new packages, installing if they're not already installed
#' @param pkg Character string. Package name
#' @return NULL. Loads packages in global environment
#' @export load.pkgs
#'
load.pkgs <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Function to load plot geolocation shapefiles
load.plot.sf <- function(path, pattern) {

  plotobj <- drive_ls(
    path=path,
    pattern=pattern
  )

  tmpfiles <- apply(plotobj, 1, function(x) {
    if(!file_ext(x[['name']]) %in% c('xml', 'kmz')) {
      tmpfile <- drive_download(
        as_id(x[['id']]),
        path=file.path(
          tempdir(),
          x[['name']]),
        overwrite=T)$local_path}
    else tmpfile <- NULL
    return(tmpfile)
  })

  shpfile <- tmpfiles[file_ext(tmpfiles)=='shp']
  shp <- st_read(shpfile)
  shp <- st_transform(shp, 'EPSG:32613')

  return(shp)
}

# Function to ingest rasters
get.rasters <- function(x, dir){
  xpath = file.path(dir, x)
  xtif = list.files(xpath, pattern = 'tif$', full.names = T)
  xras = lapply(xtif, raster)
  return(xras)
}

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

# Function to extract values from rasters
getvals <- function(ras) return(values(ras))

# Function to generate pngs and save to disk
runpng <- function(ras, bound, clrs, filepath){
  outras = mask(ras, bound)
  png(
    file=file.path,
    width=1200,
    height=1200)
  par(mar= c(5,4,4,2)+0.1)
  plot(outras, col=clrs)
  plot(bound$geometry, col=NA, border='grey10', axes=T, labels=T, add=T)
  dev.off()
}

# Function to compute arbitrary nth root
nthroot = function(x,n) {
  (abs(x)^(1/n))*sign(x)
}
