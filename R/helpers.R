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
    tmpfile <- drive_download(
      as_id(x[['id']]),
      path=file.path(
        tempdir(),
        x[['name']]),
      overwrite=T)$local_path
    return(tmpfile)
  })

  shpfiles <- tmpfiles[file_ext(tmpfiles)=='shp']
  sfs <- lapply(shpfiles, function(x) {
    shp <- st_read(x)
    shp <- st_transform(shp, 'EPSG:32613')
    shp
  })
  return(sfs)
}
