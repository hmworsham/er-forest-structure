# Function to install new packages if they're not already installed
load.pkgs <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Function to download inventory data from Drive and load as list
load.inventory <- function(filename){

  # Download file from Drive to tmp
  tmpfile <- drive_download(
    filename,
    type='xlsx',
    path=tempfile(),
    overwrite=T)$local_path

  # Define column types
  coltypes = c('text', #site name
               'numeric', #census number
               'date', #censusstart
               'date', #census end
               'numeric', #tag number
               'numeric', #previous tag number
               'date', #tag date
               rep('text',6), #spp data
               rep('numeric',3), #heights
               'date', #height date
               'text', #height method
               rep('numeric', 3), #dbh data
               'date', #dbh date
               'text', #dbh method
               'numeric', #dbh hom
               'numeric', #cii
               'date', #cii date
               'text', #canopy position
               'date', # canopy position date
               'text', #beetles qual
               'numeric', #beetles index
               'date', # beetles date
               rep('text',3), #status, health, comments
               'logical', #geotagged
               'numeric', #geotag assoc ref
               'numeric', #geotag assoc dist
               'text', #geotag assoc dir
               'date', #geotag date
               rep('numeric',2), #lat lon
               'text', #gps filename
               'date', #entry date
               'text' #entry personnel
  )

  # Check length of column types to ensure match
  length(coltypes)==44

  # Import file with read.csv
  df <- read_xlsx(
    tmpfile,
    sheet = 1,
    col_types = coltypes,
    na = 'NA')
  return(df)
}


# Function to load tree geolocation shapefiles
load.trees <- function(path, pattern) {

  treeobj <- drive_ls(
    path=path,
    pattern=pattern
  )

  tmpfiles <- apply(treeobj, 1, function(x) {
    tmpfile <- drive_download(
      as_id(x[['id']]),
      path=file.path(
        tempdir(),
        x[['name']]),
      overwrite=T)$local_path
    return(tmpfile)
  })

  shpfiles <- tmpfiles[file_ext(tmpfiles)=='shp']
  sfs <- lapply(shpfiles, munge.trees)
  return(sfs)
}

# Function to munge tree geolocation shapefiles
munge.trees <- function(x){
  shp <- st_read(x)
  names(shp)[which(names(shp) %in% c(
    'Site',
    'Tg_Nmbr',
    'Sp_Code',
    'Filenam',
    'Gtg_Ass'))] <- c(
      'Site_Name',
      'Tag_Number',
      'Sp_Code',
      'Filename',
      'Geotag_Association')
  shp <- st_transform(shp, 'EPSG:4326')
  shp$X <- st_coordinates(shp)[,1]
  shp$Y <- st_coordinates(shp)[,2]
  shp$Sp_Code <- as.character(shp$Sp_Code)
  shp$Geotag_Association <- as.numeric(shp$Geotag_Association)
  shp$Comment <- as.character(shp$Comment)
  shp
}

# Function to load raw GPS files
load.rawgps <- function(path, pattern, nmax=950) {

  gpsobj <- drive_ls(
    path=path,
    q=sprintf('name contains "%s"', pattern),
    recursive=T,
    n_max=nmax
  )

  tmpfiles <- apply(gpsobj, 1, function(x) {
    tmpfile <- drive_download(
      as_id(x[['id']]),
      path=file.path(
        tempdir(),
        x[['name']]),
      overwrite=T)$local_path
    return(tmpfile)
  })

  shpfiles <- tmpfiles[file_ext(tmpfiles)=='shp']
  rawgps <- lapply(shpfiles, st_read)
  names(rawgps) <- basename(shpfiles)
  return(rawgps)
}

# Function to merge stem observations by site and write to storage
merge.stems.by.site <- function(sitegroup, write=F) {
  sitetrees <- bind_rows(sitegroup, .id='Filename')
  sitetrees <- st_transform(sitetrees, crs='EPSG:32613')
  sitetrees$Site <- unlist(lapply(strsplit(sitetrees$Filename, '_'), '[', 2))
  site <- unlist(sitetrees$Site[1])
  dir.create(config$dat_merged)
  if (write) {
    st_write(
      sitetrees,
      file.path(
        config$dat_merged,
        'GPS_Data_Merged_2021',
        paste0(
          'GPS_Data_Merged_2021',
          site,
          '.shp')),
      driver='ESRI Shapefile',
      append=F)}

  return (sitetrees)
}

# Function to construct geolocation points via noted associations with tagged trees
make.new.point <- function(df) {

  # Conversion from degrees to radians
  rad = pi/180

  # Define radian direction based on cardinal direction input
  # if (!(is.numeric(direction))) {
  df$Geotag_Association_Dir <- tolower(df$Geotag_Association_Dir)
  df$nmdir <- case_when(
    df$Geotag_Association_Dir=='e' ~ 0.0001, # Approximates 90º E as 0 rad
    df$Geotag_Association_Dir=='n' ~ 90,
    df$Geotag_Association_Dir=='w' ~ 180,
    df$Geotag_Association_Dir=='s' ~ 270,
    df$Geotag_Association_Dir=='ne' ~ 45,
    df$Geotag_Association_Dir=='nw' ~ 135,
    df$Geotag_Association_Dir=='se' ~ 225,
    df$Geotag_Association_Dir=='sw' ~ 315,
    TRUE ~ as.numeric(df$Geotag_Association_Dir))

  df$raddir <- rad*df$nmdir

  # Find x,y coordinates of reference tag number
  df$sireftag <- paste0(df$Site_Name, '_', df$Geotag_Association_Ref)
  df$refgeom <- df$geometry[match(df$sireftag, df$SiTag)]
  df$refx <- st_coordinates(df$refgeom)[,1]
  df$refy <- st_coordinates(df$refgeom)[,2]

  # Calculate coordinates of target tag number
  df$x_prime = df$Geotag_Association_Dist * cos(df$raddir) + df$refx
  df$y_prime = df$Geotag_Association_Dist * sin(df$raddir) + df$refy

  # Create a new temp gpdf from new geometries
  new_df = as.data.frame(df[c(
    'Site_Name',
    'SiTag',
    'Tag_Number.x',
    'Geotag_Association_Ref',
    'Filename',
    'x_prime',
    'y_prime'
  )])

  # Remove NA computed x,y coordinates
  new_df <- new_df[(!is.na(new_df$x_prime)) & (!is.na(new_df$y_prime)),]

  # Add comment field
  new_df$Comment <- 'Point generated post-campaign from geotag association note'

  # Convert to sf in CRS 32613
  new_sf = st_as_sf(new_df, coords = c('x_prime', 'y_prime'), crs = 'epsg:32613')
  names(new_sf) = c('Site',
                    'SiTag',
                    'Tag_Number',
                    'Geotag_Association',
                    'Filename',
                    'Comment',
                    'geometry')
  return(new_sf)
}

write.trees.by.site <- function(sf){
  site <- unlist(sf$Site[1])
  dir.create(
    file.path(
      config$dat_stemgeo))
  st_write(
    sf,
    file.path(
      config$dat_stemgeo,
      paste0(
        'Kueppers_EastRiver_Stem_Geolocations_WGS84UTM13N_',
        site,
        '.shp')),
    driver='ESRI Shapefile',
    append=F)
}
