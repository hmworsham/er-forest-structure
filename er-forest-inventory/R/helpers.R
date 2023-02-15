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
