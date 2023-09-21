# Script for preparing datasets for modeling work

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

#############################
# Set up working environment
#############################

# Define directories
datadir <- file.path('/Volumes', 'GoogleDrive', 'My Drive', 'Research', 'RMBL', 'RMBL-East River Watershed Forest Data', 'Data')
strdir <- file.path(datadir, 'LiDAR', 'tifs')

#############################
# Ingest data
#############################

# Ingest forest structure rasters
list.files(strdir)
## TODO: be sure to get the correct files; there are some extraneous ones in here
responses <- lapply(list.files(strdir, full.names=T, pattern='*.tif$')[1:10], raster)

#############################
# Plot
#############################
list.files(strdir)
values(responses[[9]])[values(responses[[9]]==1)] <- NA
par(mfrow=c(2,3),
    mar=c(1,1,1,1)+.5)
names <- str_replace(list.files(strdir)[1:9], '.tif', '')
names <- str_replace(names, '_fromtrees', '')
names <- str_replace(names, '_', ' ')
names <- str_replace(names, '_', ' ')
names[10] <- 'basal_area'

toplot <- c(9,10,5,8,7,9)

for(i in seq_along(toplot)) {
  terra::plot(raster::projectRaster(responses[[toplot[i]]], crs='EPSG:4326'),
              col=viridis(n=20, option=LETTERS[i]),
              #main=names[toplot[i]],
              title(names[toplot[i]], line=2, cex=3),
              cex.main=3
              )
}
