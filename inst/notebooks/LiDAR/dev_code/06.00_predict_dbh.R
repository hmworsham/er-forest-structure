datadir <- '/global/scratch/users/worsham/'
plotsdir <- '/global/scratch/users/worsham/geolocated_returns_plots'
shapedir <- '/global/scratch/users/worsham/EastRiver/Plot_Shapefiles/Polygons'
fidir <- '/global/scratch/users/worsham/EastRiver/Inventory_Plots'

## ---------------------------------------------------------------------------------------------------
# Load config
config <- config::get(file=file.path('~',
                                     'Repos',
                                     'er-forest-structure',
                                     'config',
                                     'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

## ---------------------------------------------------------------------------------------------------
drive_auth(path=config$drivesa)

## ---------------------------------------------------------------------------------------------------
# Ingest plot boundaries
plotsf <- load.plot.sf(path=as_id(config$extdata$plotid),
                       pattern=config$extdata$plotpattern)

# Ingest field data
tmpfile <- drive_download(
  as_id(config$extdata$invid),
  type='csv',
  path=file.path(tempdir(), config$extdata$invid),
  overwrite=T)$local_path

inv <- read.csv(tmpfile)


aois <- list.files(plotsdir, full.names = F)
aois <- unique(sapply(strsplit(aois, '_2018'), '[', 1))

predictdbh <- function(aoi, inventorydir, shapedir) {

  # Get plot boundary for aoi
  plotpath = list.files(shapedir,
                        pattern = glob2rx(paste0(aoi,"*shp")),
                        full.names = T)
  print(plotpath)
  plotsf = vect(plotpath)
  geoextent = as.list(ext(plotsf))

  invfiles = list.files(fidir, pattern = paste0(aoi,'_inventory_data_20'), recursive=T, full.names = T)
  inv = read_excel(invfiles[1], sheet='inventory_data')
  df = data.frame('Z'=as.numeric(inv$Height_Avg_M), 'X'=as.numeric(inv$Longitude), 'Y'=as.numeric(inv$Latitude), 'D'=as.numeric(inv$DBH_Avg_CM))
  df = na.omit(df)
  return(df)
}

xx <- lapply(aois, predictdbh, fidir, shapedir)
plotdata <- rbindlist(xx)
coef <- lm(D ~ log(Z), data=plotdata)
resid <- resid(coef)
summary(coef)
plot(coef)

plot(plotdata$Z, plotdata$D)
plot(plotdata$Z, resid)
abline(0,0)
plot(1:50, -8.1946+12.2768*log(1:50), type='l')
points(plotdata$Z, plotdata$D)

# On one plot
xx <- predictdbh(fidir, shapedir, sgtrees, aoi)
coef <- lm(D ~ log(Z), data=xx)
resid <- resid(coef)
summary(coef)
plot(xx$Z, xx$D)
plot(xx$Z, resid)
abline(0,0)
