# Individual tree crown segmentation on LiDAR points
# Author: Marshall Worsham | worsham@berkeley.edu.
# Created: 04-08-21
# Revised: 06-30-22

#########################
# Front matter
########################

# Install and load libraries
pkgs <- c('dplyr',
          'tidyverse',
          'ggplot2',
          'raster',
          'devtools',
          'plotly',
          'rPeaks',
          'rgdal',
          'caTools',
          'sf', 
          'terra',
          'parallel',
          'itcSegment',
          'lidR',
          'rlas',
          'rgl',
          'nngeo',
          'broom',
          'plot3D',
          'readxl') # Name the packages you want to use here

# Function to install new packages if they're not already installed
load.pkgs <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 

# Runs the function on the list of packages defined in pkgs
load.pkgs(pkgs)
load_all('~/Repos/rwaveform')

# Name directories
#datadir <- '/global/scratch/users/worsham/geolocated_returns_plots'
scrdir <- file.path('/global', 'scratch', 'users', 'worsham')
datadir <- file.path(scrdir, 'las_decimated2')
regriddir <- file.path(scrdir, 'las_regridded')
shapedir <- file.path(scrdir, 'EastRiverInputs', 'Plot_Shapefiles', 'AllPlots')
fidir <- file.path(scrdir, 'EastRiverInputs', 'Inventory_Plots')
gpsdir <- file.path(scrdir, 'EastRiverInputs', 'StemGeolocations')
outdir <- file.path(scrdir, 'trees')
dir.create(outdir)

#########################################
# Ingest points from waveform processing
########################################

#ptcsv <- list.files(datadir, full.names = T)
#aois <- list.files(datadir, full.names = F)
#aois <- unique(sapply(strsplit(aois, '_2018'), '[', 1))

infiles <- list.files(datadir, full.names=T)
lascat <- readLAScatalog(infiles)

plotsf <- st_read(list.files(shapedir, pattern='.shp', full.names=T))
aois <- plotsf$PLOT_ID

plot(lascat)
plot(plotsf, add=T, col='red')

las1 <- readLAS(infiles[1200])
plot(extent(las1))
plot(extent(las1)-5, col='red', add=T)
ex <- ext(las1)-5 
las1c <- clip_rectangle(las1, ex[1], ex[2], ex[3], ex[4])
plot(las1, bg='white')
rglwidget()

plot(extent(las1), col='red')
plot(extent(las2), col='blue', add=T)

##########################################
# ITC segmentation
##########################################
aoi <-  'SG-NES2'

getcrowns <- function(aoi){
outtmp <- tempdir()
aoipts <- write.csv(do.call('rbind', lapply(ptcsv[grep(aoi, ptcsv)], read.csv)), file.path(outtmp, 'aoi.csv'))
pts2las(list.files(outtmp, full.names=T)[grep('aoi.csv', list.files(outtmp))], outtmp)
testlas <- readLAS(list.files(outtmp, full.names=T)[grep('aoi.las', list.files(outtmp))])
plot(testlas)
rglwidget()

# Pull points in plot bound
plotsf <- plotsf[plotsf$PLOT_ID==aoi,][1]
testgeom <- st_buffer(plotsf$geometry, 100)
subset1 <- clip_roi(lascat, plotsf)
plot(subset1, bg='white', size=2)
rglwidget()

chm <- rasterize_canopy(subset1, 0.5, pitfree(subcircle = 0.2))
plot(chm, col = height.colors(50))

sgtrees <- locate_trees(subset1, lmf(ws = 2.2))
plot(sf::st_geometry(sgtrees), add = TRUE, pch = '+')

x <- plot(subset1, bg = "white", size = 4)
add_treetops3d(x, sgtrees)
rglwidget()
st_crs(sgtrees) <- 32613
algo <- dalponte2016(chm, sgtrees, th_tree=2, th_seed=0.15)
algo <- li2012()

segtrees <- segment_trees(subset1, algo) # segment point cloud

plot(segtrees, bg = "white", size = 4, color = "treeID") # visualize trees
rglwidget()

length(unique(segtrees@data$treeID))
crowns <- crown_metrics(segtrees, func = .stdtreemetrics, geom = "convex")
st_is_valid(crowns)
crowns <- st_make_valid(crowns)

plot(sf::st_geometry(crowns), reset = FALSE)
plot(chm)
plot(crowns["convhull_area"], main = "Crown area (convex hull)", col=NA, add=T)
plot(algo(), col = pastel.colors(460))
crowns
plot(plotsf, add=T)
xx <- predictloss(fidir, shapedir, sgtrees, aoi)

?crown_metrics

itcfun <- function(aoi){
  olas = pts2las(datadir, aoi)
  print(dim(olas))
  oft = itd(olas, aoi)
  print(dim(oft))
  loss = predictloss(fidir, shapedir, oft, aoi)
  return(loss)
}

itcfun('CC-CVS1')

mloss = lapply(aois[c(10)], itcfun)

aois



###########
# DEBUGGING
###########
itcdelineate <- function(nlas, aoi, algo='li2012'){
  
  # Get plot boundary for aoi
  plotpath = list.files(shapedir, 
                        pattern = glob2rx(paste0(aoi,"*shp")),
                        full.names = T)
  plotsf = vect(plotpath)
  geoextent = as.list(ext(plotsf))
  
  # Run itcSegment delineation algorithm
  if(algo == 'dalponte2016'){
    chm = grid_canopy(nlas, 0.5, pitfree(subcircle = 0.4))
    f = function(x) {
      y <- 2.2 * (-(exp(-0.08*(x-2)) - 1)) + 3
      y[x < 2] <- 3
      y[x > 20] <- 7
      return(y)
    }
    ft = find_trees(nlas, lmf(f))
    alg = dalponte2016(chm,
                       ft, 
                       th_tree = 1.5, 
                       th_seed = 0.01, 
                       th_cr = 0.05,
                       max_cr = 12)
    itc = segment_trees(nlas, alg)
    plot(itc, bg = "white", size = 4, color = "treeID") # visualize trees
    crowns = delineate_crowns(itc)
  }
  
  if(algo == 'li2012'){
    alg = li2012(dt1 = 0.8, 
                 dt2 = 2, 
                 R = 1, 
                 Zu = 12, 
                 hmin = 1, 
                 speed_up = 6.5)
    itc = segment_trees(nlas, alg) # segment point cloud
    plot(itc, bg = "white", size = 4, color = "treeID") # visualize trees
    crowns = delineate_crowns(itc, 'convex')
  }
  
  #   
  # itc = itcLiDAR(X = ld$X, 
  #                Y = ld$Y, 
  #                Z = ld$Z, 
  #                epsg=26913, 
  #                resolution = 0.3, 
  #                MinSearchFilSize = 3,
  #                MaxSearchFilSize = 5, 
  #                TRESHSeed = 0.6,
  #                TRESHCrown = 0.85,
  #                minDIST = 1.1,
  #                maxDIST = 9,
  #                HeightThreshold = 1.5,
  #                cw = 1)
  # 
  
  clipitc = raster::crop(crowns, plotsf)
  #plot(clipitc)
  #destdir = '~/Desktop/RMBL/Projects/Watershed_Spatial_Dataset/Output/itc'
  #writeOGR(obj=clipitc, dsn=destdir, layer=paste0(aoi, '_itc'), driver="ESRI Shapefile")
  
  return(clipitc)
}

xx <- itcdelineate(testlas, aoi, 'li2012')
xx <- segment_trees(testlas, watershed())
xx
plot(xx)
