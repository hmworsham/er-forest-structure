# Individual tree crown segmentation on LiDAR points
# Author: Marshall Worsham | worsham@berkeley.edu.
# Created: 04-08-21
# Revised: 03-02-22

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
datadir <- '/global/scratch/users/worsham/geolocated_returns_plots'
shapedir <- '/global/scratch/users/worsham/EastRiver/Plot_Shapefiles/Polygons/'
outdir <- '/global/scratch/users/worsham/trees'
fidir <- '/global/scratch/users/worsham/EastRiver/Inventory_Plots'
gpsdir <- '/global/scratch/users/worsham/EastRiver/StemGeolocations'

#########################################
# Ingest points from waveform processing
########################################

ptcsv <- list.files(datadir, full.names = T)
aois <- list.files(datadir, full.names = F)
aois <- unique(sapply(strsplit(aois, '_2018'), '[', 1))

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
cg <- classify_ground(testlas, mcc())
gn <- normalize_height(cg, tin())

# Get plot boundary for aoi
plotpath = list.files(shapedir,
                      pattern = glob2rx(paste0(aoi,"*shp")),
                      full.names = T)
plotsf = vect(plotpath)
#gn <- clip_polygon(gn, geom(plotsf)[,3], geom(plotsf)[,4])
plot(gn)
rglwidget()

chm <- rasterize_canopy(gn, 0.5, pitfree(subcircle = 0.2))
plot(chm, col = height.colors(50))
sgtrees <- itd(gn, aoi)
sgtrees
plot(sf::st_geometry(sgtrees), add = TRUE, pch = 3)

x <- plot(gn, bg = "white", size = 4)
add_treetops3d(x, sgtrees)
rglwidget()

algo <- dalponte2016(chm, sgtrees)
#algo <- li2012()
segtrees <- segment_trees(gn, algo) # segment point cloud
plot(segtrees, bg = "white", size = 4, color = "treeID") # visualize trees
rglwidget()
length(unique(segtrees@data$treeID))
crowns <- crown_metrics(segtrees, func = .stdtreemetrics, geom = "convex")
st_is_valid(crowns)
crowns <- st_make_valid(crowns)


getcrowns(aoi)

lapply(aois, getcrowns)

plot(crowns["convhull_area"], main = "Crown area (convex hull)")
plot(algo(), col = pastel.colors(460))
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
