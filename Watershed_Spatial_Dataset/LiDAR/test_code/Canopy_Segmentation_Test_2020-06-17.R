##Script to run canopy segmentation on small area in East River domain

##Author: Marshall Worsham
##Updated: 6-17-2020

# Set up workspace
## Install and load libraries
pkgs <- c('dplyr',
          'tidyverse',
          'ggplot2',
          'raster',
          'rgdal',
          'sf',
          'ggspatial',
          'rasterVis',
          'XML',
          'RCurl',
          'googledrive',
          'ForestTools') # Name the packages you want to use here
load.pkgs <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} # Function to install new packages if they're not already installed
load.pkgs(pkgs) # Runs the function on the list of packages defined in pkgs

# Set working directory.
setwd('~/Desktop/RMBL/Projects/Watershed_Spatial_Dataset/')
outdir <- paste0(getwd(), '/Output')

##Check GDAL version (should work with GDAL 2.2.3 or later).
getGDALVersionInfo()

## Get list of files on AWS
sdp.cat.xml <- xmlParse(getURL('https://rmbl-sdp.s3.us-east-2.amazonaws.com'))
sdp.cat.list <- xmlToList(sdp.cat.xml)
unlisty <- data.frame(contents = unlist(sdp.cat.list))

## Subset the geotiff files
tifs <- unlisty %>%
  filter(grepl('release1', contents),
         grepl('tif', contents)) %>%
  slice(-c(16:20))
tifs

## Subset the metdata xml files
metas <- unlisty %>%
  filter(grepl('release1', contents),
         grepl('xml', contents)) %>%
  slice(-c(16:20))
metas

##Creates raster objects from cloud-based datasets.
basepath <- "/vsicurl/https://rmbl-sdp.s3.us-east-2.amazonaws.com/"
tifs$full_path <- paste0(basepath, tifs$contents)
sdp.tifs <- list(tifs$full_path)[[1]]

metas$full_path <- paste0(basepath, metas$contents)
metas$full_path

##Select canopy height tif and make it a raster
tifs$contents
canopy.ht <- raster(sdp.tifs[5])

###########################################################################
## Subset to Schofield region
###########################################################################

# Define a circle around Schofield-24 as AOI

scho24.pts <- st_read('~/Google Drive (worsham@berkeley.edu)/Research/RMBL/RMBL_East River Watershed Forest Data/Data/Inventory Plots/Scho_24/Scho_24_Boundary/Scho_24_bnd_pts_UTM13N.shp')

scho24.poly <- st_read('~/Google Drive (worsham@berkeley.edu)/Research/RMBL/RMBL_East River Watershed Forest Data/Data/Inventory Plots/Scho_24/Scho_24_Boundary/Scho_24_bnd_poly_UTM13N.shp')

scho24.gps.trees <- st_read('~/Google Drive (worsham@berkeley.edu)/Research/RMBL/RMBL_East River Watershed Forest Data/Data/Inventory Plots/Sarah_temp/Three census plots/POWELLT All GIS Data/POWELLT082413A-TREES/Point_ge_Project.shp')

scho24.gps.trees2 <- st_read('~/Google Drive (worsham@berkeley.edu)/Research/RMBL/RMBL_East River Watershed Forest Data/Data/Inventory Plots/Sarah_temp/Three census plots/POWELLT All GIS Data/POWELLT082411A/Point_ge_Project.shp')

scho24.gps.trees3 <- st_read('~/Google Drive (worsham@berkeley.edu)/Research/RMBL/RMBL_East River Watershed Forest Data/Data/Inventory Plots/Sarah_temp/Three census plots/POWELLT All GIS Data/POWELLT082509A-SCHO_24/Point_ge_Project.shp')

scho24.gps.trees4 <- st_read('~/Google Drive (worsham@berkeley.edu)/Research/RMBL/RMBL_East River Watershed Forest Data/Data/Inventory Plots/Sarah_temp/Three census plots/POWELLT All GIS Data/POWELLT082510B-SCHO_24/Point_ge_Project.shp')

scho24.gps.trees5 <- st_read('~/Google Drive (worsham@berkeley.edu)/Research/RMBL/RMBL_East River Watershed Forest Data/Data/Inventory Plots/Sarah_temp/Three census plots/POWELLT All GIS Data/POWELLT082510A/Point_ge_Project.shp')

scho24.gps.trees6 <- st_read('~/Google Drive (worsham@berkeley.edu)/Research/RMBL/RMBL_East River Watershed Forest Data/Data/Inventory Plots/Sarah_temp/Three census plots/POWELLT All GIS Data/POWELLT082609A-SCH_24/Point_ge_Project.shp')

scho24.trees <- rbind(scho24.gps.trees, scho24.gps.trees2, 
                                        scho24.gps.trees3,
                                        scho24.gps.trees4,
                                        scho24.gps.trees5,
                                        scho24.gps.trees6)

View(scho24.trees)

#scho19 <- st_read('~/Google Drive (worsham@berkeley.edu)/Research/RMBL/RMBL_East River Watershed Forest Data/Data/Inventory Plots/Scho_19/Scho_19_Boundary')

#scho23 <- st_read('~/Google Drive (worsham@berkeley.edu)/Research/RMBL/RMBL_East River Watershed Forest Data/Data/Inventory Plots/Scho_23/Scho_23_Boundary')

scho.pts <- data.frame(long = as.list(extent(scho24.pts))[1], lat = as.list(extent(scho24.pts))[3])
scho.sf <- st_as_sf(scho.pts, coords = c('long', 'lat'), crs = 'EPSG:32613')
scho.circ <- st_buffer(scho.sf, dist = 500)

# Subset maps to Schofield AOI
scho.canopyht <- crop(canopy.ht, scho24.poly, filename=tempfile(), 
                   progress="text")

###########################################################################
## Plotting 
###########################################################################

## Canopy height
canopy.plot <- gplot(canopy.ht, maxpixels=500000)+
  geom_raster(aes(fill=value), interpolate=TRUE) +
  scale_fill_gradient(low = 'snow2', high = 'springgreen4', name = 'Canopy Height [m]') +
  scale_x_continuous("") +
  scale_y_continuous("") +
  #layer_spatial(scho24, color = 'tomato2', fill = NA) +
  coord_sf(expand=0, label_axes="--EN") +
  annotation_scale(style="ticks",width_hint=0.1,location="br",
                   tick_height=0,text_col="black",line_col="black") +
  theme_bw() +
  theme(legend.position='bottom',
        legend.key.width = unit(2, 'lines'))

canopy.plot

## Segmentation

# Define a function that determines the dynamic window size
lin <- function(x){x * .001 + .001}
ttops <- vwf(CHM = scho.canopyht, winFun = lin, minHeight = 2)

# Plot CHM
plot(scho.canopyht, xlab = "", ylab = "", xaxt='n', yaxt = 'n')

# Add dominant treetops to the plot
plot(ttops, col = "blue", pch = 20, cex = 0.5, add = TRUE)

# Plot crowns
#crowns <- mcws(treetops = ttops, CHM = scho.canopyht, minHeight = 1.5, verbose = FALSE)
#plot(crowns, col = sample(gray.colors(10), length(unique(crowns[])), replace = TRUE), legend = FALSE, xlab = "", ylab = "", xaxt='n', yaxt = 'n')

# Plot outlines of tree canopies
crownsPoly <- mcws(treetops = ttops, CHM = scho.canopyht, format = 'polygons', minHeight = 1.5, verbose = FALSE)
plot(crownsPoly, border = 'blue3', lwd = 0.25, add = T)

# Plot GPS-tagged tree locations
plot(scho24.poly$geometry, border = 'grey20', lwd = 1, add = T)
plot(scho24.trees$geometry, col = 'red', pch = 20, cex = 0.5, add = T)

sp_summarise(crownsPoly, variables = c('crownArea', 'height'))




