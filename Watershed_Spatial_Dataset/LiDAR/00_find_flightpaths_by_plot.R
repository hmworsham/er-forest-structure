# R Script to find 2018 NEON LiDAR flightpaths that intersect Kueppers 2020 forest inventory plots

# Install and load libraries
pkgs <- c('dplyr',
          'tidyverse',
          'ggplot2',
          'rgdal',
          'caTools',
          'sf') # Name the packages you want to use here

# Function to install new packages if they're not already installed
load.pkgs <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
# Runs the function on the list of packages defined in pkgs
load.pkgs(pkgs)

###################
# Ingest plots 
###################

# Name directory where shapefiles live
#allplotsdir <- '/Volumes/GoogleDrive/My Drive/Research/RMBL/RMBL-East River Watershed Forest Data/Data/Geospatial/Kueppers_EastRiver_Plot_Shapefiles_WGS84UTM13N/AllPlots'
#sfdir <- '/Volumes/GoogleDrive/My Drive/Research/RMBL/RMBL-East River Watershed Forest Data/Data/Geospatial/Kueppers_EastRiver_Plot_Shapefiles_WGS84UTM13N/Polygons'
allplotsdir <- '/global/scratch/users/worsham/EastRiver/Plot_Shapefiles/AllPlots'
sfdir <- '/global/scratch/users/worsham/EastRiver/Plot_Shapefiles/Polygons'

# Read in the shapefile containing all plots
allplots_path = list.files(allplotsdir, 
                    pattern = glob2rx(paste0('*AllPlots*',"*shp")),
                    full.names = T)
allplots <- st_read(allplots_path, quiet=T)

######################################
# Find flightpaths that contain plots
######################################

# Name data directory
#datadir <- '/Volumes/GoogleDrive/.shortcut-targets-by-id/1xCDkpB9tRCZwEv2R3hSPKvGkQ6kdy8ip/waveformlidarchunks'
#datadir <- '/Volumes/GoogleDrive/My Drive/Research/RMBL/RMBL-East River Watershed Forest Data/Data/LiDAR/waveformlidar'
datadir <- '/global/scratch/users/worsham/waveform_binary_chunks'

# Name flightpaths as filenames
flightpaths <- list.dirs(datadir,recursive = F)
#flightpaths <- flightpaths[2:length(flightpaths)] 

# Make a dataframe depicting all intersects between plots and flightpaths
# Define empty dataframe to store intersection decisions
fp_plot_itx <-  data.frame(matrix(NA, 
                                  nrow = length(flightpaths), 
                                  ncol = nrow(allplots)))
names(fp_plot_itx) <- allplots$PLOT_ID
row.names(fp_plot_itx) <- sapply(flightpaths, function(x){strsplit(x, '/')[[1]][7]})

# Find flightpath chunk boundaries and check for intersections with allplots
# for(i in seq(length(flightpaths))){
#   fp_objects = gcs_list_objects(prefix = flightpaths[i])
#   geols = grep(fp_objects$name, 
#                  pattern = 'geolocation', 
#                  value = T)
#   geo_bin <- gcs_get_object(geols[1], saveToDisk = "geo")
#   geo_bin_file <- file.path(getwd(), 'geo')
#   parsefn <- function(object){
#     httr::content(object, 'text', 'text/plain', 'UTF-8')
#   }
#   geo_hdr <- gcs_get_object(geols[2], parseFunction = parsefn, saveToDisk = "geo_hdr.hdr")
#   geo_hdr_file <- file.path(getwd(), 'geo_hdr.hdr')
#   geolo = read.ENVI(geo_bin_file, headerfile = geo_hdr_file)
#   if(file.exists(c(geo_bin_file, geo_hdr_file))){
#     file.remove(c(geo_bin_file, geo_hdr_file))
#   }
#   xcoords = c(min(geolo[,1]), max(geolo[,1]), max(geolo[,1]), min(geolo[,1]), min(geolo[,1]))
#   ycoords = c(max(geolo[,2]), max(geolo[,2]), min(geolo[,2]), min(geolo[,2]), max(geolo[,2]))
#   xym = cbind(xcoords, ycoords)
#   sps = st_sfc(st_polygon(list(xym)))
#   st_crs(sps) = CRS('+init=epsg:32613')
#   itx = st_intersects(allplots, sps, sparse = F)
#   fp_plot_itx[i,] = itx
# }
# 
# View(fp_plot_itx)

for(i in seq(length(flightpaths))){
  geo_files = grep(list.files(flightpaths[i], full.names = T), # Geolocation array
                 pattern = 'geolocation',
                 value = T)
  geo_bin = geo_files[1]
  geo_hdr = geo_files[2]
  geolo = read.ENVI(geo_bin, headerfile = geo_hdr)
  xcoords = c(min(geolo[,1]), max(geolo[,1]), max(geolo[,1]), min(geolo[,1]), min(geolo[,1]))
  ycoords = c(max(geolo[,2]), max(geolo[,2]), min(geolo[,2]), min(geolo[,2]), max(geolo[,2]))
  xym = cbind(xcoords, ycoords)
  extent = data.frame(xmin = min(xcoords), xmax = max(xcoords), ymin = min(ycoords), ymax = max(ycoords))
  write.table(extent, file="~/flightpath_chunk_extents.csv", sep = ',', append=T, col.names = F, row.names = F)
  sps = st_sfc(st_polygon(list(xym)))
  st_crs(sps) = CRS('+init=epsg:32613')
  itx = st_intersects(sps, allplots, sparse = F)
  fp_plot_itx[i,] = itx
}

fp_plot_itx

# find_fps <- function(aoi){
#   
#   # Specify a plot of interest
#   plotpath = list.files(shapedir, 
#                         pattern = glob2rx(paste0(aoi,"*shp")),
#                         full.names = T)
#   plotsf = st_read(plotpath, quiet=T)
#   geoextent = as.list(extent(plotsf))
#   #print(geoextent)
#   
#   # Clip the waveforms that intersect the aoi
#   for(i in seq(length(flightpaths))){
#     geo_files = grep(list.files(flightpaths[i], full.names = T),
#                      pattern = 'geolocation',
#                      value = T)
#     geo_bin = geo_files[1]
#     geo_hdr = geo_files[2]
#     geolo = read.ENVI(geo_bin, headerfile = geo_hdr)
#     
#     fp_maxx = max(geolo[,1])
#     fp_minx = min(geolo[,1])
#     fp_maxy = max(geolo[,2])
#     fp_miny = min(geolo[,2])
#   
#     plotinfp = geoextent[1] >= fp_minx & geoextent[2] <= fp_maxx & geoextent[3] >= fp_miny & geoextent[4] <= fp_maxy
#     
#     fp_plot_itx[i, aoi] <- plotinfp
#   }
#   # waveform1 = wf[,-1]
#   # colnames(geol)[2:9] = c('x', 'y', 'z', 'dx', 'dy', 'dz', 'or', 'fr')
#   # ll = apply(waveform1, 1, wavelen)
#   # x = geol$x + geol$dx*(round(ll/2)-geol$fr) # use the middle point to represent the waveform position
#   # y = geol$y + geol$dy*(round(ll/2)-geol$fr)
#   # ind = which (x >= geoextent[1] & x<= geoextent[2] & y >= geoextent[3] & y<= geoextent[4])
#   # swaveform = wf[ind,]
#   # 
#   # return(swaveform)
#   return(fp_plot_itx)
# }

write.csv(fp_plot_itx, '~/EastRiver_Plot_LiDAR_Chunk_Intersections.csv')
