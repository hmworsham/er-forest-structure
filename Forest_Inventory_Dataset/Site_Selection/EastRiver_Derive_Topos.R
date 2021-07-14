# EastRiver_Derive_Topos
# Calculates the values of various topographic factors for rectangular 40x40 m neighborhoods around specified coordinates and/or within the extents of forest inventory plots in the East River watershed.

# Author: Marshall Worsham
# Created: 10-06-20
# Revised: 07-11-21

#############################
# Set up workspace
#############################

# Install and load libraries
pkgs <- c('dplyr',
          'tidyverse',
          'ggplot2',
          'data.table', 
          'raster', 
          'sf')

# Name the packages you want to use here
load.pkgs <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} # Function to install new packages if they're not already installed
load.pkgs(pkgs) # Runs the function on the list of packages defined in pkgs

# Set working directory.
setwd(file.path('~', 'Desktop', 'RMBL', 'Projects', fsep = '/'))
fidir <- file.path(getwd(), 'Forest_Inventory_Dataset', 'Output', fsep = '/')
wsdir <- file.path(getwd(), 'Watershed_Spatial_Dataset', 'Source', fsep = '/')
rasdir <- file.path('..', '..', '..', 'Google Drive (worsham@berkeley.edu)', 'Research', 'RMBL', 'RMBL_East River Watershed Forest Data', 'Data', 'Geospatial', 'Worsham_2021_SiteSelection', '2021_Analysis_Layers', 'ASO_Snow-Free_DEM')
sfdir <- file.path('~', 'Google Drive (worsham@berkeley.edu)', 'Research', 'RMBL/RMBL_East River Watershed Forest Data', 'Data', 'Geospatial')

#############################
# Ingest raster data
#############################

topo.factors <- c('Aspect', 
                  'Curvature',
                  'DTM', 
                  'Flow_Accumulation',
                  'Slope', 
                  'Solar_Radiation', 
                  'TPI', 
                  'TWI', 
                  'Upslope_Contributing_Area')

get.rasters = function(x){
  xpath = file.path(rasdir, x)
  xtif = list.files(xpath, pattern = 'tif$', full.names = T)
  xras = lapply(xtif, raster)
  return(xras)
}

zonals <- function(input, type=c('coord', 'sf'), radius, shape='rectangle'){
  
  toporasters = flatten(lapply(topo.factors, get.rasters))
  
  # If it's a coordinate or list of coordinates...
  if(type == 'coord'){
    xy = input[,c(2,3)]
    spdf = SpatialPointsDataFrame(xy, 
                                  input, 
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    sites = spTransform(spdf, crs(toporasters[[1]]))
    
    # Add extent points based on radius to centroid
    yPlus <- sites$Latitude+radius
    xPlus <- sites$Longitude+radius
    yMinus <- sites$Latitude-radius
    xMinus <- sites$Longitude-radius
    
    # Create squares using extent points
    square=cbind(xMinus,yPlus,  # NW corner
                 xPlus, yPlus,  # NE corner
                 xPlus,yMinus,  # SE corner
                 xMinus,yMinus, # SW corner
                 xMinus,yPlus)  # NW corner again - close ploygon
    
    # Create polygons with polys
    ID=input$Site_ID
    sites <- SpatialPolygons(mapply(function(poly, id)
    {
      latlon <- matrix(poly, ncol=2, byrow=TRUE)
      Polygons(list(Polygon(latlon)), ID = id)
    },
    split(square, row(square)), ID),
    proj4string=crs(toporasters[[1]]))
    
    site_names = names(sites)
  }
  
  # If it's a shapefile... 
  else if(type == 'sf'){
    sites = st_read(input, quiet = T)
    sites = st_transform(sites, crs(toporasters[[1]]))
    site_names = sites$PLOT_ID
  }
  
  # Plot the coordinates on top of the DEM
  plot(toporasters[[4]])
  plot(sites, add = T)
  
  # Extract values from specified factors
  topovals = sapply(toporasters, raster::extract, sites, fun = mean)
  
  # Return the values
  topovals.df = as.data.frame(topovals)
  colnames(topovals.df) <- sapply(toporasters, names)
  rownames(topovals.df) <- site_names
  
  return(topovals.df)
}

#############################
# Ingest source data
#############################

# Ingest 2020 Kueppers plot characteristics CSVs
siteinfo20 <-  read.csv(file.path(fidir, 'Kueppers_EastRiver_Final_Sites_2020.csv'), header = T)

# Ingest 2020-2021 Kueppers plot info
siteinfo21 <- read.csv(file.path(fidir, 'EastRiver_ProposedSites_2021_25.csv'), header = T)

# Refactor a couple of columns in siteinfo20 to row bind with 2021 data
siteinfo20 <- rename(siteinfo20, Site_ID = SFA_ID)
#siteinfo20$Established <- as.factor(siteinfo20$Established)
siteinfo20$Established <- 'Established'


coords <- siteinfo20[,c(1,5:6)]
plots20 <- list.files(file.path(sfdir, 
                                'Kueppers_EastRiver_Plot_Shapefiles_2020_WGS84UTM13N', 
                                'AllPlots'), pattern = 'shp', full.names = T)

plots21 <- list.files(file.path(sfdir,
                     'Worsham_2021_SiteSelection', 
                     '2021_Proposed_Sites_All',
                     'Kueppers_EastRiver_ProposedSites_2021_25'),
                     pattern = 'shp', full.names = T)

newcoords <- data.frame(
  Site_ID = c('emerald','emerald2', 'cement 28', 'walrod1', 'walrod2'), 
  Longitude = c(-107.047670, -107.047914, -106.8253285, -106.846080, -106.8472989), 
  Latitude = c(39.010148, 39.014249, 38.8274440, 38.828859, 38.8292840))


coords
newcoords

zon20 <- zonals(plots20, type = 'sf')
zon21 <- zonals(plots21, type = 'sf')
zonnew <- zonals(newcoords, type = 'coord', radius = 20)
fullset <- rbind(zon20, zon21, zonnew)
View(fullset)
