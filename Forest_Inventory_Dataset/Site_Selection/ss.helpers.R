get.rasters = function(x, dir){
  xpath = file.path(dir, x)
  xtif = list.files(xpath, pattern = 'tif$', full.names = T)
  xras = lapply(xtif, rast)
  return(xras)
}

makepolys <- function(input, radius, shape='rectangle'){
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
  
  return(sites)
}

zonals <- function(input, ras.source, topo.inputs, type=c('coord', 'sf'), radius, shape='rectangle') {
  
  toporasters = flatten(lapply(topo.inputs, get.rasters, ras.source))
  
  # If it's a coordinate or list of coordinates...
  if(type == 'coord'){
    xy = input[,c(2,3)]
    spdf = vect(xy,
                input,
                proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    sites = terra::project(spdf, crs(toporasters[[1]]))
    
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
    proj4string = crs(toporasters[[1]]))
    
    site_names = names(sites)
  }
  
  # If it's a shapefile... 
  else if(type == 'sf'){
    sites = vect(input)
    sites = terra::project(sites, crs(toporasters[[1]]))
    site_names = sites$PLOT_ID
  }
  
  # Plot the coordinates on top of the DEM
  plot(toporasters[[2]])
  plot(sites, add = T)
  
  # Extract values from specified factors
  topovals = lapply(toporasters, terra::extract, sites, fun = mean)
  
  # Return the values
  topovals.df = do.call('cbind', (lapply(topovals, '[', 2)))
  colnames(topovals.df) <- sapply(toporasters, names)
  rownames(topovals.df) <- site_names
  
  return(topovals.df)
}

# Degrees to radians
d2r <- function(a) a * pi / 180

# Radians to degrees
r2d <- function(a) a * 180 / pi

# Which direction a slope is facing
facing <- function(slope,aspect,focal=180,unit='rad') {
  if (unit %in% c('rad','deg')) {
    if (unit=='deg') {
      slope <- d2r(slope)
      aspect <- d2r(aspect)
    }
    aspect <- d2r(focal) - aspect
    return(sin(slope) * cos(aspect))
  } else print('unit must be rad or deg')
}

# Eastness, northness, southness
eastness <- function(slope,aspect,unit='deg') facing(slope,aspect,focal=90,unit=unit)
northness <- function(slope,aspect,unit='deg') facing(slope,aspect,focal=0,unit=unit)
southness <- function(slope,aspect,unit='deg') facing(slope,aspect,focal=180,unit=unit)
adjsouthness <- function(slope, aspect, unit='deg') facing(slope,aspect,focal=205, unit=unit)

# Folded aspect
foldaspect <- function(x,f=fold) abs(180-abs(x-f))

# Total potential heat load
thl <- function(L, A, S, unit='deg', fold=180) {
  d2r <- function(x) 2*pi*x/360
  r2d <- function(x) 360*x/(2*pi)
  if (unit=='deg') {
    A <- foldaspect(A,fold)
    L <- d2r(L)
    A <- d2r(A)
    S <- d2r(S)
  } else {
    atmp <- r2d(A)
    atmp <- foldaspect(atmp,fold)
    A <- d2r(atmp)
  }
  # EQN 3, McCune and Keon 2002 JVS, latitude > 30‚ slope < 60°
  return(0.339+0.808*cos(L)*cos(S)-0.196*sin(L)*sin(S)-0.482*cos(A)*sin(S))
}

