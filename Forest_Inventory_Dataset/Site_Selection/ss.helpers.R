get.rasters = function(x, dir){
  xpath = file.path(dir, x)
  xtif = list.files(xpath, pattern = 'tif$', full.names = T)
  xras = lapply(xtif, rast)
  return(xras)
}

makepolys <- function(input, radius, shape='rectangle', target.crs=32613){
  xy = input[,c(2,3)]
  xy = st_as_sf(xy, coords=c('Longitude', 'Latitude'), crs=4326)
  sites = st_transform(xy, target.crs)
  
  # Create polygons with polys
  ID = input$Location_ID
  sites = st_buffer(sites, dist=radius, endCapStyle = 'SQUARE', joinStyle = 'MITRE')
  site_names = input$Location_ID
  
  return(sites)
}

zonals <- function(input, ras.source, topo.inputs, type=c('coord', 'sf'), radius, shape='rectangle') {
  
  toporasters = flatten(lapply(topo.inputs, get.rasters, ras.source))
  
  # If it's a coordinate or list of coordinates...
  if(type == 'coord'){
    xy = input[,c(2,3)]
    xy = st_as_sf(xy, coords=c('Longitude', 'Latitude'), crs=4326)
    sites = st_transform(xy, crs(toporasters[[1]]))
    
    # Create polygons with polys
    ID = input$Site_ID
    sites = st_buffer(sites, dist=radius, endCapStyle = 'SQUARE', joinStyle = 'MITRE')
    sites = vect(sites)
    site_names = input$Site_ID
  }
  
  # If it's a shapefile... 
  else if(type == 'sf'){
    sites = vect(input)
    sites = terra::project(sites, crs(toporasters[[1]]))
    site_names = sites$PLOT_ID
  }
  
  # Plot the coordinates on top of the DEM
  #plot(toporasters[[2]])
  #plot(sites, color='blue', size=20, add = T)
  
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


print.figs <- function(df){
  #'''
  # Function
  # Input: a dataframe of sites and topographic variables
  # Returns: a set of png files 
  #'''
  
  # Define colors
  colors = c("#3B9AB2",
             "#E1AF00",
             "#F21A00",
             "#78B7C5",
             "#00A08A",
             "#F98400",
             "#4DA64D",
             "#DFB3F2", 
             "#EBCC2A",
             "#2A2A2A")
  
  varnames = c('Elevation (m)', 
               'Slope angle (º)', 
               'Aspect (º)', 
               expression(Heat~Load~(MJ~cm^-2~y^-1)),
               'Aspect Folded on 205º (º)',
               'Southness Adjusted to 205º (º)',
               'TWI [100 m]',
               'TWI [1000 m]',
               'TPI [1000 m]', 
               'TPI [2000 m]')
  
  # Refactor Established 
  df$Established <- factor(df$Established, levels = c('Established', 'Approved', 'Proposed'))
  
  # Loop through variables
  for (t in seq(3,length(df))){
    clr = colors[t-2]
    varname = varnames[t-2]
    
    #LM coeff
    lmv = df[order(df[, t]), t]
    lmi = round(coefficients(lm(lmv ~ seq(1, nrow(df))))[1], 2)
    lms = round(coefficients(lm(lmv ~ seq(1,nrow(df))))[2], 2)
    lmr = round(summary(lm(lmv~seq(1,nrow(df))))$r.squared, 2)
   
    # 1:1 line coeff
    i1 = round(min(df[, t])-(max(df[, t])-min(df[, t]))/(nrow(df)-1), 2)
    s1 = round((max(df[, t])-min(df[, t]))/(nrow(df)-1), 2)
    s2 = 2*(max(df[, t])-min(df[, t]))/(nrow(df)-1)
    
    # Open the png quartz image
    png(file.path(fidir, 
                  'Production_Images', 
                  paste0(names(df[t]),'.png')), 
        width = 12, height = 9, units = 'in', res = 180)
    
    # Print the plot to png
    print(
      ggplot(df, aes(x = reorder(Location_ID,  df[, t]), y = df[, t])) +
        geom_point(aes(color = Elevation_m), size = 5) +
        scale_color_viridis_c(
          name='Elevation',
          limits=c(
            min(df$Elevation_m),
            max(df$Elevation_m)),
          breaks=c(
            round(min(df$Elevation_m),0),
            round(max(df$Elevation_m))),0) +
        #scale_color_manual(values = c(clr, 'grey 30', 'grey 70')) +
        scale_y_continuous(name = varname) +
        labs(x = 'Plot ID', y = names(topos)[t]) +
        theme_light(base_size = 20) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              legend.position="bottom", 
              #legend.title = element_blank(), 
              legend.key.size = unit(1, 'cm')) +
        guides(scale = 'none',
               fill=guide_legend(title="New Legend Title")) +
        geom_abline(intercept=i1,
                    slope=s1,
                    color='black') +
        geom_abline(intercept=lmi, 
                    slope=lms,
                    linetype='dashed',
                    color='black') + 
        geom_text(x=25, 
                  y=min(df[, t]), 
                  label=as.expression(
                    substitute(
                      italic(r)^2~"="~lmr)),
                  hjust=1) +
        geom_text(x=25, 
                  y=min(df[, t])+s1, 
                  label=as.expression(
                    substitute(
                      bold('linreg:')~italic(y)~"="~lmi~"+"~lms*italic(x))),
                  hjust=1) + 
        geom_text(x=25, 
                  y=min(df[, t])+s2, 
                  label=as.expression(
                    substitute(
                      bold('ideal:')~italic(y)~"="~i1~"+"~s1*italic(x))),
                  hjust=1))
    
    
    dev.off()  
  }
}