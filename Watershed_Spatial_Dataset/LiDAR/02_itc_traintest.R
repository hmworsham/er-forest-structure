# Individual tree crown segmentation on LiDAR points
# Author: Marshall Worsham | worsham@berkeley.edu.
# Created: 04-08-21
# Revised: 03-02-22

#########################
# Front matter
########################

# Install non-CRAN libraries
#devtools::install_github("jrminter/rPeaks") # rPeaks for deconv/decomp
#devtools::install_github('tankwin08/waveformlidar') # waveformlidar for general processing
#devtools::install_github('lwasser/neon-aop-package/neonAOP') #neonAOP for reading binary data

# Install and load typical libraries
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
          'parallel',
          'itcSegment',
          'lidR',
          'rlas',
          'broom',
          'plot3D') # Name the packages you want to use here

# Function to install new packages if they're not already installed
load.pkgs <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
# Runs the function on the list of packages defined in pkgs
load.pkgs(pkgs)

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
xx <- read.csv(ptcsv[1], header=T)

aois <- list.files(datadir, full.names = F)
aois <- unique(sapply(strsplit(aois, '_2018'), '[', 1))
aois
##########################################
# ITC segmentation
##########################################
saferead <- function(x){
  tryCatch(read.csv(x, header=T), 
           error = function(cond) {
             message(paste('Reading csv failed'))
           })
}
aoi = 'ER-GT1'

pts2las <- function(aoi){
  
  # Get point cloud for aoi
  pc_csv = list.files(datadir, pattern = aoi, full.names = T)
  pc <- mclapply(pc_csv, saferead, mc.cores=getOption('mc.cores', 16))
  pc <- do.call(rbind.fill, pc)
  pc <- pc[order(pc$px, pc$py),]
  
  
  # Filter outliers and NAs
  #normInt = (abs(pc$pi - mean(pc$pi))/sd(pc$pi))
  #normz = (abs(pc$pz - mean(pc$pz))/sd(pc$pz))
  #pc = pc[which(normInt < 2),]
  #pc = pc[which(normz < 2),]
  pc = pc[which(pc$pi < quantile(pc$pi, 0.95)),]
  pc = pc[which(pc$pz < quantile(pc$pz, 0.99)),]
  pc = pc[which(pc$pz > quantile(pc$pz, 0.01)),]
  pc = na.omit(pc)
  
  # Convert to las and classify ground points before normalizing
  lasdata = data.frame(X = pc$px,
                       Y = pc$py,
                       Z = pc$pz,
                       gpstime = 0.0,
                       Intensity = as.integer(pc$t),
                       ReturnNumber=1L,
                       scale=1L,
                       offset=0,
                       ScanDirectionFlag = 0L,
                       EdgeOfFlightline = 0L,
                       Classification = 0L,
                       ScanAngleRank = 0L,
                       UserData = 0L,
                       PointSourceID = 0L)
  lasheader = header_create(lasdata)
  header_set_epsg(lasheader, 32613)
  lasheader$`X scale factor` = 0.1
  lasheader$`Y scale factor` = 1
  lasheader$`Z scale factor` = 1
  lasheader$`X offset` = 0
  lasheader$`Y offset` = 0
  lasheader$`Z offset` = 0
  
  lasfile = file.path(tempdir(), "temp.las")
  
  # write las file out
  write.las(lasfile, lasheader, lasdata)
  
  # read las file in
  newlas = readLAS(lasfile)
  
  # Classify ground points to create a normalization surface
  ws = seq(3, 9, 3)
  th = seq(0.1, 3, length.out = length(ws))
  gclas = classify_ground(newlas, algorithm = pmf(ws = ws, th = th))
  
  ptdata = gclas@data
  #lidR::plot(gclas, size = 3, bg = "white") 
  #scatter3D(ptdata$X, ptdata$Y, ptdata$Z, colvar = ptdata$Z, clab = 'Elevation', pch = 20, ticktype='detailed')
  
  # Normalize heights to surface
  nlas = normalize_height(gclas, kriging())
  #nlasdata = nlas@data
  #scatter3D(nlasdata$X, nlasdata$Y, nlasdata$Z, colvar = nlasdata$Z, clab = 'Canopy Height', pch = 20, ticktype = 'detailed')
  
  return(nlas)
}

testlas <- pts2las(aoi)

itcdelineate <- function(nlas, aoi){
  
  # Get plot boundary for aoi
  plotpath = list.files(shapedir, 
                          pattern = glob2rx(paste0(aoi,"*shp")),
                          full.names = T)
  plotsf = readOGR(plotpath, verbose = F)
  geoextent = as.list(extent(plotsf))
  
  # Run locate_trees
  f = function(x) {
    y <- 2.2 * (-(exp(-0.08*(x-2)) - 1)) + 3
    y[x < 2] <- 3
    y[x > 20] <- 7
    return(y)
  }
  ft = locate_trees(nlas, lmf(f))
  #st_crs(ft) <- '+proj=utm +zone=13 +ellps=WGS84 +datum=WGS84 +units=m +no_defs' 
  #plot(itc, bg = "white", size = 4, color = "treeID") # visualize trees
  #crowns = delineate_crowns(itc, 'convex')
  
  #ft=mask(ft, plotsf)
  #plot(clipitc)
  #destdir = '~/Desktop/RMBL/Projects/Watershed_Spatial_Dataset/Output/itc'
  #writeOGR(obj=clipitc, dsn=destdir, layer=paste0(aoi, '_itc'), driver="ESRI Shapefile")
  
  return(ft)
}

sgtrees <- itcdelineate(testlas, aoi)

matchtrees <- function(predictrees, aoi) {
  
  # Get plot boundary for aoi
  plotpath = list.files(shapedir, 
                        pattern = glob2rx(paste0(aoi,"*shp")),
                        full.names = T)
  plotsf = readOGR(plotpath, verbose = F)
  geoextent = as.list(extent(plotsf))
  
  
  invfiles = list.files(fidir, pattern = paste0(aoi,'_inventory_data_202'), recursive=T, full.names = T)
  inv = read_excel(invfiles[1], sheet='inventory_data')
  df = data.frame('Z'=as.numeric(inv$Height_Avg_M), 'X'=as.numeric(inv$Longitude), 'Y'=as.numeric(inv$Latitude))
  df = na.omit(df)
  
  st_crs(predictrees) <- '+proj=utm +zone=13 +ellps=WGS84 +datum=WGS84 +units=m +no_defs' 
  y = st_as_sf(df, coords = c('X', 'Y'), crs = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
  y = st_transform(y, '+proj=utm +zone=13 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
  
  plot(plotsf, axes=T)
  plot(st_geometry(y), pch='+', col='lightblue4', add=T)
  plot(st_geometry(predictrees), pch='+', add=TRUE, col='firebrick2')
  nn = st_nn(predictrees, y, k=1, returnDist=T)
  legend(legend=c('Field trees', 'Modeled trees'), col=c('lightblue4', 'firebrick2'), pch='+')
  
  dists = unlist(nn$dist)
  nns = unlist(nn$nn)
  delta.ht = c()
  delta.dist = c()
  misses = c()
  for(el in seq(length(dists))){
    if(dists[el]<=2){
      dh = df$Z[nns[el]] - predictrees$Z[el]
      delta.ht <- c(delta.ht, dh)
      delta.dist <- c(delta.dist, dists[el])
    } else {
      misses = c(misses, el)
    }
  }
  loss = sqrt(sum(delta.ht^2, delta.dist^2))*(1-length(misses)/length(dists))
  return(loss)
}

xx <- matchtrees(sgtrees, aoi)
xx

delta.dist = unlist(xx$nn)
nn = unlist(xx$nn)
nn
delta.ht = c()
for(el in length(xx)){
  if(xx[el]<=2){
    dh = nn[el]
    delta.ht[el] = dh
  } else {


itcfun <- function(aoi){
  olas = pts2las(aoi)
  oft = itcdelineate(olas, aoi)
  return(oitc)
}

itcfun('CC-CVN2')

itcout <- lapply(aois, itcfun)
itctest <- itcdelineate(testlas, 'ER-APL1', 'li2012')

#####################################
# Plot all ITC objects in facet grid
#####################################

itc_df <- lapply(itcout, tidy)
itc_df <- lapply(itc_df, function(x){x$Site_Name <- parent.frame()$i[]; return(x)})
itc_df <- do.call('rbind', itc_df)
itc_df$Site_Name <- as.character(itc_df$Site_Name)

for(i in seq(length(aois))){
  itc_df[itc_df$Site_Name == i, ]$Site_Name <- aois[i]
}

ggplot(itc_df) +
  geom_polygon(aes(x = long, y = lat, group = id), color = 'grey90', fill = 'darkgreen', alpha = 0.5) +
  facet_wrap(~Site_Name, scales = 'free') +
  labs(x = 'Longitude (m)', y = 'Latitude (m)')

##########################
# Prep for plotting
##########################

itc_data <- lapply(itcout, function(x) {
  as.data.frame(x@data)
})

itc_data <- lapply(itc_data, function(x){
  x$Site_Name <- parent.frame()$i[]; return(x)
})

itc_data <- do.call('rbind', itc_data)
itc_data$Site_Name <- as.character(itc_data$Site_Name)

for(i in seq(length(aois))){
  itc_data[itc_data$Site_Name == i, ]$Site_Name <- aois[i]
}

itc_data$Observation <- 'Predicted'
itc_data$Height_m <- itc_data$ZTOP

##########################################################################
# Import full clean inventory data
##########################################################################
# Import full clean inventory data
invdata <- read.csv(paste0(fidir, 'Inventory_Plots_Data_2018-2020_Collated', 'EastRiver_AllPlots_Inventory_Data_2018-2020_Collated.csv'))

# Import invdata from new sites
getxl <- function(plt) {
  
  invfile = list.files(fidir, pattern = paste0(plt,'_inventory_data', recursive=T, full.names = T)
  pltinv <- readxl(invfile, sheet='inventory_data', header=T)
  rbind(invdata, pltinv)
}
cvn1.inv <- read.xl()


##########################################################################
# Import site information
##########################################################################

# 2020 plot info
siteinfo20 <- read.csv(paste0(wsdir, 'Kueppers_EastRiver_Final_Sites_2020.csv'))
siteinfo20 <- siteinfo20[1:14,]
colnames(siteinfo20)[1] <- 'Site_Name'

##########################################################################
# Merge inventory data and site information
##########################################################################
inv <- merge(invdata, siteinfo20, by = 'Site_Name') %>%
  select(-X)

inv$Predicted <- 'Observed'
invtrees <- inv[c('Site_Name', 'Height_Avg_M', 'Predicted')]
names(invtrees) <- c('Site_Name', 'Height_m', 'Observation')

invtrees <- invtrees[!inv$Site_Name %in% c('XX-PLN1', 'XX-PLN2', 'XX-CAR1', 'XX-CAR2'),]
modtrees <- itc_data[c('Site_Name', Height = 'Height_m', 'Observation')]

trees <- rbind(invtrees, modtrees)
counts <- trees %>%
  group_by(Site_Name, Observation) %>%
  count()
diffs <- counts %>%
  ungroup() %>%
  group_by(Site_Name) %>%
  summarize(pct = n[2]/n[1])

fcount <- counts[counts$Observation == 'Observed',]
pcount <- counts[counts$Observation == 'Predicted',]
fcount$n <- paste0('obs=', fcount$n)
pcount$n <- paste0('pred=', pcount$n)

# Represent it
p <- trees %>%
  ggplot(aes(x=Height_m, fill=Observation)) +
  geom_histogram(position = 'dodge', binwidth = 1) +
  scale_fill_manual(values=c("#364f6b", "#3fc1c9")) +
  facet_wrap(~Site_Name, scales = 'fixed') +
  geom_text(data=fcount, aes(x = 0, y=170, label=n), 
            colour="black", inherit.aes=FALSE, parse=FALSE, hjust = 0, size = 2) +
  geom_text(data=pcount, aes(x = 0, y=160, label=n), 
          colour="black", inherit.aes=FALSE, parse=FALSE, hjust = 0, size = 2) +
  labs(x = 'Height class (1 m increments)', y = 'Number of trees')

p

###################################
# Scratch
###################################

densty <- unlist(lapply(ptclouds, nrow))

plot(densty, diffs$`n[1] - n[2]`)
x <- lm(densty ~ diffs$`n[1] - n[2]`)
summary(x)
chm <- grid_canopy(testlas, 0.5, pitfree(subcircle = 0.2))
diffs


ttops <- find_trees(testlas, lmf(f))
plot(chm, col = height.colors(50),)
plot(ttops, add = T)
pp <- plot(testlas, size = 4)
add_treetops3d(pp, ttops)

algo <- li2012(dt1 = 3, dt2 = 6, R = 3, Zu = 15, hmin = 2.5, speed_up = 8.5)
las <- segment_trees(testlas, algo) # segment point cloud
plot(las, bg = "white", size = 4, color = "treeID") # visualize trees

tree110 <- filter_poi(las, treeID == 12)
plot(tree110, size = 8, bg = "white")

las <- segment_trees(las, algo1, attribute = "IDdalponte")
las <- segment_trees(las, algo, attribute = "IDli")
crowns <- delineate_crowns(las)
crowns
plot(crowns)

x <- plot(las, bg = "white", size = 4, color = "IDdalponte", colorPalette = pastel.colors(200))
plot(las, add = x + c(100,0), bg = "white", size = 4, color = "IDli", colorPalette = pastel.colors(200))

# Plot points
fig <- plot_ly(data.frame(wfpts_df), x = ~px, y = ~py, z = ~pz,
               type = 'scatter3d',
               mode = 'markers',
               size = 1,
               color = ~pi)
fig




View(itcout)
length(itcout[[8]])


xx <- lapply(itcout, tidy)
xx <- lapply(xx, function(x){x$Site_Name <- parent.frame()$i[]; return(x)})
xx <- do.call('rbind', xx)
xx$Site_Name <- as.character(xx$Site_Name)


library(data.table)

df <- do.call('rbind', itcout)
df[['index']] <- rep(seq_along(itcout), sapply(itcout, nrow))

values <- data.frame(id = as.character(df$treeID), 
                     height = df$ZTOP, 
                     Site_Name = as.character(df$index))
View(values)
View(left_join(xx, values, by = c('Site_Name', 'id')))



ids <- seq(length(crowns))
values <- data.frame(id = as.character(ids),
                     height = crowns$ZTOP)

View(do.call('rbind', itcout))
?do.call

?rbind()

library(broom)
tidy_crowns <- broom::tidy(crowns)
plotData <- left_join(tidy_crowns,values,by='id')


