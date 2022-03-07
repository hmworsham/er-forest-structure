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
          'waveformlidar',
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
datadir <- '~/Desktop/RMBL/Projects/Watershed_Spatial_Dataset/Output/waveform_points'
fidir <- '~/Desktop/RMBL/Projects/Forest_Inventory_Dataset/Output/'
wsdir <- '~/Desktop/RMBL/Projects/Watershed_Spatial_Dataset/Source/'
shapedir <- '~/Google Drive (worsham@berkeley.edu)/Research/RMBL/RMBL_East River Watershed Forest Data/Data/Geospatial/Kueppers_EastRiver_Plot_Shapefiles_2020_WGS84UTM13N/Polygons'

#########################################
# Ingest points from waveform processing
########################################

ptcsv <- list.files(datadir, full.names = F)
ptclouds <- lapply(ptcsv, read.csv, header = T)
aois <- sapply(strsplit(ptcsv, '\\.'), '[', 1)
aois
##########################################
# ITC segmentation
##########################################
pts2las <- function(aoi){
  
  # Get point cloud for aoi
  pc_csv = list.files(datadir, pattern = aoi, full.names = T)
  pc = read.csv(pc_csv, header = T)
  
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
                       gpstime = 0,
                       Intensity = as.integer(pc$t),
                       ReturnNumber = 1L,
                       NumberOfReturns = 1L,
                       ScanDirectionFlag = 0L,
                       EdgeOfFlightline = 0L,
                       Classification = 0L,
                       ScanAngleRank = 0L,
                       UserData = 0L,
                       PointSourceID = 0L)
  lasheader = header_create(lasdata)
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
  scatter3D(ptdata$X, ptdata$Y, ptdata$Z, colvar = ptdata$Z, clab = 'Elevation', pch = 20, ticktype='detailed')
  
  # Normalize heights to surface
  nlas = normalize_height(gclas, kriging())
  nlasdata = nlas@data
  scatter3D(nlasdata$X, nlasdata$Y, nlasdata$Z, colvar = nlasdata$Z, clab = 'Canopy Height', pch = 20, ticktype = 'detailed')
  
  return(nlas)
}

#testlas <- pts2las('SG-NES2')

itcdelineate <- function(nlas, aoi, algo='li2012'){
  
  # Get plot boundary for aoi
  plotpath = list.files(shapedir, 
                          pattern = glob2rx(paste0(aoi,"*shp")),
                          full.names = T)
  plotsf = readOGR(plotpath, verbose = F)
  geoextent = as.list(extent(plotsf))
  
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
  plot(clipitc)
  destdir = '~/Desktop/RMBL/Projects/Watershed_Spatial_Dataset/Output/itc'
  #writeOGR(obj=clipitc, dsn=destdir, layer=paste0(aoi, '_itc'), driver="ESRI Shapefile")
  
  return(clipitc)
}

itcfun <- function(aoi){
  olas = pts2las(aoi)
  oitc = itcdelineate(olas, aoi)
  return(oitc)
}

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
invdata <- read.csv(paste0(fidir, 'EastRiver_Full_Inventory_Dataset.csv'))

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


