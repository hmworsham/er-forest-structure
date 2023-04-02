# Install and load libraries
pkgs <- c('dplyr',
          'tidyverse',
          'ggplot2',
          'raster',
          'devtools',
          'plotly',
          'sf', 
          'terra',
          'parallel',
          'lidR',
          'lidRplugins',
          'rlas',
          'rgl',
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
scrdir <- file.path('/global', 'scratch', 'users', 'worsham')
datadir <- file.path(scrdir, 'las_decimated')
regriddir <- file.path(scrdir, 'las_regridded')
shapedir <- file.path(scrdir, 'EastRiverInputs', 'Plot_Shapefiles', 'AllPlots')
fidir <- file.path(scrdir, 'EastRiverInputs', 'Inventory_Plots')
gpsdir <- file.path(scrdir, 'EastRiverInputs', 'StemGeolocations')
outdir <- file.path(scrdir, 'trees')
dir.create(outdir)

################
# Ingest data
################

# Ingest full las catalog
infiles <- list.files(datadir, full.names=T)
lascat <- readLAScatalog(infiles)

# Ingest plot boundaries
plotsf <- st_read(list.files(shapedir, pattern='.shp', full.names=T))
aois <- plotsf$PLOT_ID
aois <- c('SG-SWR1', 
          'CC-UC1', 
          'WG-WGM1', 
          'ER-APL1', 
          'CC-UC2', 
          'SG-NES2', 
          'ER-BME2', 
          'ER-BME1', 
          'ER-APU1', 
          'ER-GT1', 
          'SG-NES3', 
          'SR-PVG1', 
          'SG-NES1',
          'CC-CVN1',
          'CC-CVN2', 
          'CC-EMN1', 
          'CC-CVS1')

# Ingest field data

# Ingest field data
# Doesn't work - blocked by Savio
# library(googledrive)
# drive_auth_configure(path='~/.ssh/eastriver-r-googledrive-clientsecret.json', api_key='AIzaSyAi6a0sP-zwcv68MEhOJqbjk1V24M026Yo')
# drive_auth(email='worsham@berkeley.edu')
# drive_find(pattern='EastRiver_Census1_Data_Collated')

invfiles <- list.files(fidir, pattern='EastRiver_Census1_Data_Collated.csv', recursive=T, full.names=T)
invfiles
inv <- read.csv(invfiles[1])
inv <- (inv[grep(
  paste('out of plot', 
        '^oop$',
        '^OOP$',
        'outside plot',
        'Outside plot',
        'not in plot',
        'Not in plot',
        sep='|'),
  inv$Comments,
  invert=T),])
inv <- inv[inv$Status %in% c('L', 'Live', 'LIVE'),]
inv <- inv[inv$Site_Name %in% aois,]
dim(inv)
unique(inv$Site_Name)
dim(inv[inv$Geotagged==T,])
df = data.frame('Site_Name'=inv$Site_Name, 
                'Z'=as.numeric(inv$Height_Avg_M), 
                'D'=as.numeric(inv$DBH_Avg_CM), 
                'X'=as.numeric(inv$Longitude), 
                'Y'=as.numeric(inv$Latitude))
df = na.omit(df)
unique(df$Site_Name)
dim(inv)
dim(inv[inv$Geotagged==T,])
dim(inv[!is.na(inv$Latitude),])

datadir <- '/global/scratch/users/worsham/trees_100K'
trfiles <- list.files(datadir, full.names=T)

gettrees <- function(fn){
  df = read.csv(fn, col.names=c('index','tree_id', 'Z', 'X', 'Y', 'Zg'), header=F)[-1,]
  df$X = as.numeric(gsub('c\\(', '', df$X))
  df$Y = as.numeric(df$Y)
  df$Z = as.numeric(df$Z)
  df[c(2:5)]
}

trees <- mclapply(trfiles, gettrees, mc.cores=getOption('mc.cores', 16))

# Bind all trees together into one dataframe
alltrees <- rbindlist(trees)

# Create a dataframe based on tree geometries
head(alltrees)

# Remove unlikely trees
alltrees <- alltrees[alltrees$Z<=32,]

# Add diameter predictions to trees
alltrees$D <- -8.1946+16.2768*log(alltrees$Z)

# Make sf of alltrees
ptsf <- st_as_sf(alltrees, coords = c('X', 'Y'), crs = '+proj=utm +zone=13 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')

treesinplots <- st_join(ptsf, st_buffer(plotsf,3), join=st_within)
treesinplots2 <- treesinplots[!is.na(treesinplots$PLOT_ID),]
dim(treesinplots2[treesinplots2$PLOT_ID=='CC-UC2',])

# Ingest point cloud at all plots with a given buffer

lasplots <- mclapply(aois, function(x){
  p = plotsf[1][plotsf[1]$PLOT_ID==x,]
  bnd = st_buffer(p$geometry, 5)
  pc = clip_roi(lascat, bnd)
  return(pc)
}
,
mc.cores = getOption("mc.cores", 64L))

# Check
assertthat::are_equal(length(lasplots), length(aois), 17)

#ttops = find_trees(lasplots, ptrees(c(30, 20, 10, 5), 1.3, 7L))
ttops = lapply(lasplots, function(x){
  find_trees(x, ptrees(k=c(14,5,3), hmin=1.3, nmax=7L))
})

lapply(ttops, dim)
x = plot(lasplots[[1]])
add_treetops3d(x, ttops[[1]])
rglwidget()

# st   = segment_trees(las[1:50000], ptrees(c(30,15)))
# x = plot(las[1:10000])
# add_treetops3d(x, ttops)
# crowns = crown_metrics(st, func=NULL, geom='concave', attribute='treeID')
# 
# clean_dat <- st[st$treeID %in% names(which(table(st$treeID) > 4)), ]
# dim(clean_dat)
# crowns <- crown_metrics(clean_dat, func=NULL, geom='convex', attribute='treeID')
# plot(crowns)
# 
dim(df)
names(ttops) <- aois
ttops
ttops.df <- lapply(ttops, as.data.frame)
modtrees <- rbindlist(ttops.df, idcol=T)
modtrees$D <- -8.1946+16.2768*log(modtrees$Z)
names(modtrees) <- c('Site_Name', 'TreeID', 'Z', 'X', 'Y', 'D')
st_crs(ttops[[1]])
# COMPUTE LOSS
library(nngeo)
mt = st_as_sf(modtrees, coords=c('X', 'Y'), crs = st_crs(ttops[[1]]))
y = st_as_sf(df, coords = c('X', 'Y'), crs='EPSG:4326')
y = st_transform(y, '+proj=utm +zone=13 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
nn = st_nn(mt, y, k=1, returnDist=T)
dists = unlist(nn$dist)
nns = unlist(nn$nn)
delta.ht = c()
delta.dist = c()
misses = c()
dists
for(el in seq(length(dists))){
  if(dists[el]<=2){
    dh = df$Z[nns[el]] - mt$Z[el]
    delta.ht <- c(delta.ht, dh)
    delta.dist <- c(delta.dist, dists[el])
  } else {
    misses = c(misses, el)
  }
}
length(dists)-length(misses)
1-length(misses)/length(dists)
sqrt(sum(delta.ht^2))/(length(dists)-length(misses))
sqrt(sum(delta.dist^2))/(length(dists)-length(misses))
length(misses)

loss = sqrt(sum(delta.ht^2, delta.dist^2))*(1-length(misses)/length(dists))
loss


df$src <- 'Observed'
modtrees$src <- 'Predicted'
compare.trees <- rbind(modtrees, df, fill=T)

counts <- compare.trees %>%
  group_by(Site_Name, src) %>%
  dplyr::summarise(n=n())
counts

diffs <- counts %>%
  ungroup() %>%
  group_by(Site_Name) %>%
  dplyr::summarise(pct = n[2]/n[1])
diffs
mean(diffs$pct)

fcount <- counts[counts$src == 'Observed',]
pcount <- counts[counts$src == 'Predicted',]
fcount$n <- paste0('obs=', fcount$n)
pcount$n <- paste0('pred=', pcount$n)

ggplot(compare.trees, aes(x=Z, fill=src)) +
  geom_histogram(binwidth=5, position='dodge') + 
  scale_fill_manual(values=c("#364f6b", "#3fc1c9"), name='Source') +
  facet_wrap(~Site_Name) + 
  geom_text(data=fcount, aes(x = 40, y=400, label=n), 
            colour="black", inherit.aes=FALSE, parse=FALSE, hjust = 1, size = 4) +
  geom_text(data=pcount, aes(x = 40, y=350, label=n), 
            colour="black", inherit.aes=FALSE, parse=FALSE, hjust = 1, size = 4) +
  labs(x = 'Height class (5 m bins)', y = 'Number of trees') + 
  theme_minimal(base_size=18)

ggplot(compare.trees, aes(x=D, fill=src)) +
  geom_histogram(binwidth=5, position='dodge') + 
  scale_fill_manual(values=c("#364f6b", "#3fc1c9")) +
  facet_wrap(~Site_Name) + 
  geom_text(data=fcount, aes(x = 40, y=400, label=n), 
            colour="black", inherit.aes=FALSE, parse=FALSE, hjust = 1, size = 4) +
  geom_text(data=pcount, aes(x = 40, y=350, label=n), 
            colour="black", inherit.aes=FALSE, parse=FALSE, hjust = 1, size = 4) +
  labs(x = 'Height class (5 m bins)', y = 'Number of trees') + 
  theme_minimal(base_size=14)

as.data.frame(compare.trees) %>%
  group_by(Site_Name, src) %>%
  dplyr::summarise(mh = median(Z))

modmedh <- median(modtrees$Z)
modsdh <- sd(modtrees$Z)

modqmd <- sqrt((sum(modtrees$D^2)/length(modtrees$D)))
modsdd <- sd(modtrees$D)
modqmd
modsdd

invqmd <- sqrt((sum(df$D^2)/length(df$D)))
invqmd

plot(df$D, modtrees$D)
length(df$D)
length(modtrees$D)
