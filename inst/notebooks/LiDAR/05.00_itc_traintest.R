# Training individual tree crown segmentation on LiDAR points
# Author: Marshall Worsham | worsham@berkeley.edu.
# Created: 04-08-21
# Revised: 04-04-23

#########################
# Front matter
########################

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)
load_all('~/Repos/rwaveform')

# Configure Drive auth (using service account)
drive_auth(path=config$drivesa)

# Set up multinode cluster for parallel computing
workerNodes <- str_split(system('squeue -u $USER -o "%N"', intern=T)[[2]], ',', simplify=T)
workerNodes <- rep(workerNodes, 40)
cl <- parallel::makeCluster(workerNodes)
set_lidr_threads(length(workerNodes)-2)

# Name directories
# scrdir <- file.path('/global', 'scratch', 'users', 'worsham')
# datadir <- file.path(scrdir, 'las_decimated')
# regriddir <- file.path(scrdir, 'las_regridded')
# shapedir <- file.path(scrdir, 'EastRiverInputs', 'Plot_Shapefiles', 'AllPlots')
# fidir <- file.path(scrdir, 'EastRiverInputs', 'Inventory_Plots')
# gpsdir <- file.path(scrdir, 'EastRiverInputs', 'StemGeolocations')
# outdir <- file.path(scrdir, 'trees')
# dir.create(outdir)

################
# Ingest data
################

# Ingest full las catalog
infiles <- list.files(config$extdata$las_dec, full.names=T)
lascat <- readLAScatalog(infiles)

# Ingest plot boundaries
plotsf <- load.plot.sf(path=as_id(config$extdata$plotid),
                       pattern=config$extdata$plotpattern)

# Ingest field data
tmpfile <- drive_download(
  as_id(config$extdata$invid),
  type='csv',
  path=file.path(tempdir(), config$extdata$invid),
  overwrite=T)$local_path
inv <- read.csv(tmpfile)

#############
# Munge data
#############

# Subset plot shapefiles to areas of interest (those within AOP flights)
aois <- plotsf$PLOT_ID
aois <- aois[grep('XX', aois, invert=T)]
plotsf <- plotsf[plotsf$PLOT_ID %in% aois,]

# Split plots into quadrants
div <- 2 # Number of divisions
ls <- list() # Empty list to store result

for (i in 1:nrow(plotsf)){
  x <- st_make_grid(plotsf[i,], n = div) %>% st_as_sf() # divide pol
  xx <- st_intersection(plotsf[i,], x) # intersection
  xx$QUADRANT <- sapply(seq(1:4), function(x) paste(plotsf[i,]$PLOT_ID, x, sep='.'))
  ls[[i]] <- xx
}

plotsf <- sf::st_as_sf(data.table::rbindlist(ls)) # superfast

# Store plot quadrant names
quad.names <- paste(unlist(lapply(aois, rep, 4)), seq(1,4), sep='.')

# Filter out trees not meeting criteria
inv <- inv[grep('outside plot', inv$Comments, invert=T),] # Outside plots
inv <- inv[inv$Status == 'Live',] # Living stems
inv <- inv[!is.na(inv$Latitude | !is.na(inv$Longitude)),]

# Keep stem x,y,z data
stem.xyz = data.frame('Tag_Number'=as.numeric(inv$Tag_Number),
                      'Z'=as.numeric(inv$Height_Avg_M),
                      'X'=as.numeric(inv$Longitude),
                      'Y'=as.numeric(inv$Latitude))
stem.xyz = na.omit(stem.xyz)

# Turn stem.xyz into sf object
stem.sf <- st_as_sf(stem.xyz, coords=c('X', 'Y'), crs='EPSG:4326')
stem.sf <- st_transform(stem.sf, crs=st_crs(plotsf))

# Find intersection of stems and quadrants
# Returns an `sf` object with each tree associated with a quadrant (and its parent plot)
stems.in.quads <- st_intersection(plotsf, stem.sf)

#############################
# Plotting for verification
#############################

# Plot las catalog with plot boundaries
# plot(lascat)
# plot(plotsf, add=T, col='red')
# plot(plotsf[1,1])
# extent(plotsf[1,1])

# Read in one las file
# las1 <- readLAS(infiles[960])
# plot(las1)
# rglwidget()

# Crop las by 5m square [not sure why this is in here...]
# ex <- extent(las1)-5
# las1c <- clip_rectangle(las1, ex[1], ex[3], ex[2], ex[4])

# Plot cropped las
# plot(las1c, bg='white')
# rglwidget()

##########################################
# ITS optimization
##########################################

# Ingest point cloud at all plots with a given buffer
lasplots <- mclapply(quad.names, function(x){
  p = plotsf[plotsf$QUADRANT==x,][1]
  bnd = st_buffer(p$geometry, endCapStyle='ROUND', 5)
  pc = clip_roi(lascat, bnd)
  return(pc)
  },
  mc.cores = getOption("mc.cores", length(workerNodes)-2))

# Check
assertthat::are_equal(length(lasplots), length(quad.names), 68)

# Define vectors of parameters on which to run Li algorithm for optimization
dt1.seq = seq(0.01, 0.1, 0.04)
dt2.seq = seq(1, 2, 0.2)
R.seq = seq(0, 4, 1)
Zu.seq = seq(14, 16, 1)
length(dt1.seq)*length(dt2.seq)*length(R.seq)*length(Zu.seq)

g <- expand.grid(dt1.seq, dt2.seq, R.seq, Zu.seq)

# operate on each row of grid
# do.call(mapply, c("f", unname(as.list(g))))
# # or
# sapply(1:nrow(g), function(i) do.call("f", unname(g[i, ])))
# # or
# mapply(f, g[, 1], g[, 2])

# Initialize Li 2012 for optimization
li2012.opt <- function(pc, dt1, dt2, R, Zu, hmin=1.3) {
  algo = li2012(dt1, dt2, R, Zu, hmin)
  segtrees = segment_trees(pc, algo) # segment point cloud
  crowns = crown_metrics(segtrees, func = .stdtreemetrics, geom = "convex")
  crowns = crowns[st_is_valid(crowns),]
  ttops = st_centroid(crowns)
  ttops = ttops[ttops$Z >= hmin,]
  return(ttops)
}

testli <- mcmapply(li2012.opt,
                   g[,1],
                   g[,2],
                   g[,3],
                   g[,4],
                   MoreArgs=list(pc=lasplots[[1]], hmin=1.3),
                   mc.cores = getOption("mc.cores", 30))

testli <- apply(testli, 2, data.frame)
testli <- lapply(testli, st_as_sf)
testli <- lapply(testli, function (x) {
  tl <- x[!st_is_empty(x),]
  tl
})
testli <- Filter(function(x) nrow(x) > 0 , testli)

predictloss.0 <- function(predictrees, invtrees, plot.sf, aoi, draw.plots=F) {

  # Plot field-identified trees against modeled trees
  if(draw.plots) {

    plot(st_geometry(invtrees), pch='+', col='lightblue4')
    plot(st_geometry(predictrees), pch='+', add=TRUE, col='firebrick2')
    legend('topright',
           legend=c('Field trees', 'Modeled trees'),
           col=c('lightblue4', 'firebrick2'),
           pch='+',
           cex=0.6)
  }

  # For each modeled tree, find its k nearest neighbors in X-Y coordinate space
  nn = suppressMessages(st_nn(predictrees, invtrees, k=5, returnDist=T, progress=F))

  # For each of these pairs, calculate their Z distances and append to nn object
  dz = list()
  dz2 = list()
  dxy2 = list()
  for(i in seq(nrow(predictrees))) {
    dh = predictrees[i,]$Z - invtrees[nn$nn[[i]],]$Z
    dz = append(dz, list(dh))
    dist2 = nn$dist[[i]]^2
    dxy2 = append(dxy2, list(dist2))
    dz2 = append(dz2, list(dh^2))
  }

  nn$dz = dz
  nn$dxy2 = dxy2
  nn$dz2 = dz2

  # For each modeled tree, find the 3D Euclidean distance
  # between it and its k nn, then find the minimum
  dxyz = sqrt(mapply('+', nn$dxy2, nn$dz2))

  #dists = unlist(nn$dist)
  #nns = unlist(nn$nn)
  dists = apply(dxyz, 2, min)
  nns = apply(dxyz, 2, which.min)
  delta.ht = c()
  delta.dist = c()
  misses = c()
  matches = c()

  for(i in seq(length(dists))) {
    if (dists[i] <= 5) {
      dists[i] = dists[i]
      matches = c(matches, i)
    } else {
      dists[i] = NA
      misses = c(misses, i)
    }
  }

  # for(el in seq(length(dists))){
  #   if(dists[el]<=2){
  #     dh = df$Z[nns[el]] - predictrees$Z[el]
  #     delta.ht <- c(delta.ht, dh)
  #     delta.dist <- c(delta.dist, dists[el])
  #   } else {
  #     misses = c(misses, el)
  #   }
  # }

  #loss = sqrt(sum(dists^2, na.rm=T))*(1-length(misses)/length(dists))
  rmse = sqrt(sum(dists^2, na.rm=T)/length(matches))

  return(c('rmse' = rmse,
           'n match' = length(matches),
           'n miss' = length(misses),
           'n predicted' = length(dists)))
}


# Calculate loss on prediction
y = stems.in.quads[stems.in.quads$QUADRANT=='SG-SWR1.1',]
loss.tst2 <- lapply(testli[1:20], predictloss.0, y, plotsf, 'SG-SWR1', draw.plots=F)
loss.tst2

# A is a set of algorithms being evaluated
# P is a matrix of parameters used to force algorithm, where each p is a combination of parameters
# k is a plot
# 1. Procedure optimize tree detection
# 2.  for i in A do
# 3.    for p in P
# 4.      segment trees from point cloud with i,p
# 5.      delineate trees from segmented points
# 6.
# 1. define parameter set to test algorithm
  # 2. run the algorithm with ps_1 on plots 1:k-1 >>> compute loss, store result
  # 3. run the algorithm with ps_2 on plots 1:k-1 >>> compute loss, store result
  # 4. run the algorithm with ps_3 on plots 1:k-1 >>> compute loss, store result
  # 5. repeat for ps_4:n

# testli <- mapply(li2012.opt, dt1.seq, dt2.seq, R.seq, Zu.seq, MoreArgs=list(pc=pointcloud.test[[1]], hmin=1.3))

# Initialize Dalponte 2016
# Rasterize canopy surface from point cloud in AOI
chm <- rasterize_canopy(subset1, 0.5, pitfree(subcircle = 0.2))

#####################
# Write trees
####################


###################################
# Find trees using search function
###################################

# Define search function
f = function(x) {
  y <- 2.2 * (-(exp(-0.08*(x-2)) - 1)) + 3
  y[x < 2] <- 3
  y[x > 20] <- 7
  return(y)
}

# Find trees
trees = lapply(nlas, locate_trees, lmf(f))

# Write trees as csvs for safekeeping
for(i in seq(length(trees))){
  write.csv(trees[[i]], paste0('/global/scratch/users/worsham/trees_100K/', sprintf("trees_%04d",i), '.csv'))
}

##########################################
# ITC segmentation on one AOI
##########################################

# Specify AOI
aoi <-  'SG-NES2'

# Ingest points at plot bound with specified buffer
plotsf <- plotsf[plotsf$PLOT_ID==aoi,][1,1]
testgeom <- st_buffer(plotsf$geometry, 2)
subset1 <- clip_roi(lascat, testgeom)

# Plot point cloud at aoi
plot(subset1, bg='white', size=2)
rglwidget()

# Rasterize canopy surface from point cloud in AOI
chm <- rasterize_canopy(subset1, 0.5, pitfree(subcircle = 0.2))
plot(chm, col = height.colors(50))
plot(plotsf, add=T)

# Find tree tops from point cloud in AOI
sgtrees <- locate_trees(subset1, lmf(ws = 2.1))
#plot(sf::st_geometry(sgtrees), add = TRUE, pch = '+')

# Plot point cloud with identified treetops
x <- plot(subset1, bg = "white", size = 4)
add_treetops3d(x, sgtrees)
rglwidget()

# Segment trees using Dalponte 2016
st_crs(sgtrees) <- 32613
algo <- dalponte2016(chm, sgtrees, th_tree=1.3, th_seed=0.005)
crowns <- algo()

plot(crowns, col=pastel.colors(200))

segtrees <- segment_trees(subset1, algo) # segment point cloud
length(unique(segtrees@data$treeID))
plot(segtrees, bg = "white", size = 1, color = "treeID") # visualize trees
rglwidget()

crowns <- crown_metrics(segtrees, func = .stdtreemetrics, geom = "convex")
crowns <- crowns[st_is_valid(crowns),]
plot(crowns["convhull_area"], main = "Crown area (convex hull)", col=pastel.colors(nrow(crowns)))
plot(plotsf, add=T)

# Segment trees using Li 2012
algo <- li2012(dt=.01, dt2=.02, R=3, hmin=1.3)
segtrees <- segment_trees(lasplots[[1]], algo) # segment point cloud
st_crs(segtrees) <- 32613
length(unique(segtrees@data$treeID))

tree112 <- filter_poi(segtrees, treeID == 17)
plot(tree112)
rglwidget()

crowns <- crown_metrics(segtrees, func = .stdtreemetrics, geom = "convex")
crowns <- crowns[st_is_valid(crowns),]
ttops <- st_centroid(crowns)
ttops <- ttops[ttops$Z >= 1.3,]
ttops
st <- crowns %>%
  group_by(treeID) %>%
  summarise(geometry=st_centroid(geometry),
         Z=max(Z, na.rm=T))
st <- st[st$Z>=1.3,]

plot(crowns$geometry)
plot(st_centroid(crowns),col=crowns$Z)

#crowns <- st_make_valid(crowns)
nrow(crowns)

plot(crowns["convhull_area"], main = "Crown area (convex hull)", col=pastel.colors(nrow(crowns)))

plot(plotsf, add=T)
xx <- predictloss(fidir, shapedir, sgtrees, aoi)

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


ttops = find_trees(las, ptrees(c(30,15), 1.3, 7L))
st   = segment_trees(las[1:50000], ptrees(c(30,15)))
x = plot(las[1:10000])
add_treetops3d(x, ttops)
crowns = crown_metrics(st, func=NULL, geom='concave', attribute='treeID')

clean_dat <- st[st$treeID %in% names(which(table(st$treeID) > 4)), ]
dim(clean_dat)
crowns <- crown_metrics(clean_dat, func=NULL, geom='convex', attribute='treeID')
plot(crowns)

