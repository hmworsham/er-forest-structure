

## ---------------------------------------------------------------------------------------------------
# Load config
config <- config::get(file=file.path('~',
                                     'Repos',
                                     'er-forest-structure',
                                     'config',
                                     'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

## ---------------------------------------------------------------------------------------------------
drive_auth(path=config$drivesa)


## ---------------------------------------------------------------------------------------------------
workerNodes <- str_split(system('squeue -u $USER -o "%N"', intern=T)[[2]], ',', simplify=T)
workerNodes <- rep(workerNodes, 32)
#cl <- parallel::makeCluster(workerNodes)
set_lidr_threads(length(workerNodes)-2)


## ---------------------------------------------------------------------------------------------------
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


## ---------------------------------------------------------------------------------------------------
# Ingest full LAS catalog of decimated points
infiles <- list.files(config$extdata$las_dec, full.names=T)
lascat <- readLAScatalog(infiles)


## ---------------------------------------------------------------------------------------------------
# Subset plot shapefiles to areas of interest (those within AOP flights)
aois <- plotsf$PLOT_ID
aois <- aois[grep('XX', aois, invert=T)]
plotsf <- plotsf[plotsf$PLOT_ID %in% aois,]

## ---------------------------------------------------------------------------------------------------
# Split plots into quadrants
# div <- 2 # Number of divisions
# ls <- list() # Empty list to store result
#
# for (i in 1:nrow(plotsf)){
#   x <- st_make_grid(plotsf[i,], n = div) %>% st_as_sf() # divide pol
#   xx <- st_intersection(plotsf[i,], x) # intersection
#   xx$QUADRANT <- sapply(seq(1:4), function(x) paste(plotsf[i,]$PLOT_ID, x, sep='.'))
#   ls[[i]] <- xx
# }
#
# plotsf <- st_as_sf(data.table::rbindlist(ls)) # superfast

# Store plot quadrant names
#  quad.names <- paste(unlist(lapply(aois, rep, 4)), seq(1,4), sep='.')

## ---------------------------------------------------------------------------------------------------
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

## ---------------------------------------------------------------------------------------------------
# Find intersection of stems and plots
stems.in.plots <- st_intersection(plotsf, stem.sf)


## ---------------------------------------------------------------------------------------------------
# Find intersection of stems and quadrants
# Returns an `sf` object with each tree associated with a quadrant (and its parent plot)
# quads <- st_buffer(plotsf, 5)
# stems.in.quads <- st_intersection(quads, stem.sf)


## ---------------------------------------------------------------------------------------------------
# Clip LAS points to all plot quadrants using a determined buffer
# lasplots <- mclapply(quad.names, function(x){
#   p = plotsf[plotsf$QUADRANT==x,][1]
#   bnd = st_buffer(p$geometry, endCapStyle='ROUND', 1)
#   pc = clip_roi(lascat, bnd)
#   return(pc)
#   },
#   mc.cores = getOption("mc.cores", length(workerNodes)-2))
#
# # Check
# assertthat::are_equal(length(lasplots), length(quad.names), 68)
#
# # Add names to list of lasplots
# names(lasplots) <- quad.names


## ---------------------------------------------------------------------------------------------------
#  Clip LAS points to all plots using a determined buffer
lasplots <- mclapply(aois, function(x){
  p = plotsf[plotsf$PLOT_ID==x,][1]
  bnd = st_buffer(p$geometry, endCapStyle='ROUND', 1)
  pc = clip_roi(lascat, bnd)
  return(pc)
  },
  mc.cores = getOption("mc.cores", length(workerNodes)-2))

# Check
assertthat::are_equal(length(lasplots), length(aois))

# Add names to list of lasplots
names(lasplots) <- aois
