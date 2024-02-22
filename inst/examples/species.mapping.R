## East River tree species mapping

## ---------------------------------------------------------------------------------------------------
# Workspace setup
config <- config::get(file=file.path('config',
                                     'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

drive_auth(path=config$drivesa)
register_google(readLines(config$mapkey))

## ---------------------------------------------------------------------------------------------------
# Data ingest

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

# Ingest Nicola's classification map
tmpfile <- drive_download(
  as_id(config$extdata$falcoid),
  path=file.path(tempdir(), config$extdata$falcoid),
  overwrite = T)$local_path

sp.class <- rast(tmpfile)
sp.class <- as.numeric(sp.class)
names(sp.class) <- 'Sp_Code'

# Ingest Nicola's species codes
tmpfile <- drive_download(
  as_id(config$extdata$classid),
  path=file.path(tempdir(), config$extdata$classid),
  overwrite=T)$local_path

sp.codes <- read.csv(tmpfile)
sp.codes <- sp.codes %>%
  mutate(Pixel_Code=as.numeric(Pixel_Code)) %>%
  dplyr::select(-c(Genus:Common))

# Ingest detected tree crowns
# treefiles <- list.files('/global/scratch/users/worsham/trees_ls_100m', pattern='.shp', full.names=T)
# crowns <- mclapply(treefiles, \(x) {
#   tf <- st_read(x) },
#   mc.cores = getOption("mc.cores", 16))
modtrees <- read.csv(file.path(config$extdata$itc, 'opt_matches.csv'))

# Ingest las
infiles <- list.files(config$extdata$las_dec, full.names=T)
lascat <- readLAScatalog(infiles)

# # Ingest NAIP base image
# tmpfile <- drive_download(
#   as_id(config$extdata$naipid),
#   path=file.path(tempdir(), config$extdata$naipid),
#   overwrite=T)$local_path
#
# naip <- rast(tmpfile)

## ---------------------------------------------------------------------------------------------------
# Process field data

# Subset field data to AOP sites
inv <- inv %>%
  filter(!Site_Name %in% c('XX-CAR1', 'XX-CAR2', 'XX-CAR3',
                           'XX-PLN1', 'XX-PLN2', 'SG-NWS1',
                           'XX-FAR1', 'ER-BME3'))

# Filter out field trees not meeting criteria
#inv <- inv[grep('outside plot', inv$Comments, invert=T),] # Within plot bounds
inv <- inv[inv$Status == 'Live',] # Living stems
inv <- inv[!is.na(inv$Latitude | !is.na(inv$Longitude)),] # Have geolocation data

# Keep stem x,y,z data
stem.xyz = data.frame('Tag_Number'=as.numeric(inv$Tag_Number),
                      'Height'=as.numeric(inv$Height_Avg_M),
                      'DBH'=as.numeric(inv$DBH_Avg_CM),
                      'Crown_Radius'=0.082*inv$Height_Avg_M + 0.5,
                      'Canopy_Position'=inv$Canopy_Position,
                      'X'=as.numeric(inv$Longitude),
                      'Y'=as.numeric(inv$Latitude),
                      'Sp_Code'=as.factor(inv$Sp_Code),
                      'Site_Name'=inv$Site_Name)
stem.xyz = stem.xyz[!is.na(stem.xyz$Height),]

# Turn stem.xyz into sf object
stem.sf <- st_as_sf(stem.xyz, coords=c('X', 'Y'), crs='EPSG:4326')
stem.sf <- st_transform(stem.sf, crs=st_crs(plotsf))
# stem.sf <- vect(stem.sf)

# FILTER OUT SUBORDINATE TREES
# Buffer around all trees
# Exclude if:
# A. a tree is < 80th pctl in height
# B. buffer intersects the buffer of another tree(s)
# C. any of those trees are larger

# Apply diameter-weighted buffer around all trees
stem.buff <- st_buffer(stem.sf, dist=stem.sf$Crown_Radius,
                       endCapStyle = 'SQUARE', joinStyle='MITRE')

# Apply 3px buffer around all trees
stem.buff.3m <- st_buffer(stem.sf, dist=1, endCapStyle='SQUARE', joinStyle='MITRE')

# Create sparse matrix describing overlapping trees
stem.overlap <- st_overlaps(stem.buff)
stem.within <- st_within(stem.buff)

## ---------------------------------------------------------------------------------------------------
# Process modeled trees
modtrees <- modtrees %>%
  filter(src==1) %>%
  st_as_sf(coords=c('Xpred', 'Ypred'), crs='EPSG:32613') %>%
  left_join(stem.xyz, by=c('obs'='Tag_Number')) %>%
  mutate(Crown_Radius_Mod = 0.082*Zpred + 0.5)

st_crs(modtrees) <- st_crs(plotsf)

stem.sf <- modtrees

# Apply diameter-weighted buffer around all trees
stem.buff <- st_buffer(stem.sf, dist=stem.sf$Crown_Radius_Mod,
                       endCapStyle = 'SQUARE', joinStyle='MITRE')

# Apply 3px buffer around all trees
stem.buff.3m <- st_buffer(stem.sf, dist=3, endCapStyle='SQUARE', joinStyle='MITRE')

# Create sparse matrix describing overlapping trees
stem.overlap <- st_overlaps(stem.buff)
stem.within <- st_within(stem.buff)

## ---------------------------------------------------------------------------------------------------
# Make CHM

# Clip las to plot boundaries
lasplots <- clip_roi(lascat, plotsf)
lasplots <- lasplots[lapply(lasplots, nrow)>0]

# Rasterize canooy
chm.pitfree.05 <- lapply(lasplots, rasterize_canopy, 0.25, pitfree(), pkg = "terra")

# Smooth canopy
kernel <- matrix(1,5,5)
chm.smooth <- lapply(chm.pitfree.05, terra::focal, w = kernel, fun = mean, na.rm = TRUE)
plot(chm.smooth[[1]])
lapply(chm.smooth, plot)

# Prep for plotting: subset and reclassify raster
gt1.sp <- crop(sp.class, ext(plotsf[plotsf$PLOT_ID=='ER-GT1',]))
# gt1.naip <- crop(naip, ext(plotsf[plotsf$PLOT_ID=='ER-GT1',]))

reclass <- function(rs, target) {
  v <- rast(rs)
  v[] <- NA
  f <- data.frame(a=as.numeric(rs))
  names(f) <- 'Pixel_Code'
  vec <- c(1:nrow(f))
  f[,2] <- vec
  m <- left_join(f, target, by='Pixel_Code')
  colnames(m)[2] <- 'ord'
  m <- m[order(m$ord),]
  v[] <- m$Sp_Code
  names(v) <- 'Sp_Code'

  return(v)
}

gt1.sp.fct <- reclass(gt1.sp, sp.codes)

# Pull basemap from google
gt1.bmap <- get_map(location=c(lon = -107.00595, lat = 38.9772),
                    zoom=20,
                    maptype = 'satellite',
                    source = 'google')

# Specify plotting palette
sp.pal <- PNWColors::pnw_palette('Sunset2', n=5)

# Function to map at focal plot
mapit <- function(spras, stems) {
  ggmap(gt1.bmap) +
    #ggplot() +
    tidyterra::geom_spatraster(data=spras, aes(fill=Sp_Code), alpha=0.8) +
    scale_fill_manual(values=sp.pal, name='Classified Species', na.value = NA) +
    geom_sf(data=stems[stems$Site_Name=='ER-GT1', ],
            fill=NA, linewidth=1, color='gold', inherit.aes=F) +
    scale_color_manual(values=sp.pal, name='Classified Species', na.value = NA) +
    ggtitle(paste('Global N =', nrow(stems), '\nSite N =', nrow(stems[stems$Site_Name=='ER-GT1',]))) +
    xlab('Longitude') +
    ylab('Latitude') +
    coord_sf(crs = st_crs(4326)) +
    ggthemes::theme_calc(base_size=18) +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))
}

# Pull species from classification map

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

get.spp <- function(spras, stems, spcodes) {

  class.sp.plots <- extract(spras, stems, fun=\(x) modal(x, na.rm=T, ties='random'), touches=T)
  names(class.sp.plots) <- c('N', 'Pixel_Code')
  #class.sp.plots$N <- as.character(class.sp.plots$N)
  class.sp.plots$Pixel_Code <- as.numeric(class.sp.plots$Pixel_Code)

  # Join classifications to field data
  stems$N <- 1:nrow(stems)
  stems.comp <- left_join(stems, class.sp.plots, by='N')
  stems.comp <- left_join(stems.comp, spcodes, by='Pixel_Code')
  stems.comp$Sp_Code.y <- as.factor(stems.comp$Sp_Code.y)
  levels(stems.comp$Sp_Code.y) <- levels(stems.comp$Sp_Code.x) <- union(levels(stems.comp$Sp_Code.x),
                                                                        levels(stems.comp$Sp_Code.y))
  stems.comp <- stems.comp %>%
    rename(Reference=Sp_Code.x,
           Classified=Sp_Code.y)

  assertthat::are_equal(nrow(stems.comp), nrow(class.sp.plots))

  # Add size bins
  stems.comp$DBH_bins <- as.numeric(cut(stems.comp$DBH, c(0, 10, 20, 30, 40, 50)))

  return(stems.comp)
}

# Plot
plt.density <- function(x) {
  x %>%
    pivot_longer(cols=c('Reference', 'Classified'),
                 names_to='Source',
                 values_to='Species') %>%
    ggplot(aes(x=DBH, color=Source, fill=Source)) +
    geom_density(alpha=0.6) +
    scale_fill_manual(values=c('green4', 'dodgerblue')) +
    scale_color_manual(values=c('green4', 'dodgerblue')) +
    ggtitle(paste('N =', nrow(x))) +
    xlab('Height (m)') +
    ylab('Kernel density') +
    ggthemes::theme_calc(base_size=18) +
    # theme(legend.position = 'bottom',
    #       legend.direction = 'horizontal') +
    facet_grid(~Species)
}

plt.bar <- function(x){
  x %>%
    pivot_longer(cols=c('Reference', 'Classified'),
                 names_to='Source',
                 values_to='Species') %>%
    ggplot(aes(x=DBH_bins, fill=Species, alpha=Source), color='black') +
    geom_bar(position=position_dodge2(width=1, preserve='single'),
             aes(y=(..count..) / sum(..count..), fill=Species)) +
    scale_fill_manual(values=sp.pal) +
    scale_alpha_manual(values=c(1, 0.6)) +
    ggtitle(paste('N =', nrow(x))) +
    xlab('Size bins') +
    ylab('Frequency of observations') +
    ggthemes::theme_calc(base_size=18) # +
  # theme(legend.position = 'bottom',
  #       legend.direction = 'horizontal')
  # theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
}

# Maps at every site
siteplots <- lapply(unique(stem.sf$Site_Name), \(i) {
  site.sp <- crop(sp.class, ext(plotsf[plotsf$PLOT_ID==i,]))
  names(site.sp) <- 'Sp_Code'
  site.sp <- reclass(site.sp, sp.codes)
  ggplot() +
    tidyterra::geom_spatraster(data=site.sp, aes(fill=Sp_Code), alpha=0.8) +
    scale_fill_manual(values=sp.pal, name='Classified Species', na.value = NA) +
    geom_sf(data=stem.buff[stem.buff$Site_Name==i,],
            fill=NA, linewidth=1, color='gold', inherit.aes=F) +
    #ggtitle(paste('Global N =', nrow(stem.sf), '\nSite N =', nrow(stem.sf[stem.sf$Site_Name==i,]))) +
    xlab('Longitude') +
    ylab('Latitude') +
    ggthemes::theme_calc(base_size=14)
})


# Scratch
# ergt1.stems <- stem.sf[stem.sf$Site_Name=='ER-GT1',]
# ergt1.stems$treeID <- 1:nrow(ergt1.stems)
#
# algo <- dalponte2016(chm_pitfree_05_1, ergt1.stems, th_tree=1.3)
#
# ergt1.seg <- segment_trees(ergt1, algo)
# plot(ergt1.seg, color='treeID', )
# rglwidget()
# crowns <- crown_metrics(ergt1.seg, func = NULL, geom = "convex")
# plot(sf::st_geometry(crowns), reset = FALSE)
#
# ergt1.seg2 <- algo()
# crowns2 <- as.polygons(ergt1.seg2)
# plot(crowns2, col=pastel.colors(302))
# ergt1.stems <- st_join(st_as_sf(crowns2), ergt1.stems, by=c('Z'='treeID'))


# algos <- lapply(chm.smooth, dalponte2016, stem.sf, th_tree=1)
# stem.seg <- list()
# for(i in seq_along(algos)) {
#   stem.seg[[i]] <- as.polygons(algos[[i]]())
# }
#
# stem.seg <- do.call('rbind', stem.seg)
# stem.seg <- st_join(st_as_sf(stem.seg), stem.sf, by=c('Z'='treeID'))
# plot(stem.seg[stem.seg$Site_Name=='SG-NES1', 'treeID'])
