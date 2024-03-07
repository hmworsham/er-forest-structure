# Predict DBH on modeled trees

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

# Configure drive auth
drive_auth(path=config$drivesa)

# Set number of cores
nCores <- as.integer(availableCores()-2)

## ---------------------------------------------------------------------------------------------------
## Data ingest

# Ingest detected trees
# treefiles <- list.files('/global/scratch/users/worsham/trees_ls_100m_csv', pattern='.csv', full.names=T)
# trees <- mclapply(treefiles, read.csv, mc.cores=getOption('mc.cores', nCores))
alltrees <- read_csv(file.path(config$extdata$scratch, 'trees_masked_5m.csv'))

# Cast trees as sf
stem.sf <- st_as_sf(alltrees, coords=c('X', 'Y'), crs=32613)

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

# Ingest las
infiles <- list.files(config$extdata$las_dec, full.names=T)
lascat <- readLAScatalog(infiles)

## ---------------------------------------------------------------------------------------------------
## Assign species to tree objects using height-dependent buffer for trees > 90th percentile height

stem.sf <- stem.sf %>%
  mutate(Crown_Radius=0.082*H + 0.5) %>%
  filter(H >= quantile(H, 0.9))

stem.buff <- st_buffer(stem.sf, dist=0.5*stem.sf$Crown_Radius,
                       endCapStyle = 'SQUARE', joinStyle='MITRE')

stems.spp <- exactextractr::exact_extract(sp.class, stem.buff, 'mode', progress=T)
stems.spp.sf <- cbind(stem.sf, 'Pixel_Code'=stems.spp)
stems.spp.sf <- left_join(stems.spp.sf, sp.codes, by='Pixel_Code')

data.table::fwrite(stems.spp.sf,
                   file.path(config$extdata$scratch, 'stems_species.csv'))

## ---------------------------------------------------------------------------------------------------
## SCRATCH

## Crown segmentation approach

# Cast tree crown objects as sf
# stem.sf.crp <- st_crop(stem.sf, ext(chm.smooth))
# stem.sf.crp <- stem.sf[st_as_sfc(st_bbox(chm.smooth)),]
# st_crs(stem.sf.crp) <- st_crs(chm.smooth)
# stem.sf.crp$treeID <- 1:nrow(stem.sf.crp)
# plot(stem.sf.crp, add=T)
#
# full.mask <- rast(file.path(config$extdata$scratch, 'tifs', 'fullmask_5m.tif'))
# ext(full.mask) <- ext(chm.smooth)
# full.mask <- alignfun(full.mask, chm.smooth)
#
# chm.masked <- mask(chm.smooth, full.mask)
#
# crowns_poly <- mcws(stem.sf, chm.masked, minHeight=quantile(stem.sf$H, .8), format='polygons')
#
# st_write(crowns_poly)
#
# plot(crowns_poly, col=NA, add=T)
#
# stem.seg <- st_join(st_as_sf(crowns_poly), stem.sf.crp, by=c('treeID'))
#
# # Filter only top trees
# stem.filt.e <- stem.seg %>%
#   filter(H >= quantile(H, 0.8)) %>%
#   ungroup() %>%
#   mutate(Crown_Area=st_area(.))
#
# plot(stem.filt.e, col=NA, add=T)
#
# # Shrink crowns
# stem.filt.e <- st_buffer(stem.filt.e, -0.2*sqrt(stem.filt.e$Crown_Area))
#
# # Filter only top trees
# stem.filt.e <- stem.seg %>%
#   group_by(Site_Name) %>%
#   filter(Zpred >= quantile(Zpred, 0.9)) %>%
#   ungroup() %>%
#   mutate(Crown_Area=st_area(.))
