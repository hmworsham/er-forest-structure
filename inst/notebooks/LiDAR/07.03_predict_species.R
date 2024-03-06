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
alltrees <- read_csv(file.path(config$extdata$scratch, 'trees_masked_5m.csv'))

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

plan(multisession, workers=nCores)
opt_output_files(lascat) <- file.path(config$extdata$scratch, 'chm', 'chm_{XLEFT}_{YBOTTOM}')
opt_chunk_size(lascat) <- 500
opt_chunk_buffer(lascat) <- 10
#set_lidr_threads(nCores)

chm.pitfree.025 <- rasterize_canopy(lascat, 0.5, p2r(.2), pkg='terra', overwrite=T)

plot(chm.pitfree.025)

plot(chm.pitfree.025)
kernel <- matrix(1,7,7)
chm.smooth <- focal(chm.pitfree.025, w = kernel, fun = mean, na.rm = TRUE)
plot(chm.smooth)

# Get tree crown objects
stem.sf <- st_as_sf(alltrees, coords=c('X', 'Y'), crs=32613)
stem.sf.crp <- st_crop(stem.sf, ext(chm.smooth))
# stem.sf.crp <- stem.sf[st_as_sfc(st_bbox(chm.smooth)),]
st_crs(stem.sf.crp) <- st_crs(chm.smooth)
stem.sf.crp$treeID <- 1:nrow(stem.sf.crp)
plot(stem.sf.crp, add=T)

crowns_poly <- mcws(stem.sf.crp, chm.smooth, minHeight=quantile(stem.sf.crp$H, .8), format='polygons')

plot(crowns_poly, col=NA, add=T)

stem.seg <- st_join(st_as_sf(crowns_poly), stem.sf.crp, by=c('treeID'))

# Filter only top trees
stem.filt.e <- stem.seg %>%
  filter(H >= quantile(H, 0.8)) %>%
  ungroup() %>%
  mutate(Crown_Area=st_area(.))

plot(stem.filt.e, col=NA, add=T)

# Shrink crowns
stem.filt.e <- st_buffer(stem.filt.e, -0.2*sqrt(stem.filt.e$Crown_Area))

# Filter only top trees
stem.filt.e <- stem.seg %>%
  group_by(Site_Name) %>%
  filter(Zpred >= quantile(Zpred, 0.9)) %>%
  ungroup() %>%
  mutate(Crown_Area=st_area(.))

# Plot CHM
crowns_poly <- lapply(chm.smooth, mcws, treetops = stem.sf, minHeight = 3, format='polygons')
plot(chm.smooth[[10]], xlab = "", ylab = "", xaxt='n', yaxt = 'n')
plot(crowns_poly[[10]]$geometry, border = "blue", lwd = 0.5, add = TRUE)

stem.seg <- do.call('rbind', crowns_poly)
stem.seg <- st_join(st_as_sf(stem.seg), stem.sf, by=c('treeID'))

# Filter only top trees
stem.filt.e <- stem.seg %>%
  group_by(Site_Name) %>%
  filter(Zpred >= quantile(Zpred, 0.9)) %>%
  ungroup() %>%
  mutate(Crown_Area=st_area(.))

# Shrink crowns
stem.filt.e <- st_buffer(stem.filt.e, -0.2*sqrt(stem.filt.e$Crown_Area))

