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

# Ingest las
infiles <- list.files(config$extdata$las_dec, full.names=T)
lascat <- readLAScatalog(infiles)

## ---------------------------------------------------------------------------------------------------
## Create CHM

# Processing controls
plan(multisession, workers=nCores)
opt_output_files(lascat) <- file.path(config$extdata$scratch, 'chm', 'chm_{XLEFT}_{YBOTTOM}')
opt_chunk_size(lascat) <- 500
opt_chunk_buffer(lascat) <- 10

# Make CHM with p2r algorithm
chm.pitfree.025 <- rasterize_canopy(lascat, 0.5, p2r(.2), pkg='terra', overwrite=T)
plot(chm.pitfree.025)

# Smooth CHM
kernel <- matrix(1,7,7)
chm.smooth <- focal(chm.pitfree.025, w = kernel, fun = mean, na.rm = TRUE)
plot(chm.smooth)

# Write smoothed CHM
writeRaster(chm.smooth, file.path(config$extdata$scratch, 'chm.smooth.tif'))

# Mask CHM to conifer forest
chm.smooth <- rast(file.path(config$extdata$scratch, 'chm.pitfree.smooth.tif'))
full.mask <- rast(file.path(config$extdata$scratch, 'tifs', 'fullmask_5m.tif'))
full.mask <- alignfun(full.mask, chm.smooth)
chm.masked <- mask(chm.smooth, full.mask)

# Write masked smoothed CHM
writeRaster(chm.masked, file.path(config$extdata$scratch, 'chm_smooth_masked.tif'))

## ---------------------------------------------------------------------------------------------------
## Segment crowns

# Cast tree crown objects as sf
# stem.sf.crp <- st_crop(stem.sf, ext(chm.smooth))
# stem.sf.crp <- stem.sf[st_as_sfc(st_bbox(chm.smooth)),]
# st_crs(stem.sf.crp) <- st_crs(chm.smooth)
# stem.sf.crp$treeID <- 1:nrow(stem.sf.crp)
# plot(stem.sf.crp, add=T)

crowns_poly <- mcws(stem.sf, chm.masked, minHeight=quantile(stem.sf$H, .8), format='polygons')

st_write(crowns_poly)

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


## ---------------------------------------------------------------------------------------------------
## Do it with Height-dependent buffer for trees > 90th percentile height

stem.sf <- stem.sf %>%
  mutate(Crown_Radius=0.082*H + 0.5) %>%
  filter(H >= quantile(H, 0.9))

stem.buff <- st_buffer(stem.sf, dist=0.5*stem.sf$Crown_Radius,
                       endCapStyle = 'SQUARE', joinStyle='MITRE')

#stems.spp <- get.spp(sp.class, stem.buff, sp.codes)

stems.spp <- exactextractr::exact_extract(sp.class, stem.buff, 'mode', progress=T)
stems.spp.sf <- cbind(stem.sf, 'Pixel_Code'=stems.spp)
stems.spp.sf <- left_join(stems.spp.sf, sp.codes, by='Pixel_Code')

data.table::fwrite(stems.spp.sf,
                   file.path(config$extdata$scratch, 'stems_species.csv'))
