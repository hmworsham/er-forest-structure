# Predict species of modeled trees
# Author: Marshall Worsham | worsham@berkeley.edu
# Created: 03-21-24
# Revised: 07-23-24

#############################
# Set up working environment
#############################

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

# Configure drive auth
drive_auth(path=config$drivesa)

# Define parallel scope
nCores <- as.integer(availableCores()-2)

#############################
# Data ingest
#############################

# Ingest detected trees
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

# Ingest CHM
chm.masked <- rast(file.path(config$extdata$scratch, 'chm_full_extent', 'chm_smooth_masked.tif'))

#############################
# Assign species
#############################

## Assign species to tree objects using height-dependent buffer for trees > 90th percentile height

# Filter to 90th percentile height
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

#############################
# Crown segmentation approach
#############################

# Ingest trees
treefiles <- list.files('/global/scratch/users/worsham/trees_ls_100m', pattern='.shp', full.names=T)

segtrees <- function(treefile, chmraster, spraster, spcodes) {
  t1 <- st_read(treefile, quiet=T)
  t1$treeID <- 1:nrow(t1)
  chm1 <- tryCatch({
    crop(chmraster, ext(t1))},
    error=function(e) {NULL}
    )

  if(is.null(chm1)) {return(crowns <- NULL)}

  if(sum(!is.na(minmax(chm1)))>0) {
    crowns <- mcws(t1, chm1, minHeight=min(t1$Z)-1,
                   format='polygon', IDfield='treeID')

    crowns <- crowns %>%
      left_join(data.frame(t1), by='treeID') %>%
      filter(Z>=quantile(Z, 0.9)) %>%
      mutate(Crown_Area=st_area(.),
             X=st_coordinates(geometry.y)[,1],
             Y=st_coordinates(geometry.y)[,2]) %>%
      dplyr::select(-geometry.y)

    crowns <- crowns %>%
      st_buffer(-0.2*sqrt(crowns$Crown_Area))

    stems.spp <- tryCatch({
      exactextractr::exact_extract(spraster, crowns, 'mode')},
      error=function(e) {NULL})

    if(!is.null(stems.spp)) {
      crowns <- cbind(crowns, 'Pixel_Code'=stems.spp)
      crowns <- left_join(crowns, spcodes, by='Pixel_Code')
    } else {return(NULL)}

    crowns <- crowns %>%
      data.frame() %>%
      dplyr::select(-c(id_1, Class, geometry.x))

    data.table::fwrite(crowns,
                       file.path(config$extdata$scratch, 'trees_species_100m_csv', paste0(crowns$id[1], '_species.csv')),       )

  } else {crowns <- NULL}

  return(crowns)
}

# Segment crowns and predict species
mclapply(treefiles, segtrees, chm.masked, sp.class, sp.codes,
                 mc.cores=getOption('mc.cores', nCores)
                 )
#############################
# Mask and write
#############################

# Ingest tree species csvs
trees.spp.fn <- list.files(file.path(config$extdata$scratch, 'trees_species_100m_csv'), full.names=T)
trees.spp <- mclapply(trees.spp.fn, read.csv, mc.cores=getOption('mc.cores', nCores))
trees.spp <- data.table::rbindlist(trees.spp, idcol='file')
trees.spp <- trees.spp %>%
  dplyr::select(-c(file))

# Read full mask
full.mask <- rast(file.path(config$extdata$scratch, 'tifs', 'fullmask_5m.tif'))
full.mask.poly <- st_as_sf(as.polygons(full.mask))
full.mask.poly <- st_cast(full.mask.poly, 'POLYGON')
full.mask.area <- as.numeric(st_area(full.mask.poly))
full.mask.poly <- full.mask.poly[full.mask.area>=1000,]

# Subset detected trees to unmasked zones
alltrees.df <- data.frame(trees.spp)
trees_geos <- geos::geos_read_xy(alltrees.df[c("X", "Y")])
full.mask.geos <- geos::as_geos_geometry(full.mask.poly)
wk::wk_crs(full.mask.geos) <- NULL
trees_tree <- geos::geos_strtree(trees_geos)
keys <- geos::geos_contains_matrix(full.mask.geos, trees_tree)
trees_conif <- alltrees.df[unlist(keys),]

# Write masked trees as csv
data.table::fwrite(trees.spp, file.path(config$extdata$scratch, 'trees_species_masked_5m.csv'), append=F)
