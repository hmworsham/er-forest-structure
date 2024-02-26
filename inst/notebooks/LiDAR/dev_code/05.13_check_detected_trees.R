# Detect trees on full watershed using optimized algorithm and parameter set

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
workerNodes <- rep(workerNodes, availableCores())
nCores <- as.integer(availableCores()-2)
set_lidr_threads(length(workerNodes)-2)

## ---------------------------------------------------------------------------------------------------
# Specify directories
scrdir <- file.path('/global', 'scratch', 'users', 'worsham')
datadir <- file.path(scrdir, 'trees_ls_50m')
datadir.500 <- file.path(scrdir, 'trees')
datadir.500fill <- file.path(scrdir, 'trees_ls_500mfill')
datadir.lmf <- file.path(scrdir, 'trees_lmffw')
outdir <- file.path(scrdir, 'trees_ls_50m_remainder')
dir100 <- file.path(scrdir, 'trees_ls_100m')

## ---------------------------------------------------------------------------------------------------
## Ingest full LAS catalog of decimated points
infiles <- list.files(config$extdata$las_dec, full.names=T)
lascat <- readLAScatalog(infiles)
lascat@output_options$drivers$sf$param$append <- F
lascat@output_options$drivers$sf$param$delete_dsn <- T

## ---------------------------------------------------------------------------------------------------
## Make idealized grid on same resolution as processing and write out
opt_output_files(lascat) <- file.path(outdir, 'grid_{XLEFT}_{YBOTTOM}')
opt_chunk_size(lascat) <- 50
opt_chunk_buffer(lascat) <- 10

plan(multisession, workers=30)
chks <- engine_chunks(lascat)

grid50 <- mclapply(chks, 'slot', 'save', mc.cores=getOption('mc.cores', 30))
grid50 <- unlist(grid50, recursive=F)
grid50 <- lapply(str_split(grid50, '/'), '[', 7)
grid50 <- str_replace_all(grid50, 'grid', 'trees')

comp50 <- tools::file_path_sans_ext(basename(trs.ls.fn))

fullgrid <- do.call(rbind, lapply(chks, FUN=function(x) {
  bb <- st_bbox(x)
  if(!is.na(sum(bb))) {
    bsf <- st_as_sfc(bb)
  }
}))

fullgrid.df <- data.frame('id'=grid50, 'geometry'=fullgrid)
fullgrid.sf <- st_as_sf(fullgrid.df, crs='EPSG:32613')
# st_write(fullgrid.sf, file.path(scrdir, '50mgrid.geojson'), append=F)

## ---------------------------------------------------------------------------------------------------
## Check completion on original LS run on 50m grid
trs.ls50.fn <- list.files(datadir, pattern='shp', full.names=T)

trs.ls50 <- mclapply(trs.ls50.fn, FUN=st_read,
                   mc.cores=getOption("mc.cores", nCores))

grids.comp.50 <- do.call(rbind, mclapply(trs.ls50, FUN=function(x) {
  bb <- st_bbox(x)
  if(!is.na(sum(bb))) {
    bsf <- st_as_sfc(bb)
  } else {
    bsf <- NA
  }
  return(bsf)
}, mc.cores=getOption('mc.cores', nCores)))

ntrs <- unlist(lapply(trs.ls50, nrow))
npercell <- data.frame('n'=ntrs, 'geometry'=grids.comp.50)
npercell <- npercell[npercell$n>0,]

plot(st_as_sf(npercell), lwd=0.01)

## ---------------------------------------------------------------------------------------------------
## Check completion for 500m LS run

trs.ls500.fn <- list.files(datadir.500, pattern='shp', full.names=T)
trs.ls500 <- mclapply(trs.ls500.fn, FUN=st_read, mc.cores=getOption("mc.cores", 16))
#trs.ls500 <- st_as_sf(data.table::rbindlist(trs.ls500))

grids.comp.500 <- do.call(rbind, mclapply(trs.ls, FUN=function(x) {
  bb <- st_bbox(x)
  if(!is.na(sum(bb))) {
    bsf <- st_as_sfc(bb)
  } else {
    bsf <- NA
  }
  return(bsf)
}, mc.cores=getOption('mc.cores', nCores)))

ntrs.500 <- unlist(lapply(trs.ls500, nrow))
ntrs.500 <- ntrs.500[ntrs.500>0]
plot(st_as_sf(data.frame('n'=ntrs.500, 'geometry'=polys.500)), lwd=0.01)

## ---------------------------------------------------------------------------------------------------
## Merge all available LS results from 500m and 50m LS runs

# Ingest the full 50 m grid template
fullgrid.sf <- st_read(file.path(scrdir, '50mgrid.geojson'))
fullgrid.df <- data.frame(fullgrid.sf)

# Define dataframe of completed grid cells from 50 m run
comp50 <- tools::file_path_sans_ext(basename(trs.ls50.fn))
grids.comp50.df <- data.frame('id'=trs.ls50.fn, 'geometry'=grids.comp.50)

# Find which 50m cells were incomplete (empty)
grids.incomp50 <- st_as_sf(fullgrid.df[!fullgrid.df$id %in% comp50,], crs='EPSG:32613')

# Pull all available results from 500m LS run into empty 50m grid cells
st_crs(grids.incomp50) <- st_crs(trs.ls500) <- 'EPSG:32613'
trees.fill500 <- st_intersection(grids.incomp, trs.ls500)
trees.fill500.grid <- split(trees.fill500, f=trees.fill500$id)

# Write 50m cells "filled" with 500m results
lapply(trees.fill500.grid, function(x){st_write(x, dsn=file.path(scrdir, 'trees_ls_500mfill', paste0(x$id[1], '.shp')))})

## ---------------------------------------------------------------------------------------------------
## After merging 50m and 500m results, check for completeness



## ---------------------------------------------------------------------------------------------------
## Find incompletes to run LS again

# Ingest the full 50 m grid template
fullgrid.sf <- st_read(file.path(scrdir, '50mgrid.geojson'))
fullgrid.df <- data.frame(fullgrid.sf)

# Ingest completed tree files
trs.ls.fn <- list.files(datadir, pattern='shp', full.names=T)
trs.ls <- mclapply(trs.ls.fn, FUN=st_read,
                   mc.cores=getOption("mc.cores", nCores))

# Define file IDs and grid cell IDs for comparison
comp.files <- tools::file_path_sans_ext(basename(trs.ls.fn))
fullgrid.id <- tools::file_path_sans_ext(basename(fullgrid.sf$id))

grids.comp <- do.call(rbind, mclapply(trs.ls, FUN=function(x) {
  bb <- st_bbox(x)
  if(!is.na(sum(bb))) {
    bsf <- st_as_sfc(bb)
  } else {
    bsf <- NA
  }
  return(bsf)
}, mc.cores=getOption('mc.cores', nCores)))

grids.df <- data.frame('id'=trs.ls.fn, 'geometry'=grids.comp)

# >>>> START: GENERATE GRID OF INCOMPLETES PROGRAMATICALLY AND WRITE OUT <<<<< #
grids.incomp.d <- find.incompletes(fullgrid.sf, comp.files)

grids.incomp.d.filt <- grids.incomp.d[as.integer(st_area(grids.incomp.d))>=100000]

st_write(grids.incomp.d.filt, file.path(scrdir, '50mgrid_incomp.geojson'), append=F, crs='EPSG:32613')
# >>>> END: GENERATE GRID OF INCOMPLETES PROGRAMATICALLY AND WRITE OUT <<<<< #

# Ingest the grid of incompletes generated above and roll on
grids.incomp.d.filt <- st_read(file.path(scrdir, '50mgrid_incomp.geojson'))
plot(grids.incomp.d.filt, col=viridis(28))

# Subset LAS catalog to large incomplete clusters
tmpdir <- file.path(scrdir, 'las_remainder')
opt_output_files(lascat) <- file.path(tmpdir, 'trees_{XLEFT}_{YBOTTOM}')
lascat.incomp <- clip_roi(lascat, grids.incomp.d.filt)
plot(lascat.incomp, chunk=T)

# Retile incomplete LAS catalog to 50m incomplete grid
tmpdir <- file.path(scrdir, 'las_remainder')
infiles <- list.files(tmpdir, full.names=T)
lascat.incomp <- readLAScatalog(infiles)
plot(lascat.incomp, chunk=T)

opt_stop_early(lascat.incomp) <- F # Proceed through errors, leaving gaps for failed chunks
opt_output_files(lascat.incomp) <- file.path(scrdir, 'las_remainder_regrid', 'las_rem_rg_{XLEFT}_{YBOTTOM}')
opt_chunk_size(lascat.incomp) <- 50
opt_chunk_buffer(lascat.incomp) <- 0

plan(multisession, workers=nCores)
catalog_retile(lascat.incomp)

## ---------------------------------------------------------------------------------------------------
## Find final incompletes and run LS again

# Ingest the full 50 m grid template
fullgrid.sf <- st_read(file.path(scrdir, '50mgrid.geojson'))
fullgrid.df <- data.frame(fullgrid.sf)

# Ingest the completed files in full 50m directory and remainder directory
trs.ls50.fn <- list.files(datadir, pattern='shp', full.names=T)
trs.ls50remainder.fn <- list.files(file.path(scrdir, 'trees_ls_50m_remainder'), pattern='shp', full.names=T)
trs.ls.fn <- c(trs.ls50.fn, trs.ls50remainder.fn)
trs.ls <- mclapply(trs.ls.fn, FUN=st_read,
                   mc.cores=getOption("mc.cores", nCores))

# Define file IDs and grid cell IDs for comparison
comp.files <- tools::file_path_sans_ext(basename(trs.ls.fn))
fullgrid.id <- tools::file_path_sans_ext(basename(fullgrid.sf$id))

grids.comp <- do.call(rbind, mclapply(trs.ls, FUN=function(x) {
  bb <- st_bbox(x)
  if(!is.na(sum(bb))) {
    bsf <- st_as_sfc(bb)
  } else {
    bsf <- NA
  }
  return(bsf)
}, mc.cores=getOption('mc.cores', nCores)))

# Make DF of completed grid
grids.df <- data.frame('id'=trs.ls.fn, 'geometry'=grids.comp)

# Check N per cell in completed grid
ntrs <- unlist(lapply(trs.ls, nrow))
npercell <- data.frame('n'=ntrs, 'geometry'=grids.comp)
npercell <- npercell[npercell$n>0,]
plot(st_as_sf(npercell), lwd=0.01)

# Generate grid of incompletes programatically
grids.incomp.d <- find.incompletes(fullgrid.sf, comp.files)
grids.incomp.d.filt <- grids.incomp.d[as.integer(st_area(grids.incomp.d))>=150000]
grids.incomp.d.filt <- st_as_sf(grids.incomp.d.filt)

# st_write(grids.incomp.d, file.path(scrdir, '50mgrid_incomp3.geojson'), append=F, crs='EPSG:32613')
# grids.incomp.d.filt <- st_read(file.path(scrdir, '50mgrid_incomp2.geojson'))

# Subset LAS catalog to large incomplete clusters
tmpdir <- file.path(scrdir, 'las_remainder3')
opt_output_files(lascat) <- file.path(tmpdir, 'trees_{XLEFT}_{YBOTTOM}')
lascat.incomp <- clip_roi(lascat, grids.incomp.d.filt)

# Retile incomplete LAS catalog to 50m grid
tmpdir <- file.path(scrdir, 'las_remainder3')
infiles <- list.files(tmpdir, full.names=T)
lascat.incomp <- readLAScatalog(infiles)
plot(lascat.incomp, chunk=T)

opt_stop_early(lascat.incomp) <- F # Proceed through errors, leaving gaps for failed chunks
opt_output_files(lascat.incomp) <- file.path(scrdir, 'las_remainder_regrid3', 'las_rem_rg_{XLEFT}_{YBOTTOM}')
opt_chunk_size(lascat.incomp) <- 50
opt_chunk_buffer(lascat.incomp) <- 0

plan(multisession, workers=nCores)
catalog_retile(lascat.incomp)

## ---------------------------------------------------------------------------------------------------
# Resample to 100 m grid
#
# opt_output_files(lascat) <- file.path(scrdir, '100m_grid', 'grid_{XLEFT}_{YBOTTOM}')
# opt_chunk_size(lascat) <- 100
# opt_chunk_buffer(lascat) <- 0
#
# plan(multisession, workers=30)
# chks100 <- engine_chunks(lascat)
#
# grid100 <- mclapply(chks100, 'slot', 'save', mc.cores=getOption('mc.cores', 30))
# grid100 <- unlist(grid100, recursive=F)
# grid100 <- lapply(str_split(grid100, '/'), '[', 7)
# grid100 <- str_replace_all(grid100, 'grid', 'trees')
#
# fullgrid100 <- do.call(rbind, lapply(chks100, FUN=function(x) {
#   bb <- st_bbox(x)
#   if(!is.na(sum(bb))) {
#     bsf <- st_as_sfc(bb)
#   }
# }))
#
# fullgrid.df <- data.frame('id'=grid100, 'geometry'=fullgrid100)
# fullgrid.sf <- st_as_sf(fullgrid.df, crs='EPSG:32613')
# st_write(fullgrid.sf, file.path(scrdir, '100mgrid.geojson'), append=F)

# Ingest 100m grid file
fullgrid100.sf <- st_read(file.path(scrdir, '100mgrid.geojson'))

# Ingest tree files
trs.ls.all.fn <- list.files(datadir, pattern='shp', full.names=T)
trs.ls.all <- mclapply(trs.ls.all.fn, FUN=st_read,
                   mc.cores=getOption("mc.cores", nCores))

trs.ls.all <- trs.ls.all[lapply(trs.ls.all, nrow)>0]
trs.ls.all <- data.table::rbindlist(trs.ls.all, fill=T)
trs.ls.all <- st_as_sf(trs.ls.all, crs='EPSG:32613')

# trees100 <- mcmapply(st_intersection,
#                      fullgrid100.sf,
#                      trs.ls.all,
#                      mc.cores=getOption('mc.cores', nCores))
#
# cl = parallel::makeCluster(detectCores(), type="FORK")
# doParallel::registerDoParallel(cl, detectCores())
#
# gridcells <- unique(fullgrid.sf$id) #different munis
# pols <-  foreach(i=1:length(gridcells)) %dopar% {
#   x <- fullgrid.sf %>% dplyr::filter(id == gridcells[[i]])
#   crop <- st_crop(trs.ls.all, x) %>% st_make_valid()
#
#   # remove invalid polygons
#   # this allows the process to continue. Sometimes topology is not perfectly
#   # made and tinny pols can ruin your process
#   notvalid <- which(s2_is_valid_detail(crop)==FALSE)
#   if(length(notvalid) > 0){crop <- crop[-notvalid,]}
#
#   # intersection between x and eeoval and esfval
#   x2 <- crop %>% st_intersection(x) %>% st_make_valid()
#
#   # "dissolve" to get
#   # x3 <- x2 %>% group_by(NAME, id, name) %>%
#   #   summarize()
#
#   # return
#   x2
# }
#
# parallel::stopCluster(cl)
# toc()

trees100 <- st_intersection(fullgrid100.sf, trs.ls.all)
trees100.grid <- split(trees100, f=trees100$id)

length(trees100.grid)
length(unique(trees100$id))
dim(fullgrid100.sf)

mclapply(trees100.grid, function(x){st_write(x, dsn=file.path(scrdir, 'trees_ls_100m', paste0(x$id[1], '.shp')))},
         mc.cores=getOption('mc.cores', nCores))

trs100.fn <- list.files(dir100, pattern='shp', full.names=T)
trs100.grid <- mclapply(trs100.fn, FUN=st_read,
                   mc.cores=getOption("mc.cores", nCores))

grids100 <- do.call(rbind, mclapply(trs100.grid, FUN=function(x) {
  bb <- st_bbox(x)
  if(!is.na(sum(bb))) {
    bsf <- st_as_sfc(bb)
  } else {
    bsf <- NA
  }
  return(bsf)
}, mc.cores=getOption('mc.cores', nCores)))

ntrs.all <- unlist(lapply(trs100.grid, nrow))

npercell.all <- data.frame('n'=ntrs.all, 'geometry'=grids100)
npercell.all <- npercell.all[npercell.all$n>0,]

plot(st_as_sf(npercell.all), lwd=0.001)


# CHECK TO MAKE SURE NO OVERLAPPING TREES

jkl <- list.files(datadir, pattern='.shp', full.names=T)[120000:120005]

mno <- lapply(jkl, st_read)
mno.bb <- lapply(mno, \(x) st_as_sf(st_as_sfc(st_bbox(x))))
mno.bb <- do.call('rbind', mno.bb)

pqr <- st_as_sf(data.table::rbindlist(mno, idcol='file'))

ggplot(pqr) +
  geom_sf(aes(color=factor(file), alpha=0.2), size=0.1, shape=3) +
  geom_sf(data=mno.bb, fill=NA)

ggplot(mno.bb) +
  geom_sf()

mno.bb

# @ 100

efg <- list.files(dir100, pattern='.shp', full.names=T)[c(1200:1205, 1356:1361)]

hij <- lapply(efg, st_read)
hij.bb <- lapply(hij, \(x) st_as_sf(st_as_sfc(st_bbox(x))))
hij.bb <- do.call('rbind', hij.bb)

bcd <- st_as_sf(data.table::rbindlist(hij, idcol='file'))

ggplot(bcd) +
  geom_sf(aes(color=factor(file), alpha=0.2), size=0.1, shape=3) +
  geom_sf(data=hij.bb, fill=NA)


##
##------------------------------------------------------------------------------
## SCRATCH

## ---------------------------------------------------------------------------------------------------
# Compare ls and lmf

# trs.lmf.fn <- list.files(datadir.lmf, pattern='shp', full.names=T)
# trs.lmf <- mclapply(trs.lmf.fn, FUN=st_read, mc.cores=getOption("mc.cores", 26L))
#
# polys.lmf <- do.call(rbind, lapply(trs.lmf, FUN=function(x) {
#   bb <- st_bbox(x)
#   if(!is.na(sum(bb))) {
#     bsf <- st_as_sfc(bb)
#   }
# }))
#
# ntrs.lmf <- unlist(lapply(trs.lmf, nrow))
# ntrs.lmf <- ntrs.lmf[ntrs.lmf>0]
#
# plot(st_as_sf(data.frame('n'=ntrs.lmf, 'geometry'=polys.lmf)), lwd=0.001)
#


## ---------------------------------------------------------------------------------------------------
# Now merge LMF into final missing grid cells... as a second-best solution

# Filter out 0-row members of LMF-identified trees
# trs.lmf <- trs.lmf[lapply(trs.lmf, nrow)>0]
#
# # Make trs.lmf into an sf object
# trs.lmf <- st_as_sf(data.table::rbindlist(trs.lmf))
# st_crs(grids.incomp) <- st_crs(trs.lmf) <- 'EPSG:32613'
#
# # Extract LMF-identified trees at each grid cell in 50m grid of incompletes
# trees.fill.lmf <- st_intersection(grids.incomp, trs.lmf)
#
# ### Subset trees in LMF fill to homogenize density with LS ###
#
# # Find n trees in LS grid
# View(npercell)
# summary(npercell$n)
#
# # Find n trees per cell in LMF fill
# npercell.lmffill <- trees.fill.lmf %>%
#   group_by(id) %>%
#   count()
#
# summary(npercell.lmffill$n)
#
# plot(st_as_sf(npercell.lmffill))
#
# # Split LMF-filled trees into lists by 50m grid ID
# trees.filllmf.grid <- split(trees.fill.lmf, f=trees.fill.lmf$id)
#
# length(trees.filllmf.grid)
# length(unique(trees.fill.lmf$id))
# dim(grids.incomp)
#
#
# lapply(trees.filllmf.grid, function(x){st_write(x, dsn=file.path(scrdir, 'trees_ls_lmffill', paste0(x$id[1], '.shp')))})
#
# ## ---------------------------------------------------------------------------------------------------
# # After merging 50, 500, and LMF, check for completeness
# trs.ls50.fn <- list.files(datadir, pattern='shp', full.names=T)
# #trs.ls50remainder.fn <- list.files(file.path(scrdir, 'trees_ls_50m_remainder'), pattern='shp', full.names=T)
# #trs.ls500fill.fn <- list.files(file.path(scrdir, 'trees_ls_500mfill'), pattern='shp', full.names=T)
# trs.lslmffill.fn <- list.files(file.path(scrdir, 'trees_ls_lmffill'), pattern='shp', full.names=T)
#
# trs.ls.fn <- c(trs.ls50.fn, trs.lslmffill.fn)
#
# trs.ls.all <- mclapply(trs.ls.fn, FUN=st_read, mc.cores=getOption("mc.cores", 28L))
#
# #trees.filllmf.grid <- lapply(trees.filllmf.grid, st_as_sf)
# #trs.ls.all <- c(trs.ls, trees.filllmf.grid)
# #trs.ls.all <- trs.ls.all[lapply(trs.ls.all, nrow)>0]
#
# grids.comp.all <- do.call(rbind, mclapply(trs.ls.all, FUN=function(x) {
#   bb <- st_bbox(x)
#   if(!is.na(sum(bb))) {
#     bsf <- st_as_sfc(bb)
#   } else {
#     bsf <- NA
#   }
#   return(bsf)
# }, mc.cores=getOption('mc.cores', 24L)))
#
# ntrs.all <- unlist(lapply(trs.ls.all, nrow))
#
# npercell.all <- data.frame('n'=ntrs.all, 'geometry'=grids.comp.all)
# npercell.all <- npercell.all[npercell.all$n>0,]
#
# plot(st_as_sf(npercell.all), lwd=0.001)
#
# trs.lmffill.down <- lapply(trs.ls.all[130415:148084], function(x) {
#
#   if(nrow(x) > 450) {x.down <- sample_frac(x, .8)}
#   else{x.down <- x}
#   x.down
# })
#
# trs.ls.all <- trs.ls.all[1:130414]
# trs.ls.all <- c(trs.ls.all, trs.lmffill.down)
# length(trs.ls.all)
#
# ntrs.all.x <- unlist(lapply(trs.ls.all, nrow))
#
# npercell.all.x <- data.frame('n'=ntrs.all.x, 'geometry'=grids.comp.all)
# npercell.all.x <- npercell.all.x[npercell.all.x$n>0,]
#
# plot(st_as_sf(npercell.all.x), lwd=0.001)


#
# # Resample to 500 m grid
# opt_output_files(lascat) <- file.path(scrdir, '500m_grid', 'grid_{XLEFT}_{YBOTTOM}')
# opt_chunk_size(lascat) <- 0
# opt_chunk_buffer(lascat) <- 0
#
# plan(multisession, workers=30)
# chks500 <- engine_chunks(lascat)
#
# grid500 <- mclapply(chks500, 'slot', 'save', mc.cores=getOption('mc.cores', 30))
# grid500 <- unlist(grid500, recursive=F)
# grid500 <- lapply(str_split(grid500, '/'), '[', 7)
# grid500 <- str_replace_all(grid500, 'grid', 'trees')
#
# fullgrid500 <- do.call(rbind, lapply(chks500, FUN=function(x) {
#   bb <- st_bbox(x)
#   if(!is.na(sum(bb))) {
#     bsf <- st_as_sfc(bb)
#   }
# }))
#
# fullgrid.df <- data.frame('id'=grid500, 'geometry'=fullgrid500)
# fullgrid.sf <- st_as_sf(fullgrid.df, crs='EPSG:32613')
# #st_write(fullgrid.sf, file.path(scrdir, '500mgrid.geojson'), append=F)
# fullgrid.sf <- st_read(file.path(scrdir, '500mgrid.geojson'))
#
# trs.ls.all <- trs.ls.all[lapply(trs.ls.all, nrow)>0]
# trs.ls.all <- data.table::rbindlist(trs.ls.all, fill=T)
# trs.ls.all <- st_as_sf(trs.ls.all, crs='EPSG:32613')
# # st_crs(trs.ls.all) <- st_crs(fullgrid.sf)
#
# trees500 <- mcmapply(st_intersection, fullgrid.sf, trs.ls.all, mc.cores=getOption('mc.cores', 24))
# cl = parallel::makeCluster(detectCores(), type="FORK")
# doParallel::registerDoParallel(cl, detectCores())
#
# gridcells <- unique(fullgrid.sf$id) #different munis
# pols = foreach(i=1:length(gridcells)) %dopar% {
#   x <- fullgrid.sf %>% dplyr::filter(id == gridcells[[i]])
#   crop <- st_crop(trs.ls.all, x) %>% st_make_valid()
#
#   # remove invalid polygons
#   # this allows the process to continue. Sometimes topology is not perfectly
#   # made and tinny pols can ruin your process
#   notvalid <- which(s2_is_valid_detail(crop)==FALSE)
#   if(length(notvalid) > 0){crop <- crop[-notvalid,]}
#
#   # intersection between x and eeoval and esfval
#   x2 <- crop %>% st_intersection(x) %>% st_make_valid()
#
#   # "dissolve" to get
#   # x3 <- x2 %>% group_by(NAME, id, name) %>%
#   #   summarize()
#
#   # return
#   x2
# }
#
# parallel::stopCluster(cl)
# toc()
#
# trees500 <- st_intersection(fullgrid.sf, trs.ls.all)
# trees500.grid <- split(trees500, f=trees500$id)
#
# length(trees500.grid)
# length(unique(trees500$id))
# dim(fullgrid.sf)
#
# mclapply(trees500.grid, function(x){st_write(x, dsn=file.path(scrdir, 'trees_ls_500m', paste0(x$id[1], '.shp')))},
#          mc.cores=getOption('mc.cores', 16L))
#
# trees500.grid.fn <- list.files(file.path(scrdir, 'trees_ls_500m'), pattern='shp', full.names=T)
# trees500.grid <- lapply(trees500.grid.fn, st_read)
#
# grids.500 <- do.call(rbind, mclapply(trees500.grid, FUN=function(x) {
#   bb <- st_bbox(x)
#   if(!is.na(sum(bb))) {
#     bsf <- st_as_sfc(bb)
#   } else {
#     bsf <- NA
#   }
#   return(bsf)
# }, mc.cores=getOption('mc.cores', 24L)))
#
# ntrs.500 <- unlist(lapply(trees500.grid, nrow))
#
# npercell.500 <- data.frame('n'=ntrs.500, 'geometry'=grids.500)
# npercell.500 <- npercell.500[npercell.500$n>0,]
#
# plot(st_as_sf(npercell.500), lwd=0.001)
#

