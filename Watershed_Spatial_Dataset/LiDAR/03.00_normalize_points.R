# Normalize points to ground surface

# Load libraries
options(rgl.useNULL=TRUE)

# Install and load libraries
pkgs <- c('future',
          'lidR',
          'raster',
          'rgl',
          'terra') # Name the packages you want to use here

# Function to install new packages if they're not already installed
load.pkgs <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 

# Runs the function on the list of packages defined in pkgs
load.pkgs(pkgs)

# Setup workspace
scrdir <- '/global/scratch/users/worsham'
shapedir <- file.path(scrdir, 'EastRiverInputs/RMBL_2020_EastRiver_SDP_Boundary')
datadir <- file.path(scrdir, 'las_regridded')
geredir <- file.path(scrdir, 'geolocated_returns')
neondir <- file.path(scrdir, 'neon_las_regridded')
outdir <- file.path(scrdir, 'las_normalized')
dir.create(outdir)

############################
# Prep data
############################

# Ingest gridded points as las catalog
infiles <- list.files(datadir, full.names=T)
lascat <- readLAScatalog(infiles)
summary(lascat)

# Plot las catalog by number of points in file
lascat <- lascat[lascat['Number.of.point.records']$Number.of.point.records>0,]
plot(lascat['Number.of.point.records'])

# Ingest original las dtm
dtm <- rast(file.path(scrdir, 'EastRiverInputs', 'dtm_mosaic.tif'))
dtm <- raster(dtm) # Coerce to raster, as SpatRaster isn't serializable for pll processing

# Set chunk buffer and other catalog processing params
#opt_chunk_size(lascat) <- 1000
opt_chunk_buffer(lascat) <- 20
opt_output_files(lascat) <-  paste0(outdir, '/las_norm_{XLEFT}_{YBOTTOM}')
opt_laz_compression(lascat) <- T

# Plot catalog showing buffered chunks to process
plot(lascat, chunk = TRUE)
summary(lascat)

# Set up parallel processing
plan(multisession)

# Normalize height on full las catalog
lascat_norm <- normalize_height(lascat, tin(), dtm=dtm)

#######################
# Scratch
#######################

# Normalize height with 2 methods
las <- readLAS(infiles[1])

# Method 1 = simple subtraction
gn1 <- las - dtm
gn1 <- gn1[gn1$Z>=0]
gn1

# Method 2 = `normalize_height` interpolating over dtm
gn2 <- normalize_height(las, tin(), dtm = dtm)
fc10 = c('#654321', forest.colors(15))
lidR::plot(gn2[gn2$Z>=0][1:50000], pal=fc10, color='Z', bg='white', legend=T, nbreaks=15)
rglwidget()

# Attemps at a safe function for normalization

## Attempt 1
norm_height <- function(chunk, srf) {
  pc <- readLAS(chunk)                  # read the chunk
  if (is.empty(pc)) return(NULL)        # check if it actually contain points
  lasnorm <- tryCatch({pc-srf}, 
                      error=function(cond){message('Normalization error')
                        return(NULL)}
  ) # apply computation of interest
  return(lasnorm) # output
}

output <- catalog_apply(lascat, norm_height, dtm)

## Attempt 2
lascat_norm <- tryCatch({normalize_height(lascat, tin(), dtm=dtm)},
                        error=function(cond) {
                          message('Oops!')
                          message("Here's the original error message:")
                          message(cond)
                          # Choose a return value in case of error
                          return(NA)})

output <- lapply(list.files(datadir, full.names=T), norm_height, dtm)

# Screwing around with plotting merged file in RGB
# rgbmap <- terra::rast(file.path(scrdir, 'EastRiverInputs/aop_naip_ortho.tif'))
# 
# terra::plot(rgbmap)
# 
# norm_color <- function(las, rgbmap, dtm) # create user-defined function
# {
#   nlas <- normalize_height(las, tin(), dtm=dtm) # normalize
#   nlas <- nlas[nlas$Z>=0]
#   colorized <- merge_spatial(nlas, rgbmap) # colorize
#   return(colorized) # output
# }
# 
# cols <- rgb(maxColorValue = 255)
# 
# rgb_las <- norm_color(las, rgbmap, dtm)
# rgb_las$R <- as.integer(rgb_las$R / max(rgb_las$R)*255)
# rgb_las$G <- as.integer(rgb_las$G / max(rgb_las$G)*255)
# rgb_las$B <- as.integer(rgb_las$B / max(rgb_las$B)*255)
# lidR::plot(rgb_las[1:500000], color = "G", pal=rgb(rgb_las$R, rgb_las$G, rgb_las$B, maxColorValue=255), bg='white', size=2)
# rglwidget()



# Screwing around with ground classification
las <- readLAS(infiles[99])
ws <- seq(3, 15, 5)
th <- seq(0.1, 0.8, length.out = length(ws))

las.cg <- classify_ground(las, algorithm = pmf(ws = ws, th = th))
las <- classify_ground(las, algorithm = csf(sloop_smooth = TRUE, class_threshold = 1, cloth_resolution = 0.85, time_step = 0.9))

#cg1 <- classify_ground(las1, mcc())
plot(las.cg[1:50000], color = "Classification", size = 3, bg = "white") 
rglwidget()

p1 <- c(330363, 4316560)
p2 <- c(330500, 4316570)
plot_crossection <- function(las,
                             p1 = c(min(las@data$X), mean(las@data$Y)),
                             p2 = c(max(las@data$X), mean(las@data$Y)),
                             width = 4, colour_by = NULL)
{
  colour_by <- enquo(colour_by)
  data_clip <- clip_transect(las, p1, p2, width)
  p <- ggplot(data_clip@data, aes(X,Z)) + geom_point(size = 0.5) + coord_equal() + theme_minimal()
  
  if (!is.null(colour_by))
    p <- p + aes(color = !!colour_by) + labs(color = "")
  
  return(p)
}

plot_crossection(las, colour_by = factor(Classification))

gnd <- filter_ground(las[1:50000])
plot(gnd, size = 3, bg = "white") 
rglwidget()

dtm <- rasterize_terrain(las, 1, knnidw())

gn1 <- normalize_height(las, knnidw())
gn2 <- classify_ground(gn1, algorithm = pmf(ws = ws, th = th))
