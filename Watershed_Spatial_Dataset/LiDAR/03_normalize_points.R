# Load libraries
options(rgl.useNULL=TRUE)
library(terra)
library(parallel)
library(tidyverse)
library(plyr)
library(RColorBrewer)
library(pbmcapply)
library(devtools)
library(lidR)
library(rlas)
library(rgdal)
library(rgl)
load_all('~/Repos/rwaveform')

# Setup workspace
scrdir <- '/global/scratch/users/worsham'
shapedir <- file.path(scrdir, 'EastRiver/RMBL_2020_EastRiver_SDP_Boundary')
datadir <- file.path(scrdir, 'las_regridded')
geredir <- file.path(scrdir, 'geolocated_returns')
neondir <- file.path(scrdir, 'neon_las_regridded')
outdir <- file.path(scrdir, 'las_decimated')
dir.create(outdir)

############################
# Prep data
############################

# Ingest gridded points
infiles <- list.files(datadir, full.names=T)
lascat <- readLAScatalog(infiles)

# Ingest original las dtm
dtm <- rast(file.path(scrdir, 'EastRiver', 'dtm_mosaic.tif'))

# Set chunk buffer and plot
opt_chunk_buffer(lascat) <- 50
plot(lascat, chunk = TRUE)
summary(lascat)

las <- readLAS(infiles[99])
gn1 <- las - dtm
las.cg <- classify_ground(gn1, algorithm = pmf(ws = ws, th = th))

hist(gn1$Z)
hist(gn1[gn1$Z>0]$Z)
lidR::plot(gn1[gn1$Z>=0][1:50000], color='Z', bg='white', legend=T, nbreaks=6)
rglwidget()




#######################
# Scratch
#######################
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


# opt_output_files(lascat) <- file.path(outdir, '{ORIGINALFILENAME}_decimated')
# dp <- decimate_points(lascat, homogenize(8, 1))
# 
# myMetrics = function(z, i) {
#   metrics = list(
#     zwimean = sum(z*i)/sum(i), # Mean elevation weighted by intensities
#     zimean  = mean(z*i),       # Mean products of z by intensity
#     zsqmean = sqrt(mean(z^2))) # Quadratic mean
#   
#   return(metrics)
# }
# 
# hmean <- pixel_metrics(lascat, ~myMetrics(Z,Intensity), 20)

