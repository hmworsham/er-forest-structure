
# Set up workspace
## Install and load libraries
pkgs <- c('dplyr',
          'tidyverse',
          'ggplot2',
          'raster',
          'rgdal',
          'sf',
          'ggspatial',
          'rasterVis',
          'XML',
          'RCurl',
          'googledrive',
          'ggfortify') # Name the packages you want to use here
load.pkgs <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} # Function to install new packages if they're not already installed
load.pkgs(pkgs) # Runs the function on the list of packages defined in pkgs

# Set working directory.
setwd('~/Desktop/RMBL/Projects/Watershed_Spatial_Dataset/')
indir <- '~/Google Drive (worsham@berkeley.edu)/Research/RMBL/RMBL_East River Watershed Forest Data/Data/Remote_Sensing/Henry_2019_SiteSelection/2019 Analysis Layers/Original Data/From Nicola 5-2019'
outdir <- paste0(getwd(), '/Output')

##Check GDAL version (should work with GDAL 2.2.3 or later).
getGDALVersionInfo()

# TWI Plot
topo.files <- list.files(indir, recursive = T, pattern = '.tif$')
topo.tifs <- list(paste0(indir, '/', topo.files))[[1]]
topo.tifs <- topo.tifs[c(1,3:6,8,10)]

topo.factors <- list()
for(i in 1:length(topo.tifs)) {
  ras = raster(topo.tifs[i])
  topo.factors[[i]] = ras
}
topo.factors[[2]] <- crop(topo.factors[[2]], topo.factors[[3]])

topo.stack <- stack(topo.factors)

# Cover raster
cover <- raster('~/Google Drive (worsham@berkeley.edu)/Research/RMBL/RMBL_East River Watershed Forest Data/Data/Remote_Sensing/RMBL_2020_EastRiver_SDP/UER_landcover_1m_v4.tif')

# Plot points
plot.dir <- '~/Google Drive (worsham@berkeley.edu)/Research/RMBL/RMBL_East River Watershed Forest Data/Data/Inventory Plots'

plot.files <- list(paste0(plot.dir, '/', list.files(plot.dir), '/', list.files(plot.dir), '_Boundary'))[[1]][c(1:2, 5:6,8:13)]
plot.files

plot.list <- list()
for(j in 1:length(plot.files)){
  st = st_read(plot.files[j])
  plot.list[[j]] = st
}

plot.pts <- lapply(plot.list, FUN = function(x) {list(long = as.list(extent(x))[1], lat = as.list(extent(x))[3])})

plot.pts.tbl <- do.call(rbind, plot.pts)
plot.pts.df <- as.data.frame(plot.pts.tbl)
plot.pts.df$long <- as.numeric(unlist(plot.pts.df$long))
plot.pts.df$lat <- as.numeric(unlist(plot.pts.df$lat))

topo.values <- extract(topo.stack, plot.pts.df)
topo.pt.values <- cbind(plot.pts.df, topo.values)

pca <- prcomp(topo.values)

topo.sample <- sampleRandom(topo.stack, 60)
all.sample <- as.factor(extract(cover, topo.sample[,1:2]))
all.sample <- cbind(all.sample, topo.sample)
pca <- prcomp(topo.sample)
pca
autoplot(pca, data = all.sample, colour = 'all.sample', loadings = T) +
  scale_colour_distiller()

biplot(pca, 1:2, col = c('black', 'blue3'), scale = 1)
?biplot()

screeplot(pca)


# Plot TWI
twi.plot <- gplot(twi, maxpixels=1000000) +
  geom_raster(aes(fill=value), interpolate=TRUE) +
  scale_fill_gradient(low = 'black', high = 'white',
  ) +
  scale_x_continuous("") +
  scale_y_continuous("") +
  coord_sf(expand=0, label_axes="--EN") +
  annotation_scale(style="ticks",width_hint=0.1,location="br",
                   tick_height=0,text_col="white",line_col="white") +
  theme_bw() +
  theme(legend.position='bottom',
        legend.key.width = unit(2, 'lines'))

twi.plot

##Writes plot to disk.
png("structure_map.png",width=6,height=8,units="in",res=300)
gplot
dev.off()

##Writes subset of raster data to disk.
writeRaster(structure.stack, filename="Schofield_Structure_Rasters.tif",
            progress='text')
