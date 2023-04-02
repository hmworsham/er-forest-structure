# Downsample (decimate) points to 
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
scrdir <- file.path('/global', 'scratch', 'users', 'worsham')
shapedir <- file.path(scrdir, 'EastRiverInputs/RMBL_2020_EastRiver_SDP_Boundary')
datadir <- file.path(scrdir, 'gridded_returns')
neondir <- file.path(scrdir, 'neon_las_regridded')

############################
# Prep data and visualize
############################

# Ingest gridded points
infiles <- list.files(datadir, full.names=T)

# # Make raster grid and convert to vector grid
aop <- vect(file.path(shapedir, 'RMBL_2020_EastRiver_SDP_Boundary', 'SDP_Boundary.shp'))
aop.ras <- rast(ext(aop)+1000, nrows=30, ncols=30, crs=crs(aop))
values(aop.ras) <- seq(1:900)
aop.grid <- as.polygons(aop.ras)

# Ingest gridded point counts
returncounts <- read.table(file.path(scrdir, 'gridded_returns_wc.txt'))

# Add gridcell vector to dataframe
returncounts['gridcell'] <- as.numeric(unlist(lapply(str_split(returncounts$V2, './|_'), '[', 2)))

# Set any point count=1 to NA
returncounts[returncounts['V1']==1,]$V1 <- NA
returncounts <- returncounts[order(returncounts$gridcell),]

# Assign point counts as raster cell values
values(aop.ras) <- returncounts$V1

# Plot heatmap of returns
plot(aop.ras, col=heat.colors(100))
lines(aop, col='grey10', lwd=3)