# Load libraries
library(terra)
library(parallel)
library(tidyverse)
library(plyr)
library(RColorBrewer)
library(pbmcapply)

shapedir <- '/global/scratch/users/worsham/EastRiver/RMBL_2020_EastRiver_SDP_Boundary'
datadir <- '/global/scratch/users/worsham/geolocated_returns'
outdir <- '/global/scratch/users/worsham/gridded_returns'
dir.create(outdir)

############################
# Prep data and visualize
############################

# Make raster grid and convert to vector grid
aop <- vect(file.path(shapedir, 'RMBL_2020_EastRiver_SDP_Boundary', 'SDP_Boundary.shp'))
aop.ras <- rast(ext(aop)+1000, nrows=30, ncols=30, crs=crs(aop))
values(aop.ras) <- seq(1:900)
aop.grid <- as.polygons(aop.ras)

# Visualize aop domain and overlying grid
plot(aop.grid, col=brewer.pal(5,'Greens'))
lines(aop, col='tomato1', lwd=3)
polys(aop.grid[535], col='tomato1', border='tomato1')

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

############################
# Ingest data and regrid
############################

# Specify input files
infiles <- list.files(datadir, full.names=T)

# Ingest one csv of returns to get column names 
somepoints <- read.csv(infiles[40], nrows=5)
l1 <- data.frame(t(names(somepoints)))
rm(somepoints)

# Write empty csv files with correct header to populate later
for(i in seq_along(aop.grid)) {
  write.table(
    l1, 
    file.path(outdir, paste0(sprintf('%03d', i), '_gridded_returns.csv')), 
    col.names=F, 
    row.names=F, 
    sep=',')
}

# Ingest a subset of files and make dataframe
nsplits = 24
seqs <- seq(length(infiles))
seqs <- split(seqs, cut(seq_along(seqs), nsplits))

# Define seq of length of grid
gridseq <- seq_along(aop.grid)

# Define safe csv read function
saferead <- function(x){
  tryCatch(read.csv(x, header=T), 
           error = function(cond) {
             message(paste('Reading csv failed'))
           })
}

# Working in chunks of length = length(infiles)/n_splits, regrid points and write to csv
for(s in seqs){
  res <- mclapply(infiles[s], saferead, mc.cores=getOption('mc.cores', 30))
  pcloud <- do.call(rbind.fill, res)

  # Find which points are in grid cells
  regrid <- function(list.element, grid, ptset){
    exti <- ext(grid[list.element])
    gci <- list.element
    hits <- which(ptset$px>=exti[1] & ptset$px<exti[2] & ptset$py>=exti[3] & ptset$py<exti[4])
    ptsub <- ptset[hits,]
    
    # If there are points in the grid, append them to the csv corresponding to that grid cell
    if(dim(ptsub)[1]>0){
      print(c(gci, dim(ptsub)))
      write.table(
        ptsub,
        file.path(outdir, paste0(sprintf('%03d', gci), '_gridded_returns.csv')),
        row.names=F,
        col.names=F,
        sep=',',
        append=T)
    }
  }
  # Apply function `regrid` to all grid cells
  pbmclapply(gridseq, regrid, aop.grid, pcloud, mc.cores=getOption('mc.cores', 16))
  
  # Clean up
  rm(res)
  rm(pcloud)
  gc()
}
