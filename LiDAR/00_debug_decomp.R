# Install and load typical libraries
pkgs <- c('dplyr',
          'tidyverse',
          'ggplot2',
          'raster',
          'data.table',
          'devtools',
          'plotly',
          'rPeaks',
          'rwaveform',
          'rgdal',
          'caTools',
          'sf', 
          'parallel',
          'itcSegment',
          'rlist') # Name the packages you want to use here

# Function to install new packages if they're not already installed
load.pkgs <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 
# Runs the function on the list of packages defined in pkgs
load.pkgs(pkgs)

# Debug
aoi <- 'ER-GT1'
itx_true <- intersects[intersects[aoi] == T,1]
aoi_fps <- file.path(datadir, itx_true)
testfp <- aoi_fps[1]
testfp
failset <- ingest(testfp)
failclip <- doclip(failset)

for(i in 2490:2492){
  x = decom.adaptive(failclip$return[i])
}

dc = tryCatch(apply(failclip$return[2492:2495], 1, decom.adaptive, thres = 0.22), error = function(e) {'error msg'})

safe_decom <- function(x){
  tryCatch(decom.adaptive(x, smooth = T, thres = 0.22, width = 3), error = function(e){NA})
}

geolx = failclip$geolocation[2490:2495]
dc = apply(failclip$return[2490:2495], 1, safe_decom)
dc = dc[which(!is.na(dc))]
dc[which(!is.na(dc))]
fails = which(!is.na(dc))
fails
dc[c(fails)]
dc[fails]
geolx = geolx[c(fails)]
geolx[fails]

wfpts = geotransform(decomp = decom$repars, decom$geolocation)
