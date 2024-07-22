# Discretize waveform data to hyperpointcloud
# Author: Marshall Worsham | worsham@berkeley.edu
# Created: 02-24-22
# Revised: 07-22-24

#############################
# Set up working environment
#############################

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

# Name directories
datadir <- '/global/scratch/users/worsham/geolocated_returns'
wfdir <- '/global/scratch/users/worsham/waveform_binary_chunks'
outdir <- '/global/scratch/users/worsham/hyperpointcloud'

#############################
# Data ingest
#############################

# List files to process
returns <- list.files(datadir, full.names=T)
wfs <- list.files(wfdir, full.names=T)

#############################
# Processing
#############################

# Processing function
makehpc <- function(repath, geopath){

  filenam = tail(unlist(strsplit(geopath, '/')), 1)

  pts = read.csv(repath)
  geo = ingest(geopath)$geol
  colnames(geo)[c(1:9,16)]<- c("index", "x","y","z","dx","dy","dz","or","fr", "outpeak")
  geo <- geo[which(seq(1,nrow(geo)) %in% unique(pts$index))]

  ge <- lapply(
    split(pts, pts$index),
    build.gauss,
    tbins=500)

  waveform = data.table(do.call('rbind', ge))

  hypc = hpc(waveform, geo, thres=1)
  hypc = data.table(hypc)
  rb = setorder(rbind.fill(pts, hypc), index)
  rownames(rb) = NULL

  outname = paste0(filenam, '_hpc.csv')
  write.csv(rb, file.path(outdir, outname), row.names = F)

  print(filenam)
  return(rb)

}

# Run function
mcmapply(
  makehpc,
  returns,
  wfs,
  mc.preschedule = T,
  mc.cores = getOption("mc.cores", detectCores()-2)
  )
