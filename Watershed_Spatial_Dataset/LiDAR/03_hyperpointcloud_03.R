library(lidR)
library(data.table)
library(devtools)
library(plotly)
library(parallel)
library(stringr)
load_all('~/Repos/rwaveform')

# Name directories
datadir <- '/global/scratch/users/worsham/geolocated_returns'
wfdir <- '/global/scratch/users/worsham/waveform_binary_chunks'
outdir <- '/global/scratch/users/worsham/hyperpointcloud'

returns <- list.files(datadir, full.names=T)
wfs <- list.files(wfdir, full.names=T)

makehpc <- function(repath, geopath){
  
  filenam = tail(unlist(strsplit(geopath, '/')), 1)
  
  pts = read.csv(repath)
  geo = ingest(geopath)$geol
  colnames(geo)[c(1:9,16)]<- c("index", "x","y","z","dx","dy","dz","or","fr", "outpeak")
  geo <- geo[which(seq(1,nrow(geo)) %in% unique(pts$index))]
  
  ge <- lapply(
    split(pts, pts$index), 
    build.gauss, 
    tbins=500#,
    #mc.preschedule = T,
    #mc.cores = getOption("mc.cores", detectCores()-2)
    )
  
  waveform = data.table(do.call('rbind', ge))
  
  hypc = hpc(waveform, geo, thres=1)
  hypc = data.table(hypc)
  rb = setorder(rbind.fill(pts, hypc), index)
  rownames(rb) = NULL

  outname = paste0(filenam, '_hpc.csv')
  write.csv(rb, file.path(outdir, outname), row.names = F)
  
  print(filenam)
  #return(rb)

}

r.did = str_replace(list.files(outdir), '_hpc.csv', '_returnpoints.csv')
r.did = file.path(datadir, r.did)
returns <- returns[!returns %in% r.did]

g.did = str_replace(list.files(outdir), '_hpc.csv', '')
g.did = file.path(wfdir, g.did)
wfs <- wfs[!wfs %in% g.did]

mcmapply(
  makehpc, 
  returns[1401:1942], 
  wfs[1401:1942],
  mc.preschedule = T,
  mc.cores = getOption("mc.cores", detectCores()-2)
  )
