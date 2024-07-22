# Process NEON waveform LiDAR
# Author: Marshall Worsham | worsham@berkeley.edu
# Created: 03-26-21
# Revised: 03-02-22


#############################
# Set up working environment
#############################

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

# Name data directory
datadir <- '/global/scratch/users/worsham/waveform_binary_chunks'

# Name directory where inventory plot shapefiles live
shapedir <- '/global/scratch/users/worsham/EastRiver/Plot_Shapefiles/Polygons/'

# Name output directory
outdir <- '/global/scratch/users/worsham/geolocated_returns_plots'

# Name logfile
logpath = '/global/scratch/users/worsham/logs/plots_pwf_log.txt'

# Name flightpaths as filenames
flightpaths <- list.files(datadir, full.names = T)

# Get plot/LiDAR intersections
intersectscsv <- '~/Output/EastRiver_Plot_LiDAR_Chunk_Intersections.csv'

intersects <- read.csv(intersectscsv)
names(intersects) <- str_replace(names(intersects), '\\.', '-')

##########################################
# Process waveforms for one flightpath
##########################################

# tic <- proc.time()
# test1 <- rwaveform::process_wf(flightpaths[648], outdir)
# toc <- proc.time()
# print(toc-tic)

##########################################
# process waveforms at all plot locations
##########################################

aop.plots <- c(
  'CC-CVN1',
  'CC-CVN2',
  'CC-CVS1',
  'CC-EMN1',
  'CC-UC1',
  'CC-UC2',
  'ER-APL1',
  'ER-APU1',
  'ER-BME1',
  'ER-BME2',
  'ER-GT1',
  'SG-NES1',
  'SG-NES2',
  'SG-NES3',
  'SG-SWR1',
  'SR-PVG1',
  'WG-WGM1'
)

processatplots <- function(plt, itx, datadir, shapedir){

  # Get flighpaths intersecting plot
  itx_true = itx[itx[plt]==T,1]

  # Find which have completed in outdir
  did = lapply(strsplit(list.files(outdir), '_'), '[', c(2:6))
  did = unlist(lapply(did, paste, collapse ='_'))
  incomplete = itx_true[which(!intersects[intersects[plt]==T,1] %in% did)]
  aoi_flps = file.path(datadir, incomplete)

  # Process and write csv
  lapply(aoi_flps, process.wf.clip, plt, datadir, shapedir, buff=2, logpath, outdir)
}

for(i in aop.plots){
  processatplots(i, intersects, datadir, shapedir)
}

dc <- process.wf.clip(file.path(datadir,'2018_CRBU_1_2018061914_FL011-050'), 'ER-BME2', datadir, shapedir, buff=2, logpath, outdir)
