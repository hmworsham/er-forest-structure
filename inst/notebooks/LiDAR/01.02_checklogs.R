# Check logs from waveform processing
# Author: Marshall Worsham | worsham@berkeley.edu
# Created: 04-02-23
# Revised: 07-22-24

#############################
# Set up working environment
#############################

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

logdir <- '/global/scratch/users/worsham/logs'
log <- read.csv(file.path(logdir, 'wf_processing_log.csv'))

colnames(log)
log %>%
  group_by(flightpath) %>%
  count()
