library(data.table)
library(dplyr)

logdir <- '/global/scratch/users/worsham/logs'
log <- read.csv(file.path(logdir, 'wf_processing_log.csv'))

colnames(log)
log %>%
  group_by(flightpath) %>%
  count()
