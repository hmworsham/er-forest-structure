logdir <- '/global/scratch/users/worsham/logs'

# All flightpaths log
log <- read.delim(file.path(logdir, 'processwf_log.txt'), sep='\t', header=F)
names(log) <- c('datetime', 'thread', 'type', 'src_pkg', 'src_fun', 'message')

log$flightpath <- as.character(lapply(strsplit(log$message, ' : '), '[', 1))
log$result <- as.character(lapply(strsplit(log$message, ' : '), '[', 2))
log <- data.frame(log)
log <- log[complete.cases(log), ]
log <- na.omit(log)

# Processed at plots
plog <- read.delim(file.path(logdir, 'processwf_plots_log.txt'), sep='\t', header=F)
names(plog) <- c('datetime', 'thread', 'type', 'src_pkg', 'src_fun', 'message')

plog$flightpath <- as.character(lapply(strsplit(plog$message, ' : '), '[', 1))
plog$result <- as.character(lapply(strsplit(plog$message, ' : '), '[', 2))
plog <- data.frame(plog)
plog <- plog[complete.cases(plog), ]
plog <- na.omit(plog)

View(plog)
