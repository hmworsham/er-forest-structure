library(data.table)

logdir <- '/global/scratch/users/worsham/logs'

# Read in all flightpaths log
log <- read.delim(file.path(logdir, 'sbatch_pwf_log.txt'), sep='\t', header=F)
names(log) <- c('datetime', 'thread', 'type', 'src_pkg', 'src_fun', 'message')
log <- data.frame(log)

# Split the message based on ':' delimeter
log$flightpath <- as.character(lapply(strsplit(log$message, ' : '), '[', 1))
log$vector <- lapply(strsplit(log$message, ' : '), '[', 2)
log$result <- as.character(lapply(strsplit(log$message, ' : '), '[', 3))

# Clean up results and vectors
log <- log[!log$message %like% 'NA : Cleaning flightpath threw an error.', ]
log <- log[!log$message %like% 'all scheduled cores encountered errors in user code', ]
log <- log[!log$message %like% 'NA : Ingest failed. A data or header file may be missing.', ]
log$result[is.na(log$result)] <- log$vector[is.na(log$result)]
log$vector <- as.numeric(log$vector)
log <- log[order(log$flightpath),]
log <- log[-6]

fwrite(log, '/global/scratch/users/worsham/logs/wf_processing_log.csv')

# Processed at plots log
plog <- read.delim(file.path(logdir, 'processwf_plots_log.txt'), sep='\t', header=F)
names(plog) <- c('datetime', 'thread', 'type', 'src_pkg', 'src_fun', 'message')

plog$flightpath <- as.character(lapply(strsplit(plog$message, ' : '), '[', 1))
plog$result <- as.character(lapply(strsplit(plog$message, ' : '), '[', 2))
plog <- data.frame(plog)
plog <- plog[complete.cases(plog), ]
plog <- na.omit(plog)
