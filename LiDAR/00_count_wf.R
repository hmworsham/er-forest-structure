library(caTools)

wfdir <- '/global/scratch/users/worsham/waveform_binary'
wffiles <- list.files(wfdir)

countem <- function(f){
  x <- read.delim(file.path(wfdir, f, paste0(f, '_waveform_return_pulse_array_img.hdr')))
  n <- as.numeric(unlist(strsplit(x$ENVI[4], ' = '))[2])
}


ns <- lapply(wffiles, countem)
ns <- unlist(ns)
sum(ns)