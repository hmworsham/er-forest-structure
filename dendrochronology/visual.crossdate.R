# Load libraries
#library(readxl)
library(tidyverse)
library(ggplot2)
library(data.table)
library(dplR)


dendrodir <- '/Volumes/GoogleDrive/My Drive/Research/RMBL/RMBL-East River Watershed Forest Data/Data/Dendrochronology'
rwdir <- file.path(dendrodir, 'Skeleton_Data')

cd.data <- read.csv(file.path(rwdir, 'Crossdating_Master.csv'))

erapl1.abla <- cd.data[(cd.data$Site=='ER-APL1') & (cd.data$Species=='ABLA') & (cd.data$Series!='Master'),]

abla.freq <- data.frame(table(unlist(as.list(erapl1.abla[5:29]))))
abla.freq <- abla.freq[order(abla.freq$Freq, decreasing = T),]
View(abla.freq)
