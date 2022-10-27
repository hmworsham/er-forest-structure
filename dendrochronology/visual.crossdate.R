# Load libraries
library(tidyverse)
library(ggplot2)
library(data.table)
library(dplR)
library(readxl)

# Set up workspace
dendrodir <- file.path('/Volumes', 'GoogleDrive', 'My Drive', 'Research', 'RMBL', 'RMBL-East River Watershed Forest Data', 'Data', 'Dendrochronology')
cddir <- file.path(dendrodir, 'Crossdating')

# Ingest crossdating spreadsheet
cd.data <- read_xlsx(file.path(cddir, 'Crossdating_Master.xlsx'))

# Specify site and species of interest
site = 'CRB'
spp = 'ABLA'

# Get all marker years for site and spp of interest
site.spp <- cd.data[
  (cd.data$ITRDB_SiteID==site) & 
    (cd.data$Species==spp) & 
    (cd.data$Series!='Master'),]

# Calculate frequency of occurrence of marker years
abla.freq <- data.frame(table(unlist(as.list(site.abla[10:length(site.abla)]))))
abla.freq <- abla.freq[order(abla.freq$Freq, decreasing = T),]
