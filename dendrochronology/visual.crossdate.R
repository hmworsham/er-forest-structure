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
cd.data <- read_xlsx(file.path(cddir, 'Crossdating_Master.xlsx'),
                     col_types = c(rep('text', 3),
                                   'numeric',
                                   'numeric',
                                   'text', 
                                   'numeric',
                                   'logical',
                                   'text',
                                   rep('numeric', 100)))

# Specify site and species of interest
site = 'SNB'
spp = 'PIEN'

# Get all marker years for site and spp of interest
site.spp <- cd.data[
  (cd.data$ITRDB_SiteID==site) & 
    (cd.data$Species==spp) & 
    (cd.data$Series!='Master') &
    cd.data$MY_1>=1000,]

# Calculate frequency of occurrence of marker years
my.freq <- data.frame(table(unlist(as.list(site.spp[10:length(site.spp)]))))
yrs <- data.frame('Var1'=seq(min(as.numeric(as.character(my.freq$Var1))), 2019))
my.freq <- merge(yrs, my.freq, all.x=T)
names(my.freq) <- c('Year', 'Freq')
my.freq <- my.freq %>%
  mutate(Freq3 = case_when(Freq>=2 ~ Freq))  

ggplot(my.freq, aes(x=Year, y=Freq)) +
  geom_col(fill=mrmoose::icolors('mario')[2], color='grey90', width=1) + 
  geom_text(aes(y=Freq3, label=Year), nudge_y=0.1, colour="black", size=2.5)
