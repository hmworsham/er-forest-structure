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
spp = 'ABLA'

# Get all marker years for site and spp of interest
site.spp <- cd.data[
  (cd.data$ITRDB_SiteID==site) &
    (cd.data$Species==spp) &
    (cd.data$Series!='Master') &
    (cd.data$MY_1>=1000),]
site.spp <- site.spp[!rowSums(is.na(site.spp))==ncol(site.spp),]

site.spp.t <- t(site.spp[10:length(site.spp)])
minyr <- apply(site.spp.t, 2, min, na.rm=T, simplify=T)
minyr <- sapply(minyr/10, floor)*10
decs <- sapply(minyr, seq, 2010, 10, simplify=T)
decs <- sapply(decs, rev)
decs <- sapply(decs, function(x) c(x, rep(NA, max(lengths(decs)-length(x)))))
dec.freq <- data.frame(table(decs))
names(dec.freq) <- c('Dec', 'Ncores')

# Calculate frequency of occurrence of marker years
my.freq <- data.frame(table(unlist(as.list(site.spp[10:length(site.spp)]))))
yrs <- data.frame('Var1'=seq(min(as.numeric(as.character(my.freq$Var1))), 2019))
my.freq <- merge(yrs, my.freq, all.x=T)
names(my.freq) <- c('Year', 'Freq')
my.freq$Dec <- floor(my.freq$Year/10)*10
my.freq <- merge(my.freq, dec.freq, by='Dec', all.x=T)
my.freq <- my.freq %>%
  mutate(FreqNorm = Freq/Ncores,
         FreqThresh = case_when(Freq>=3 ~ Freq),
         FreqNormThresh = case_when(FreqNorm>0.5 ~ FreqNorm))

# Plot gross frequency
ggplot(my.freq, aes(x=Year, y=Freq)) +
  geom_area(aes(x=Year, y=Ncores), alpha=.2) +
  geom_col(fill=nifty::icolors('mario')[2], width=1) +
  geom_text(aes(y=FreqThresh, label=Year), nudge_y=0.1, colour="black", size=2.5) +
  scale_y_continuous(sec.axis = sec_axis(~., name='N Cores'))

# Plot frequency normalized to number of cores that include a marker year's decade
ggplot(my.freq, aes(x=Year, y=FreqNorm)) +
  geom_area(aes(x=Year, y=Ncores/10), alpha=.2) +
  geom_col(fill=nifty::icolors('mario')[2], width=1) +
  geom_text(aes(y=FreqNormThresh, label=Year), nudge_y=0.01, colour="black", size=2.5) +
  scale_y_continuous(sec.axis=sec_axis(~.*10,
                                       name='N Cores',
                                       breaks=seq(1,max(my.freq$Ncores),1)))

