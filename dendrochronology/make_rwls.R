######################
# Load packages
######################
# Load libraries
library(readxl)
library(tidyverse)
library(data.table)
library(dplR)

# Source local functions
source(file.path('.', 'eastriver', 'dendrochronology', 'dendro.helpers.R'))

######################
# Set up environment
######################

# Define directory of ring-width data
dendrodir <- file.path('/Volumes', 'GoogleDrive', 'My Drive', 'Research', 'RMBL', 'RMBL-East River Watershed Forest Data', 'Data', 'Dendrochronology')
rwdir <- file.path(dendrodir, 'Ring_Widths')
rwldir <- file.path(dendrodir, 'rwl')

######################
# Ingest data 
######################

# Ingest master dendro index
di.path <- file.path(dendrodir, 'Dendro_Core_Index.xlsx')
dendroindex <- read_excel(
  di.path, 
  sheet='Cores', 
  col_types=c(
    'text', 
    'text', 
    'text', 
    'numeric', 
    'numeric', 
    'text', 
    'numeric', 
    'numeric', 
    rep('guess', 18)))

dendroindex <- dendroindex[which(rowSums(is.na(dendroindex))<20),]

# Ingest master site name index
siteindex <- read_excel(di.path, sheet='Sites', col_types=c('numeric', 'text', 'text', 'text', 'numeric', rep('guess', 9)))

# # Create name key from site index
# namekey <- unique(data.frame('plotid'=siteindex$Plot_ID, 'siteid'=siteindex$ITRDB_SiteID))
# namekey <- namekey[order(namekey$plotid),]
# rownames(namekey) <- NULL

# Ingest ring-width data from csv files
rwfiles <- list.files(rwdir, full.names = T, pattern='[*[:digit:]].csv$')
ringwidths <- lapply(rwfiles, read.csv, header=T)
names(ringwidths) <- lapply(strsplit(rwfiles, '/|_'), '[', 12)

# Check that dimensions of ringwidth arrays are correct
all(sapply(ringwidths, function(x) dim(x)[2]==6))

######################################
# Convert to rwl and write to tucson
######################################

# List sites and species to process
sites <-  c('CRB', 'GTA', 'CRA', 'APL', 'BMA')
spps <- c('ABLA', 'PIEN')
names(sites) <- sites
names(spps) <- spps

# Restructure ringwidth dataframes
rw <- restructure.rw(ringwidths, dendroindex)

# Split dataframes by site and species
sss = outer(sites, spps, split.site.spp)

# Create rwl and write out to tucson format
rwl <- generate.rwl(sites, spps, sss, dendroindex, outdir=rwldir)

# Check it worked
ts <- read.tucson(file.path(rwldir, 'BMA_PIEN_dated.rwl'))
spag.plot(ts, zfac=0.5)
