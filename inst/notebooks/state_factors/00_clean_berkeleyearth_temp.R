# Clean Berkeley Earth Temperature Data 

####################################################
# Subsets, cleans and applies QA filters to 
# Berkeley Earth temperature dataset 
# for Crested Butte station (Station_ID=927113)
####################################################

# Load libraries
library(dplyr)
library(zoo)
library(lubridate)

# Set up environment
datadir <- file.path('/Volumes', 'GoogleDrive', 'My Drive', 'Research', 'RMBL', 'RMBL-East River Watershed Forest Data', 'Data')
bedir <- file.path(datadir, 'Climate', 'Berkeley_Earth')
outdir <- file.path(bedir, 'CrestedButte_927113_T_Breakpoint_Corrected')

# Read delimited data for T average, max, and min
tavg <- read.delim(
  file.path(bedir, 'TAVG_Breakpoint_Corrected', 'data.txt'), 
  skip=111, # Data start at line 112, before that is just unstructured human-readable metadata
  col.names=c('Station_ID', 'Series_Number', 'Date', 'Temperature_C', 'Uncertainty_C', 'Observations', 'Time_of_Observation'),
  row.names=NULL)

tmax <- read.delim(
  file.path(bedir, 'TMAX_Breakpoint_Corrected', 'data.txt'), 
  skip=111, # Data start at line 112, before that is just unstructured human-readable metadata
  col.names=c('Station_ID', 'Series_Number', 'Date', 'Temperature_C', 'Uncertainty_C', 'Observations', 'Time_of_Observation'),
  row.names=NULL)

tmin <- read.delim(
  file.path(bedir, 'TMIN_Breakpoint_Corrected', 'data.txt'), 
  skip=111, # Data start at line 112, before that is just unstructured human-readable metadata
  col.names=c('Station_ID', 'Series_Number', 'Date', 'Temperature_C', 'Uncertainty_C', 'Observations', 'Time_of_Observation'),
  row.names=NULL)

# Subset each dataset to CB station 927113
cb.tavg <- tavg[tavg$Station_ID==927113,]
cb.tmax <- tmax[tmax$Station_ID==927113,]
cb.tmin <- tmin[tmin$Station_ID==927113,]

# Full join avg, max, and min data
cb.temp <- merge(cb.tavg, cb.tmax, by='Date', all='T', suffixes=c('_AVG', '_MAX'))
cb.temp <- merge(cb.temp, cb.tmin, by='Date', all='T', suffixes=c('', '_MIN'))

# Remove duplicate fields, e.g. Station_ID and Series_Number, identical across the 3 sets
cb.temp  <- subset(cb.temp, select=-c(
  Station_ID, 
  Series_Number, 
  Station_ID_MAX, 
  Series_Number_MAX))

# Rename columns
names(cb.temp) <- c('Date',
                    'Station_ID', 
                    'Series_Number', 
                    'Temp_C_AVG', 
                    'Unc_C_AVG',
                    'Obs_AVG',
                    'ObsTime_AVG',
                    'Temp_C_MAX', 
                    'Unc_C_MAX',
                    'Obs_MAX',
                    'ObsTime_MAX',
                    'Temp_C_MIN', 
                    'Unc_C_MIN',
                    'Obs_MIN',
                    'ObsTime_MIN')

# Write to csv
write.csv(cb.temp, file.path(outdir, 'CrestedButte_927113_T_Breakpoint_Corrected.csv'))

# Plot to visualize

# Generate annual mean
t.avg.ann <- cb.temp %>%
  mutate(Year=year(as.Date(date_decimal(Date)))) %>%
  group_by(Year) %>%
  summarise(T_AVG_ANN=mean(Temp_C_AVG)) %>%
  filter(Year!=2022 & Year!=1906)

# Generate rolling mean
rmean <- rollmean(t.avg.ann$T_AVG_ANN, k=15)

plot(t.avg.ann$Year, t.avg.ann$T_AVG_ANN, type='l')
lines(1907:(1907+length(rmean)-1), rmean, col='salmon')

plot(cb.temp$Temp_C_AVG, type='l')
lines(seq(1,nrow(cb.tavg)), rep(mean(cb.tavg$Temperature_C), nrow(cb.tavg)), col='red')
lines(rmean, col='salmon')
