df <- read.delim('~/Downloads/LATEST - Breakpoint Corrected/data.txt', skip=111, row.names=NULL)

tmin <- read.delim('~/Downloads/LATEST - Breakpoint Corrected(1)/data.txt', skip=111, row.names=NULL)

tmax <- read.delim('/Volumes/GoogleDrive/My Drive/Research/RMBL/RMBL-East River Watershed Forest Data/Data/Climate/Berkeley_Earth/TMAX_Breakpoint_Corrected/data.txt', skip=111, row.names=NULL)

names(df) <- c('Station_ID', 'Series_Number', 'Date', 'Temperature_C', 'Uncertainty_C', 'Observations', 'Time_of_Observation')

names(tmin) <- c('Station_ID', 'Series_Number', 'Date', 'Temperature_C', 'Uncertainty_C', 'Observations', 'Time_of_Observation')

names(tmax) <- c('Station_ID', 'Series_Number', 'Date', 'Temperature_C', 'Uncertainty_C', 'Observations', 'Time_of_Observation')

cb <- df[df$Station_ID==927113,]
cb$Anomaly <- cb$Temperature_C-mean(cb$Temperature_C)

cbtmin <- tmin[tmin$Station_ID==927113,]
View(cbtmin)

cbtmax <- tmax[tmax$Station_ID==927113,]
View(cbtmax)



plot(cb$Temperature_C, type='l')
plot(cb$Anomaly, type='l')
lines(seq(1,nrow(cb)), rep(mean(cb$Temperature_C), nrow(cb)), col='red')
lines(seq(1,nrow(cb)), rep(0, nrow(cb)))
