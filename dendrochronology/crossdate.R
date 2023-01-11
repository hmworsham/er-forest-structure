library(tidyverse)
library(dplR)
library(ggplot2)

# Source local functions
source(file.path('.', 'eastriver', 'dendrochronology', 'dendro.helpers.R'))

######################
# Set up environment
######################

# Define directory of ring-width data
dendrodir <- file.path('/Volumes', 'GoogleDrive', 'My Drive', 'Research', 'RMBL', 'RMBL-East River Watershed Forest Data', 'Data', 'Dendrochronology')
rwdir <- file.path(dendrodir, 'Ring_Widths')
rwldir <- file.path(dendrodir, 'rwl')

################################################
# Plotting, summary stats, etc.
################################################

# Read in one series
series <- read.tucson(file.path(rwldir, 'SNB_PIEN_dated.rwl'))
#series <- series[!names(series) %in% c('CRB5503A')]

# Spaghetti plot
plot(series, plot.type='spag', zfac=0.5)

# RWL report
re <- rwl.report(series)
re

# Generate summary statistics
series.sum <- summary(series)
series.sum
mean(series.sum$year)
mean(series.sum$stdev)
mean(series.sum$median)
mean(series.sum$ar1)
mean(interseries.cor(series, method='spearman')[,1])

# Get RWL correlations
series.cor <- corr.rwl.seg(series, seg.length=50, pcrit=0.01, method='spearman', bin.floor=10)

################################################
# Evaluate a problematic core
################################################
target <- 'BMA1268A'
  
# View single-core correlations over time series
cor.50 <- corr.series.seg(rwl=series, series=target, seg.length=30, bin.floor=10)
xskel.ccf.plot(rwl=series, series=target, win.start=1930, win.width=60, prewhiten=T)

# Check problematic segment using n-year window
win <- 1920:1990
series.yrs <- time(series)
series.trunc <- series[series.yrs %in% win,]
ccf.30 <- ccf.series.rwl(rwl=series.trunc, series=target, prewhiten=T, seg.length=30, bin.floor=0)

##########################################################
# Iteratively drop cores to find optimal correlation score
###########################################################
iter <- seq(length(series))
combos <- lapply(iter, function(x) combn(iter, x, simplify=T))
out <- matrix(0, nrow=length(iter), ncol=3)
for(i in iter[-1]){
  sl <- dim(combos[[i]])[2]
  print(paste('i:', i))
  subout <- matrix(0, nrow=sl, ncol=2)
  for(k in seq(sl)){
    scomb <- series[combos[[i]][,k]]
    isc <- mean(interseries.cor(scomb)[,1])
    subout[k,1] <- k
    subout[k,2] <- isc
  }
  maxcorr <- max(subout[,2])
  idxmaxcorr <- which.max(subout[,2])
  out[i,1] <- i
  out[i,2] <- maxcorr
  out[i,3] <- idxmaxcorr
}

out

names(series[combos[[6]][,4842]])
names(series[combos[[5]][,430]])
series[combos[[4]][,264]]
series5 <- series[combos[[5]][,430]]

################################################
# Crossdate 'floaters' (relative/undated series)
################################################

series.u <- read.tucson(file.path(rwldir, 'CRB_ABLA_undated.rwl'))

u1 <- series.u[3]

xdate1 <- xdate.floater(
  rwl=series, 
  series=u1, 
  series.name='CRB5482A', 
  min.overlap=18, 
  method='pearson', 
  make.plot=T)

head(xdate1[order(xdate1$r, decreasing=T),], 20)

xdate2 <- xdate.floater(
  rwl=series, 
  series=u2, 
  series.name='CRB6576B', 
  min.overlap=50, 
  method='pearson', 
  make.plot=T)

head(xdate2[order(xdate2$r, decreasing=T),], 20)

################################################
# Detrend
################################################

# Detrend series
dt.0004A <- detrend.series(series$GTA0004A, method='ModNegExp')

# Plot detrended series with mean line
plot(dt.0004A, type='l')
abline(h=mean(dt.0004A, na.rm=T))

# Detrend all series in site
dt.series <- dplR::detrend(series, method='ModNegExp')

# Make skeleton plot
dt.chron <- chron(dt.series)
skel.plot(dt.chron$xxxstd, yr.vec=1542:2019)

# Make skeleton plot for one series
skel.plot(series$CRB5483A, yr.vec=as.numeric(rownames(series)))
skel.plot(series$CRB5483B, yr.vec=as.numeric(rownames(series)))

################################################
# Make master chronology
################################################

# Make master chronology for site
crn <- chron(dt.series)

###############################################
# Make RWL reports for all sites and species
###############################################

# Create empty lists
sitename <- list()
spcs <- list()
nseries <- list()
nmeas <- list()
avlen <- list()
seriesrange <- list()
seriesspan <- list()
r.mean <- list()
r.sd <- list()
ar.mean <- list()
ar.sd <- list()

# Populate lists of variables by running rwl reports looping through all series
runfiles <- list.files(rwldir)

for(i in seq_along(runfiles)){
  srs <- read.tucson(file.path(rwldir, runfiles[i]))
  rpt <- rwl.report(srs)
  nm <- unlist(strsplit(runfiles[i], '.rwl',))
  sitename[i] <- nm
  spcs[i] <- unlist(strsplit(nm, '_'))[2]
  nseries[i] <- rpt$nseries
  nmeas[i] <- rpt$n
  avlen[i] <- rpt$segbar
  seriesrange[i] <- rpt$yr1-rpt$yr0
  seriesspan[i] <- paste(rpt$yr0, '-', rpt$yr1)
  r.mean[i] <- rpt$interrbar
  r.sd[i] <- rpt$interrbar.sd
  ar.mean[i] <- rpt$ar1bar
  ar.sd[i] <- rpt$ar1bar.sd
}

# Make it into a dataframe
xx <- cbind(
  'sitename' = sitename,
  'species' = spcs,
  'nseries' = nseries,
  'n' = nmeas,
  'avg.length' = avlen,
  'range' = seriesrange,
  'span' = seriesspan,
  'r.mean' = r.mean,
  'r.sd' = r.sd,
  'ar.mean' = ar.mean,
  'ar.sd' = ar.sd
)

View(xx)
write.csv(xx, '~/Desktop/dendro_correlation_scores_pre-correction.csv')

#####################################
# Paired series correlations
#####################################

# Raw series
series.a <- series$CRB5483A[359:457]
series.b <- series$CRB5483B[359:457]

# Detrended series
dt.a <- detrend.series(series.a, method='ModNegExp')
dt.b <- detrend.series(series.b, method='ModNegExp')

# Scatterplot
plot(dt.a, dt.b)
abline(0,1)

# Linear model (slope, R^2)
summary(lm(series.a ~ series.b))

# Pearson correlation coefficient
cor(series.a, series.b)
