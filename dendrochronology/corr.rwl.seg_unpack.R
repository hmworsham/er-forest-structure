library(dplR)

# Define directory of ring-width data
dendrodir <- file.path('/Volumes', 'GoogleDrive', 'My Drive', 'Research', 'RMBL', 'RMBL-East River Watershed Forest Data', 'Data', 'Dendrochronology')
rwdir <- file.path(dendrodir, 'Ring_Widths')
rwldir <- file.path(dendrodir, 'rwl')

# Ingest
rwl <- read.tucson(file.path(rwldir, 'GTA_PIEN_dated.rwl'))

# Define parameters
yrs <- time(rwl)
min.yr <- min(yrs)
max.yr <- max(yrs)
cnames <- names(rwl)
nseries <- length(rwl)
seg.length <- 30
pcrit = 0.01
bin.floor <- NULL
biweight <- T
floor.plus1 <- F
master <- NULL
n <- NULL
label.cex <- 1
method <- method2 <- 'spearman'

# Set lag
seg.lag <- seg.length / 2

# Define n years in chronology
nyrs <- length(yrs)

# Set bins
if (is.null(bin.floor) || bin.floor == 0) {
  min.bin <- min.yr
} else if (floor.plus1) {
  min.bin <- ceiling((min.yr - 1) / bin.floor) * bin.floor + 1
} else {
  min.bin <- ceiling(min.yr / bin.floor) * bin.floor
}
max.bin <- max.yr - seg.length + 1

bins <- seq(from=min.bin, to=max.bin, by=seg.lag)
bins <- cbind(bins, bins + (seg.length - 1), deparse.level=0)
nbins <- nrow(bins)
bin.names <- paste0(bins[, 1], "-", bins[, 2])

##**##
bins
bin.names

## structures for results
res.cor <- matrix(NA, nseries, nbins)
rownames(res.cor) <- cnames
colnames(res.cor) <- bin.names

res.pval <- matrix(NA, nseries, nbins)
rownames(res.pval) <- cnames
colnames(res.pval) <- bin.names

overall.cor <- matrix(NA, nseries, 2)
rownames(overall.cor) <- cnames
colnames(overall.cor) <- c("rho", "p-val")

## normalize all series
norm.one <- normalize1(rwl, n, prewhiten=F)

## rwi for segments altered by normalizing
rwi <- norm.one$master # is a matrix
idx.good <- norm.one$idx.good

## loop through series
seq.series <- seq_len(nseries)

##**##
seq.series

for (i in seq.series) {
  if (is.null(master)) {
    idx.noti <- rep(TRUE, nseries)
    idx.noti[i] <- FALSE
    master.norm <- rwi[, idx.good & idx.noti, drop=FALSE]
    
    ## compute master series by normal mean or robust mean
    if (!biweight) {
      master2 <- apply(master.norm, 1, exact.mean)
    } else {
      master2 <- apply(master.norm, 1, tbrm, C=9)
    }
  }
  
  series <- rwi[, i]
  ## loop through bins
  for (j in seq_len(nbins)) {
    mask <- yrs %in% seq(from=bins[j, 1], to=bins[j, 2])
    ## cor is NA if there is not complete overlap
    if (!any(mask) ||
        any(is.na(series[mask])) ||
        any(is.na(master2[mask]))) {
      bin.cor <- NA
      bin.pval <- NA
    } else {
      tmp <- cor.test(series[mask], master2[mask],
                      method = method2, alternative = "greater")
      bin.cor <- tmp$estimate
      bin.pval <- tmp$p.val
    }
    res.cor[i, j] <- bin.cor
    res.pval[i, j] <- bin.pval
  }
  ## overall correlation
  tmp <- cor.test(series, master2,
                  method = method2, alternative = "greater")
  overall.cor[i, 1] <- tmp$estimate
  overall.cor[i, 2] <- tmp$p.val
}

##**##
plot(master2[132:162], type='l')
lines(series[132:162], col='red')
series.lm <- lm(master2[132:162] ~ series[132:162])
length(master2)

##**##
plot(master2[132:162], series[132:162])
abline(coef(series.lm), col='red')
text()

##**##
View(res.cor)
View(overall.cor)

## avg seg correlation
segavg.cor <- colMeans(res.cor, na.rm=TRUE)

## make a list of problem segments
seg.flags <- rep(NA, nseries)
names(seg.flags) <- cnames
flag.logical <- res.pval >= pcrit
flag.logical[is.na(flag.logical)] <- FALSE
for (i in seq_along(seg.flags)) {
  seg.flags[i] <- paste(bin.names[flag.logical[i, ]], collapse = ", ")
}
seg.flags <- seg.flags[seg.flags != ""]

res <- list(spearman.rho = res.cor, p.val = res.pval, overall = overall.cor,
            avg.seg.rho = segavg.cor, flags = seg.flags, bins = bins, 
            rwi = rwi, seg.lag = seg.lag, seg.length = seg.length, 
            pcrit = pcrit, label.cex = label.cex)
class(res) <- c("list","crs")
View(res)

## plot
plot.crs(res)

# Required functions
normalize1 <- function(rwl, n, prewhiten){
  rwl.mat <- as.matrix(rwl)
  ## Run hanning filter over the data if n isn't NULL
  ## divide by mean if n is null
  if(is.null(n)){
    master.stats <- colMeans(rwl.mat, na.rm=TRUE)
    master.mat <- sweep(rwl.mat, 2, master.stats, "/")
  } else {
    master.stats <- apply(rwl.mat, 2, hanning, n)
    master.mat <- rwl.mat / master.stats
  }
  ## Apply ar if prewhiten
  if(prewhiten){
    ## take note of, ignore later, any columns without at least
    ## four observations
    idx.good <- colSums(!is.na(master.mat)) > 3
    master.mat <- apply(master.mat, 2, ar.func)
  } else {
    idx.good <- rep(TRUE, ncol(master.mat))
  }
  list(master=master.mat, idx.good=idx.good)
}
