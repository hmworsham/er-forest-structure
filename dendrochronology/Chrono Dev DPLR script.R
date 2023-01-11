# Site chronology development script.
# "MySite" is dummy name for site ID on the rwl file.
# B. Brice, USGS, Nov 2022

rm(list=ls())
library(dplR)
library(utils)

MySite <-read.rwl('MySite.rwl')
head(rownames(MySite))
class(MySite)
plot(MySite, plot.type="spag")
dev.off()

### Use iDetrend first to determine most appropriate detrending option, then apply detrending option for entire site.
# This you will need to interact with R for each series after viewing plots/options.
MySite.rwi<-i.detrend(MySite, y.name = names(MySite), nyrs = NULL, f = 0.5,
                   pos.slope = FALSE)
dev.off()

### If you want to detrend a single series then use the code below.
singleseries<-MySite[, "MySite01A"]
names(singleseries)<-rownames(MySite)
singleseries.rwi<-detrend.singleseries(y = singleseries, y.name = "MySite01A", verbose = TRUE)

### Site-level detrend. Detrend method options are "Spline", "ModNegExp","Mean", "Ar", "Friedman", "ModHugershoff"
MySite.rwi <- detrend(rwl = MySite, method = "ModNegExp")
rwl.stats(MySite)

# The function read.ids is used to identify which trees have multiple cores.
MySite.ids <- read.ids(MySite, stc = c(3,2,3))
stats <- rwi.stats(MySite.rwi, MySite.ids, prewhiten=TRUE)

# Interseries correlation gives average interseries correlation (different than rbar)
MySite.rho <- interseries.cor(MySite.rwi, prewhiten=TRUE, method="spearman")
mean(MySite.rho [,1])

### Building a mean value chronology (standard chronology). After detrending, build a chronology by averaging across 
# the years of the rwi object. In dplR the function for doing this is chron which by default uses Tukey’s biweight robust 
# mean (an average that is unaffected by outliers). This object has the same number of rows as the rwi object that was 
# used as the input and two columns. The first gives the chronology and the second the sample depth (the number of 
# series available in that year). ie: dim(ca533.rwi)

## Standard only chrono:
MySite.crn <- chron(MySite.rwi, prefix="XXX")

## Both Standard and Residual chronologies in one file
MySite.crnr <- chron(MySite.rwi, prefix="XXX", prewhiten=TRUE)

## If preferred, remove the Standard chronology for writing .crn file ('crn' must have two columns)
MySiter.crn <- MySite.crnr[,-1]

## Plot the chronologies and sample depth
plot(MySite.crn, add.spline=TRUE, nyrs=20, ylab = 'Index')
plot(MySite.crnr, add.spline=TRUE, nyrs=20, ylab = 'Index')
## Write .crn file.
write.csv(MySite.crnr, file="MySite Chronologies.csv")

### After Building a mean value chronology, determine Expressed Population Signal (EPS) cutoff (typically 
# 0.85, but should be selected by you based on site characteristics and research objectives) then plot.
def.par <- par(no.readonly=TRUE)
# Arbitrary EPS cutoff for demonstration 
eps.cut <- 0.85 

# Plot the chronology showing a potential cutoff year based on EPS. Running stats on the rwi with a window.
# Adjust plot parameters as needed.
foo <- rwi.stats.running(MySite.rwi, MySite.ids, window.length = 80)
yrs <- as.numeric(rownames(MySite.crnr))
bar <- data.frame(yrs = c(min(yrs), foo$mid.year, max(yrs)), eps = c(NA, foo$eps, NA))
par(mar = c(2, 2, 2, 2), mgp = c(1.1, 0.1, 0), tcl = 0.25, mfcol = c(1, 1), xaxs='i')
cutoff <- max(bar$yrs[bar$eps < eps.cut], na.rm = TRUE)
xx <- c(1539, 1539, cutoff, cutoff)
yy <- c(-1, 3, 3, -1)

# Add EPS
plot(bar$yrs, bar$eps, type = "b", xlab = "", ylab = "", axes = FALSE, pch = 20, col = "blue")
axis(4, at = pretty(foo$eps))
mtext("EPS", side = 4, line = 1.1)
box()
polygon(xx, yy, col = "grey80")
abline(h = 1, lwd = 1.5)
par(new = TRUE)

## First plot is the std chronology after the cutoff only - chronology is rebuilt using just years after cutoff.
# Repeat for residual chronology if desired.
yr.mask <- yrs > cutoff
yrs2 <- yrs[yr.mask]
MySite.crn2 <- chron(MySite.rwi[yr.mask, ])
plot(yrs2, MySite.crn2[, 1], type = "n", xlab = "Year", ylab = "RWI", axes=FALSE)
abline(h = 1, lwd = 1.5)
lines(yrs2, MySite.crn2[, 1], col = "grey50")
lines(yrs2, ffcsaps(MySite.crn2[, 1], nyrs = 32), col = "red", lwd = 2)
axis(1); axis(2);
box()
par(def.par)

dev.off()