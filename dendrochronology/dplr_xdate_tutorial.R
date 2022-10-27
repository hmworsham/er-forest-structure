data(co021)
dat <- co021
dat
dat.sum <- summary(dat)
mean(dat.sum$year)
mean(interseries.cor(dat)[,1])
plot(dat, plot.type='spag')

rwl.ok <- corr.rwl.seg(dat, seg.length=60, pcrit=0.01, method='pearson')

set.seed(4576)
i <- sample(x=nrow(dat), size=1)
j <- sample(x=ncol(dat), size=1)
tmp <- dat[,j]
tmp <- c(NA, tmp[-i])
dat[,j] <- tmp

rwl.60 <- corr.rwl.seg(dat, seg.length=60, pcrit=0.01)
eg.60 <- corr.series.seg(rwl=dat, series="643143", seg.length=60)

win <- 1690:1780
dat.yrs <- time(dat)
dat.trunc <- dat[dat.yrs %in% win, ]
ccf.30 <- ccf.series.rwl(rwl=dat.trunc, series="643143", seg.length=30, bin.floor=50)


win <- 1700:1750
dat.yrs <- time(dat)
dat.trunc <- dat[dat.yrs %in% win, ]
ccf.30 <- ccf.series.rwl(rwl=dat.trunc, series="643143", seg.length=20, bin.floor=0)

xskel.ccf.plot(rwl=dat, series="643143", win.start=1720, win.width=40)

colnames(co021)[j]
rownames(co021)[i]
