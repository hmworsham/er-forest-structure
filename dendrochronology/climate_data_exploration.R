library(tidyverse)
library(reshape2)
library(dplR)
library(ggplot2)
library(ggsci)
library(wesanderson)
library(forecast)
library(waveslim)

# Source local functions
source(file.path('.', 'eastriver', 'dendrochronology', 'dendro.helpers.R'))

######################
# Set up environment
######################

# Define directory of ring-width data
datadir <- file.path('/Volumes', 'GoogleDrive', 'My Drive', 'Research', 'RMBL', 'RMBL-East River Watershed Forest Data', 'Data')
dendrodir <- file.path(datadir, 'Dendrochronology')
rwdir <- file.path(dendrodir, 'Ring_Widths')
rwldir <- file.path(dendrodir, 'rwl')
climdir <- file.path(datadir, 'Climate')
precipdir <- file.path(climdir, 'NOAA_CDO', 'COOP', 'Crested_Butte')
tempdir <- file.path(climdir, 'Berkeley_Earth', 'CrestedButte_927113_T_Breakpoint_Corrected')

###################################
# Generate ring-width time series
###################################

# Aggregate rwls from all sites and species
rwfiles <- list.files(rwldir, pattern='_dated', full.names=T)
rws <- lapply(rwfiles, read.tucson)
sites <- c('APL', 'GTA', 'CRA', 'CRB', 'BMA')
spp <- c('ABLA', 'PIEN')
ids <- paste(rep(sites, each = length(spp)), spp, sep = "_")

# Build master chronologies for each site-spp set
crn <- lapply(rws, chron)

# Detrend series
dt <- lapply(rws, dplR::detrend, method='Spline')

# Build master chronologies for each site-spp set
crn <- lapply(dt, chron)

# Assign year as distinct column
for(i in seq(length(crn))){
  #crn[[i]]$id = ids[i]
  crn[[i]]$YEAR = as.numeric(rownames(crn[[i]]))
}

# Make a wide df of years and rwi
crn.long <- do.call('rbind', crn) # Rowbind the list of chronologies
crn.df <- data.frame('YEAR'=seq(as.numeric(min(rownames(crn.long))), 2019)) # Initialize df with years

# Iteratively add site-spp rwis and sample depths by year
for(i in crn){
  crn.df = left_join(crn.df, i, by = 'YEAR')
}

# Rename columns so they're interpretable
names(crn.df) <- c('YEAR', unlist(lapply(ids, paste, c('STDDEV', 'SAMPLE_DEPTH'), sep='_')))

# Select year and rwi values only
crn.df <- select(crn.df, matches('YEAR|STDDEV'))

###############
# Climate
###############

# Ingest precip data
precipfiles <- list.files(precipdir, full.names=T)
precip <- read.csv(precipfiles[1])

# Create precip record
precip <- precip[,c('DATE', 'PRCP')]
precip$DATE <- as.POSIXct(precip$DATE)
precip <-  precip %>% 
  mutate(YEAR=as.numeric(format(precip$DATE, '%Y')),
         MONTH=as.numeric(format(precip$DATE, '%M'))) %>%
  group_by(YEAR) %>%
  summarise(ANN_P = sum(PRCP, na.rm=T)) %>%
  mutate(P_MEAN = mean(ANN_P),
         P_ANOM = ANN_P-P_MEAN, 
         P_ANOM_NEG = P_ANOM < 0, 
         P_ANOM_STD = (P_ANOM-mean(P_ANOM))/sd(P_ANOM),
         P_ANOM_CAT = case_when((P_ANOM_STD <= -1) ~ 'Extreme low',
                                (P_ANOM_STD <= -0.5 & P_ANOM_STD > -1) ~ 'Moderate low',
                                (P_ANOM_STD <= 0.5 & P_ANOM_STD > -0.5) ~ 'Mean conditions',
                                (P_ANOM_STD <= 1 & P_ANOM_STD > 0.5) ~ 'Moderate high',
                                (P_ANOM_STD >= 1) ~ 'Extreme high',
                                TRUE ~ 'NA'))

p.anom <- precip[c('YEAR', 'P_ANOM_STD', 'P_ANOM_CAT')]
p.anom.rep <- p.anom[rep(seq_len(nrow(p.anom)), length(crn)),]
p.anom.rep$SITE <- sort(rep(ids, nrow(precip)))

precip

# p.anom.neg <- precip[c('YEAR', 'P_ANOM')]
# p.anom.neg[p.anom.neg$P_ANOM >=0, 'P_ANOM'] <- 0
# p.anom.neg$P_ANOM_STD <- scale(p.anom.neg$P_ANOM)
# 
# p.an.rep <- p.anom.neg[rep(seq_len(nrow(p.anom.neg)), length(crn)), ]
# p.an.rep$SITE <- sort(rep(ids, 114))

###################################
# Merge precip and chronologies
###################################
# Merge precip and rwi
crn.df.long <- melt(crn.df, id=c('YEAR'))
crn.p <- merge(crn.df.long, p.anom, on='YEAR')

crn.p$SITE <- unlist(lapply(str_split(crn.p$variable, '_'), '[', 1))
crn.p$SP <- unlist(lapply(str_split(crn.p$variable, '_'), '[', 2))
head(crn.p, length(crn))

crn.p.si.means <- crn.p %>%
  group_by(SITE, YEAR) %>%
  mutate(RWI_SITE_MEAN=mean(value)) %>%
  ungroup()

crn.p.sp.means <- crn.p %>%
  group_by(SP, YEAR) %>%
  mutate(RWI_SP_MEAN=mean(value)) %>%
  #summarize(RWI_SP_MEAN=mean(value))%>%
  ungroup()

crn.p.sp.means <- crn.p.sp.means[crn.p.sp.means$YEAR<2019,]
crn.p.site.means <- crn.p.si.means[crn.p.si.means$YEAR<2019,]

###########################
# Cross correlation
###########################
crn.p.wide <- inner_join(crn.df, precip, on='YEAR')

crosscorr.AA <- ccf(crn.p.wide$P_ANOM, crn.p.wide$APL_ABLA_STDDEV)
crosscorr.AP <- ccf(crn.p.wide$P_ANOM, crn.p.wide$APL_PIEN_STDDEV)
crosscorr.GA <- ccf(crn.p.wide$P_ANOM, crn.p.wide$GTA_ABLA_STDDEV)
crosscorr.GP <- ccf(crn.p.wide$P_ANOM, crn.p.wide$GTA_PIEN_STDDEV)
crosscorr.GP
crn.p.wide$P_LAG3 <- lag(crn.p.wide$P_ANOM, 3)
crn.p.wide$P_LAG4 <- lag(crn.p.wide$P_ANOM, 4)
crn.p.wide$P_LAG5 <- lag(crn.p.wide$P_ANOM, 5)
crn.p.wide$P_LAG6 <- lag(crn.p.wide$P_ANOM, 6)

df <- crn.p.wide

pacf(crn.p.wide$P_ANOM, lag.max=25)
detrend <- residuals(lm(crn.p.wide$P_ANOM ~ c(1:length(crn.p.wide$P_ANOM))))

ggplot(df, aes(y=APL_ABLA_STDDEV)) +
  geom_point(aes(x=P_ANOM), color='grey10') +
  geom_point(aes(x=P_LAG3), color='blue') + 
  geom_point(aes(x=P_LAG4), color='green') + 
  geom_point(aes(x=P_LAG5), color='red') + 
  geom_point(aes(x=P_LAG6), color='pink') + 
  geom_smooth(method='lm', aes(P_ANOM, APL_ABLA_STDDEV)) + 
  geom_smooth(method='lm', aes(P_LAG3, APL_ABLA_STDDEV)) + 
  geom_smooth(method='lm', aes(P_LAG4, APL_ABLA_STDDEV)) + 
  geom_smooth(method='lm', aes(P_LAG5, APL_ABLA_STDDEV)) + 
  geom_smooth(method='lm', aes(P_LAG6, APL_ABLA_STDDEV))

lml.AA <- tslm(ts(crn.p.wide$APL_ABLA_STDDEV) ~ ts(crn.p.wide$P_LAG5))
lml.AP <- lm(crn.p.wide$APL_PIEN_STDDEV ~ crn.p.wide$P_LAG6)
lml.GA <- lm(crn.p.wide$GTA_ABLA_STDDEV ~ crn.p.wide$P_LAG3)
lml.GP <- lm(crn.p.wide$GTA_PIEN_STDDEV ~ crn.p.wide$P_LAG6)
summary(lml.AA)
summary(lml.AP)
summary(lml.GA)
summary(lml.GP)
summary(lml1)
residuals(lml1)

######################
# Plots
######################

# Precip anomaly and ringwidth
ggplot(crn.p, aes(x=YEAR)) + 
  geom_line(aes(y=value, color=variable)) +
  #facet_grid(vars(variable)) +
  geom_col(aes(y=P_ANOM_STD/10)) + 
  scale_y_continuous(
    name=bquote('Ring-width Index (unitless)'),
    sec.axis = sec_axis(~., name=bquote('Precipitation Anomaly' (mm ~d^-1))))

pal <- wes_palette("Darjeeling1", n=5, type = "discrete")

# Precip anomaly and RWI by species
ggplot(crn.p.sp.means, aes(x=YEAR)) + 
  #facet_wrap(vars(SP)) +
  geom_line(aes(y=RWI_SP_MEAN, color=SP)) +
  geom_col(aes(y=P_ANOM_STD/100)) + 
  #scale_color_manual(values=pal) +
  #scale_color_breer(palette='Oranges') + 
  scale_color_jco(labels= c('ABLA',
                            'PIEN')) +
  scale_y_continuous(
    name=bquote('Ring-width Index'),
    sec.axis = sec_axis(~.*100, name=bquote('Precipitation Anomaly' (mm ~y^-1)))) +
  labs(title = 'Ring Width Index by Spp. vs. Precipitation Anomaly, 1909-2018',
       color = 'Species') +
  xlab('Year') +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        plot.title = element_text(size=20))

# Precip anomaly and ring width by site
ggplot(crn.p.si.means, aes(x=YEAR)) + 
  #facet_wrap(vars(SP)) +
  geom_line(aes(y=RWI_SITE_MEAN, color=SITE)) +
  geom_col(aes(y=P_ANOM_STD/100)) + 
  #scale_color_manual(values=pal) +
  #scale_color_breer(palette='Oranges') + 
  scale_color_npg(labels= c('APL',
                            'GTA',
                            'CRA',
                            'CRB',
                            'BMA')) +
  scale_y_continuous(
    name=bquote('Ring-width Index'),
    sec.axis = sec_axis(~.*100, name=bquote('Precipitation Anomaly' (mm ~y^-1)))) +
  labs(title = 'Ring Width Index by Site vs. Precipitation Anomaly, 1909-2018',
       color = 'Site') +
  xlab('Year') +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        plot.title = element_text(size=20))

# Boxplot: ringwidth by site and precip categorical
crn.p.order <- crn.p %>%
  arrange(value) %>%
  mutate(P_CAT=factor(P_ANOM_CAT, levels=c('Extreme low', 
                                           'Moderate low', 
                                           'Mean conditions', 
                                           'Moderate high',
                                           'Extreme high')))

ggplot(crn.p.order, aes(x=value, y=SP, fill=P_CAT)) +
  geom_boxplot() + 
  scale_fill_brewer(palette='RdYlBu') +
  coord_flip() + 
  labs(title='Ring growth responds to precipitation extremes',
       x='Ring-width index',
       y='Species', 
       fill='Precipitation anomaly magnitude') +
  theme_minimal() + 
  theme(axis.title=element_text(size=16),
        axis.text=element_text(size=14),
        title=element_text(size=16, hjust=0.5),
        asp=1)

crn.p.lo <- crn.p.order[crn.p.order$P_CAT=='Extreme low' | crn.p.order$P_CAT=='Moderate low',]

ggplot(crn.p.lo, aes(x=value, y=SITE, fill=SITE)) +
  geom_boxplot() + 
  scale_fill_brewer(palette='RdYlBu') +
  coord_flip() + 
  labs(title='Ring growth responds to precipitation extremes',
       x='Species',
       y='Ring-width index', 
       fill='Precipitation anomaly magnitude') +
  theme_minimal() + 
  theme(axis.title=element_text(size=16),
        axis.text=element_text(size=14),
        title=element_text(size=16, hjust=0.5),
        asp=1)

#######################
# ANOVA
#########################

crn.p.aov <- aov(value ~ P_ANOM_CAT, data=crn.p)
summary(crn.p.aov)
TukeyHSD(crn.p.aov)
pairwise.t.test(crn.p$value, crn.p$P_ANOM_CAT,
                p.adjust.method = "BH",
                pool.sd=F)
library(car)
leveneTest(value ~ factor(P_ANOM_CAT), data=crn.p)

plot(crn.p.aov,1)

crn.p.sp.aov <- aov(value ~ SITE, data=crn.p.lo)
summary(crn.p.sp.aov)
TukeyHSD(crn.p.sp.aov)


#########################
# Time series smoothing
#########################

dat <- crn[[8]][1]
cron <- crn[[8]][c(1,2)]

# Plot with sample depth
plot(cron, add.spline=F, nyrs=19)

# Moving average
yrs <- time(dat)
dat <- dat$xxxstd

ma32 <- stats::filter(x=dat, filter=rep(x=1/32,times=32), sides=2)
ma64 <- stats::filter(x=dat, filter=rep(x=1/64,times=64), sides=2)
ma128 <- stats::filter(x=dat, filter=rep(x=1/128,times=128), sides=2)

par(mar=rep(2.5,4),mgp=c(1.2,0.25,0),tcl=0.5, xaxs="i",yaxs="i")
my.cols <- mrmoose::icolors('mario')
plot(yrs,dat,type="l",xlab="Year",ylab="RWI",col="grey")
abline(h=1)
lines(yrs,ma128, col = my.cols[1], lwd = 2)
lines(yrs,ma64, col = my.cols[2], lwd = 2)
lines(yrs,ma32, col = my.cols[3], lwd = 2)
axis(3); axis(4)
legend("topright", c("rwi", "128yrs", "64yrs", "32yrs"), 
       lwd = 2, col = c("grey", my.cols),bg = "white")
box()

# Hanning filter
han32 <- hanning(dat,n=32)
han64 <- hanning(dat,n=64)
han128 <- hanning(dat,n=128)

par(mar=rep(2.5,4),mgp=c(1.2,0.25,0),tcl=0.5, xaxs="i",yaxs="i")
my.cols <- mrmoose::icolors('summer21')
plot(yrs,dat,type="l",xlab="Year",ylab="RWI",col="grey")
abline(h=1)
lines(yrs,han128, col = my.cols[1], lwd = 2)
lines(yrs,han64, col = my.cols[2], lwd = 2)
lines(yrs,han32, col = my.cols[3], lwd = 2)
axis(3);axis(4)
legend("topright", c("rwi", "128yrs", "64yrs", "32yrs"), 
       lwd = 2, col = c("grey", my.cols),bg = "white")
box()

# Smoothing spline (Cook, cubic)
spl128 <- ffcsaps(dat,nyrs=128)
spl64 <- ffcsaps(dat,nyrs=64)
spl32 <- ffcsaps(dat,nyrs=32)

par(mar=rep(2.5,4),mgp=c(1.2,0.25,0),tcl=0.5, xaxs="i",yaxs="i")
my.cols <- mrmoose::icolors('f2')
plot(yrs,dat,type="n",xlab="Year",ylab="RWI",axes=FALSE)
grid(col="black",lwd=0.5)
abline(h=1)
lines(yrs,dat,col="grey",lwd=1)
lines(yrs,spl128,col=my.cols[1],lwd=2)
lines(yrs,spl64,col=my.cols[2],lwd=2)
lines(yrs,spl32,col=my.cols[3],lwd=2)
axis(1);axis(2);axis(3);axis(4)
legend("topright", c("rwi", "128yrs", "64yrs", "32yrs"), 
       lwd = 2, col = c("grey",my.cols),bg = "white")
box()

#######################################
# Characterizing temporal structure
#######################################

# Look at temporal structure
# 
par(mfcol=c(1, 3), mar=rep(3,4))
acf.cor <- acf(dat, type='cor', main='CRB PIEN autocor')
acf.cov <- acf(dat, type='cov', main='CRB PIEN autocov')
acf.par <- acf(dat, type='par', main='CRB partial autocor')

# Augmented Dickey-Fuller test of stationarity
library(tseries)
adf.test(dat, alternative='stationary') # Infer the time series is stationary
dat.dif <- diff(dat$xxxstd)
acf(dat.dif, type='cor')
adf.test(dat.dif)

# Find best fitting autoregressive model
dat.ar <- ar(crn.df$APL_ABLA_STDDEV[310:length(crn.df$APL_ABLA_STDDEV)], order.max = 10)
dat.ar
dat.ar$aic
plot(0:10,dat.ar$aic,type="b",xlab="AR Order",ylab="AIC",
     main="Difference in AIC between each model and the best-fitting model")

# Find best fitting ARIMA model
ari1 <- arima(dat,order=c(1,0,1))
ari2 <- arima(dat,order=c(2,0,1))
ari3 <- arima(dat,order=c(3,0,0))
ari4 <- arima(dat,order=c(4,0,0))
ari5 <- arima(dat,order=c(5,0,0))
ari6 <- arima(dat,order=c(6,0,0))
ari7 <- arima(dat, order=c(7,0,1))

ari1
ari6
ari7
ari.bic <- BIC(ari1,ari2,ari3,ari4,ari5, ari6, ari7)
plot(0:6, ari.bic$BIC,type="b",xlab="AR Order",ylab="BIC",
     main="Difference in BIC between each model and the best-fitting model")
ari.bic$BIC
checkresiduals(dat)
checkresiduals(ari6)
ari6$
plot(dat$xxxstd, type='l')
plot(fitted(ari6), col='red')

# Auto-ARIMA
dat.arima <- forecast::auto.arima(dat, ic="aic")
summary(dat.arima)
acf(residuals(dat.arima))

############################
# Frequency domain
############################

# REDFIT
redf.dat <- redfit(dat, nsim = 1000)
redf.dat

par(tcl = 0.5, mar = rep(2.2, 4), mgp = c(1.1, 0.1, 0),xaxs="i")
plot(redf.dat[['freq']], redf.dat[['gxxc']])
plot(redf.dat[["freq"]], redf.dat[["gxxc"]],
     ylim = range(redf.dat[["ci99"]], redf.dat[["gxxc"]]),
     type = "n", ylab = "Spectrum", xlab = "Frequency (cycles per year)",
     axes = FALSE)
grid()
lines(redf.dat[["freq"]], redf.dat[["gxxc"]], col = "black",lwd=1.5)
lines(redf.dat[["freq"]], smooth.spline(redf.dat[["ci99"]],spar = 0.8)$y, col = "#D95F02")
lines(redf.dat[["freq"]], smooth.spline(redf.dat[["ci95"]],spar = 0.8)$y, col = "#7570B3")
lines(redf.dat[["freq"]], smooth.spline(redf.dat[["ci90"]],spar = 0.8)$y, col = "#E7298A")
freqs <- pretty(redf.dat[["freq"]])
pers <- round(1 / freqs, 2)
axis(1, at = freqs, labels = TRUE)
axis(3, at = freqs, labels = pers)
mtext(text = "Period (year)", side = 3, line = 1.1)
axis(2); axis(4)
legend("topright", c("Fourier power spectrum", "CI99", "CI95", "CI90"), lwd = 2,
       col = c("black", "#D95F02", "#7570B3", "#E7298A"),
       bg = "white")
box()

########################
# Wavelet
########################
library(viridis)
out.wave <- morlet(y1 = dat, x1 = yrs, p2 = 8, dj = 0.25, siglvl = 0.99)
wavelet.plot(out.wave, useRaster=NA, reverse.y = TRUE, key.cols=viridis(10, option='D'))

pdat <- p.anom$P_ANOM
p.wave <- morlet(pdat, x1=p.anom$YEAR, p2=6, dj=0.25, siglvl=0.99)
wavelet.plot(p.wave, reverse.y=T)

mor <- pi^-0.25 * exp(j*w*x) * exp(-0.5*x^2)
J <- trunc(log2(n * Dt / s0) / dj) # [Eqn(10)]
else J <- p2 / dj

morlet.func <- function(k0=6, Scale, k) {
  n <- length(k)
  expnt <- -(Scale * k - k0) ^ 2 / 2 * as.numeric(k > 0)
  Dt <- 2 * pi / (n * k[2])
  norm <- sqrt(2 * pi * Scale / Dt) * (pi ^ (-0.25)) #  total energy=N   [Eqn(7)]
  
  morlet <- norm * exp(ifelse(expnt > -100, expnt, 100))
  morlet <- morlet * (as.numeric(expnt > -100))  # Avoid underflow errors
  morlet <- morlet * (as.numeric(k > 0))  # Heaviside step function (Morlet is complex)
  fourier_factor <-
    (4 * pi) / (k0 + sqrt(2 + k0 ^ 2)) # Scale-->Fourier [Sec.3h]
  period <- Scale * fourier_factor
  coi <- fourier_factor / sqrt(2)   # Cone-of-influence [Sec.3g]
  ## dofmin = 2   # Degrees of freedom with no smoothing
  ## Cdelta = -1
  ## if(k0 == 6) Cdelta = 0.776 # Reconstruction factor
  ## psi0 = pi^(-0.25)
  list(psi_fft = morlet, period = period, coi = coi)
}


######################################################
# Extracting signals / multiresolution decomposition
######################################################
n <- length(yrs)
nPwrs2 <- trunc(log(n)/log(2)) - 1
dat.mra <- mra(dat, wf = "la8", J = nPwrs2, method = "modwt",
               boundary = "periodic")
yrsLabels <- paste(2^(1:nPwrs2),"yrs",sep="")
par(mar=c(3,2,2,2),mgp=c(1.25,0.25,0),tcl=0.5,xaxs="i",yaxs="i")
plot(yrs,rep(1,n),type="n", axes=FALSE, ylab="",xlab="",
     ylim=c(-3,38))
title(main="Multiresolution decomposition",line=0.75)
axis(side=1)
mtext("Years",side=1,line = 1.25)
Offset <- 0
dat.mra2 <- scale(as.data.frame(dat.mra))
dat.mra2 <- as.data.frame(dat.mra)
for(i in nPwrs2:1){
  x <- dat.mra2[,i] + Offset
  lines(yrs,x)
  abline(h=Offset,lty="dashed")
  mtext(names(dat.mra)[[i]],side=2,at=Offset,line = 0)
  mtext(yrsLabels[i],side=4,at=Offset,line = 0)
  Offset <- Offset+5
}
box()


#####################
# Fourier transform
#####################

del=1
dat.fft <- fft(dat$xxxstd)
abs(dat.fft)
dat.fft <- dat.fft * Conj(dat.fft)
plot(1:length(dat.fft), dat.fft, type='l')

plot(seq(1:478), log(dat.fft), type='l', xlab='time', ylab='f(t)')

plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
  plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
  
  # TODO: why this scaling is necessary?
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
  
  plot(plot.data, t="h", lwd=1, main="", 
       xlab="Frequency (Hz)", ylab="Strength", 
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2])/5)))
}

plot.frequency.spectrum(FkFk, xlimits=c(0, length(dat.fft)/2))

plot(1:100, Conj(morlet(1:100)$wave[,3]), type='l')
     
# From Baldocchi
t = 1:length(dat)
tmax = max(t)
kmax=tmax/2

# Simple sin wave
xt.1 = sin(t*2*pi/tmax)
plot(xt, type='l')

# sin period of 1 out of period of 100 and 25 out of period of 100
xt.25=xt.1+sin(25*t*2*pi/tmax)
plot(xt.25, type='l')

# artificial signal with random noise
xt.rn = xt.25 + sample(tmax, 5)
plot(xt.rn, type='l')

# Fourier transform with Euler
k=1:kmax
Fk = dat * exp(-2 * 1i * pi * k * t / tmax)

fk.mat <- sapply(t, "*", k)
colSums(fk.mat)
fou.eul <- function(x.t, t) {
  tmax = max(t)
  kmax = tmax/2
  k = 1:kmax
  kt.mat = sapply(t, "*", k)
  Fk = x.t * exp(-2 * 1i * pi * kt.mat / tmax)
  Fk = colSums(Fk)
  return(Fk)
}

Fk <- fou.eul(dat, t)
Fkabs = abs(Fk)
FkFk = Fk * Conj(Fk)
plot(t, FkFk, type='h')

Fk <- fou.eul(dat, 1:length(dat))
Fkabs = abs(Fk)
FkFk = Fk * Conj(Fk)
plot(1:length(dat), FkFk, type='h')

# Spectral analysis
dat.sp <- spectrum(dat)
spx <- length(dat)*dat.sp$freq
spy <- 2*dat.sp$spec
plot(spy~spx,xlab="frequency",ylab="spectral density",type="h")

dat.sp

