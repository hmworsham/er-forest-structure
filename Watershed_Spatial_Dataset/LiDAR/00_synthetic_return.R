# Load libraries
library('caTools')
library('devtools')
load_all('~/Repos/rwaveform')

# Define x sequences
x1 <- seq(1,28)
x2 <- seq(1,12)
x3 <- seq(1,24)
x4 <- seq(1,43)
x5 <- seq(1,7)
x6 <- seq(1,500-sum(lengths(c(x1,x2,x3,x4,x5))))

# Calculate y sequences with x and random noise
y1 <- 585*dnorm(x1, 19, 0.9)
y2 <- 122*dnorm(x2, 5, 2.2)
y3 <- 108*dnorm(x3, 20, 2.6)
y4 <- 73*dnorm(x4, 11, 3)
y5 <- 64*dnorm(x5, 4, 0.7)
y6 <- rep(0,length(x6))

# Define x,y for gaussian addition
x <- seq(1,500)
y <- c(y1,y2,y3,y4,y5,y6)
plot(y, type='l')

# Define function for gaussian estimation
gaus <- function(x, A, mu, sd, r){
  yhat = A * exp(-(abs(x-mu)^r/(2*sd^2)))
  return(yhat)
}

# Define params for Gaussian curves
A <- c(200, 150, 120)
mu <- c(24, 45, 81)
sd <- c(6,10,16)
r <- rep(2,3)

# Compute Gaussians
gg <- lapply(seq(1,3), function(i) gaus(x, A[i], mu[i], sd[i], r[i]))
gg <- gg[[1]]+gg[[2]]+gg[[3]]
lines(gg[1:100])

# Compute adjusted Gaussians
dv <- lapply(seq(1,3), function(i) gaus(x, A[i]*4, mu[i]+3, sd[i]/10, r[i]))
dv <- dv[[1]]+dv[[2]]+dv[[3]]
dv[dv==0] <- rnorm(length(dv[dv==0]), 25, 3)
plot(dv[1:100], col='red', type='l')


##############################
# Deconvolve synthetic return
##############################

# Generate synthetic outgoing pulse
out <- gaus(x, 620, 25, 18, 2)
out <- out+210
plot(out, type='l')

# Generate synthetic impulse response
imp <- gaus(x, 300, 32, 18, 2)
imp <- imp + 210
imp[58] <- 301
imp[59] <- 221
imp[60] <- 112
imp[61] <- 2
imp[62] <- 0
imp[63:length(imp)] <- 0
plot(imp, type='l')

# Deconvolve waveforms
dv <- waveformlidar::deconvolution(gg, out, imp)

# Decompose waveforms
dc <- waveformlidar::decom(dv)[[3]]
dc[,3]
dc <- lapply(seq(1,2), function(i) gaus(x, dc[i,2], dc[i,3], dc[i,4], r[i]))
dc <- dc[[1]]+dc[[2]]
lines(dc[1:100], col='orange')

#######################
# Final plot for figure
#######################

# Prep dataframe with synthetic raw, deconv, and decomp data
df <- data.frame(gg, dv, dc)[1:100,]
df <- reshape(df, varying=list(names(df)), direction='long')
df$time <- as.factor(df$time)
levels(df$time) <- c('Raw', 'Deconvolved', 'Decomposed')
names(df) <- c('Waveforms', 'Amplitude', 'Time')

# Plot raw, deconv, and decomp curves
ggplot(df, aes(x=Time, y=Amplitude, col=Waveforms, linetype=Waveforms, size=Waveforms)) +
  geom_line() + 
  scale_color_manual(values=c('grey20', 'darkblue', 'turquoise')) +
  scale_linetype_manual(values=c(2,1,1)) +
  scale_size_manual(values=c(1,1.5,1)) +
  labs(title='Waveform Intensity Estimation', x='Time (ns)', y='Amplitude') +
  theme_bw() +
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=26,face="bold"),
        plot.title=element_text(size=30, hjust=0.5, face='bold'),
        legend.title=element_text(size=26, face='bold'), 
        legend.text=element_text(size=26))
  # geom_line(aes(x=1:150, y=gg), col='black', linetype=2, size=1) + 
  # geom_line(aes(x=1:150, y=dv), col='darkblue', size=2) +
  # geom_line(aes(x=1:150, y=dc), col='turquoise', size=1) + 



#######################################
### Prep for direct decomposition
#######################################

# y0<-as.numeric(y)
# index<-as.integer(y0[1])
# y<-y0[-1]
# y[y==0]<-NA
# 
# # Smooth waveform with running mean and window of size width, using 'C' algorithm; 'fast' can't handle na
# 
# y <- runmean(y, 3, "C")
# 
# # Restore NAs to 0
# y[is.na(y)] <- 0
# 
# # Fix problematic peaks if necessary
# y <- rwaveform::peakfix(y)
# 
# # Identify peaks
# peakrecord <- lpeak(y, 3)
# 
# # Find return time of peak
# peaktime <- which(peakrecord == T)
# n.peaks = length(peaktime)
# 
# # Catch errors where the deconvolved waveform finds no peaks
# if (n.peaks == 0){
#   peaktime <- suppressWarnings(which.max(y))
# }
# 
# # Filter out noisy peaks (those less than threshold*max intensity in the return vector)
# imax <- suppressWarnings(max(y, na.rm=T))
# ind <- y[peaktime] >= imax
# 
# # Get the time of true peaks
# realind<-peaktime[ind]
# 
# # Get the intensity of real peaks
# newpeak<-y[realind]
# 
# # Get the number of true peaks
# z <- length(realind)
