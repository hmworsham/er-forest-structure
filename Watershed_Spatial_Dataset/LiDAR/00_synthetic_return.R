library('caTools')
library('devtools')
load_all('~/Repos/rwaveform')

x1 <- seq(1,28)
x2 <- seq(1,12)
x3 <- seq(1,24)
x4 <- seq(1,43)
x5 <- seq(1,7)
x6 <- seq(1,500-sum(lengths(c(x1,x2,x3,x4,x5))))

y1 <- 585*dnorm(x1, 19, 0.9)
y2 <- 122*dnorm(x2, 5, 2.2)
y3 <- 108*dnorm(x3, 20, 2.6)
y4 <- 73*dnorm(x4, 11, 3)
y5 <- 64*dnorm(x5, 4, 0.7)
y6 <- rep(0,length(x6))

x <- seq(1,500)
y <- c(y1,y2,y3,y4,y5,y6)
plot(y, type='l')

gaus <- function(x, A, mu, sd, r){
  yhat = A * exp(-(abs(x-mu)^r/(2*sd^2)))
  return(yhat)
}

lines(gaus(x, 185.66, 18, 1.29, 2), col='red')

y0<-as.numeric(y)
#index<-as.integer(y0[1])
#y<-y0[-1]
y[y==0]<-NA

### Prep for direct decomposition

# Smooth waveform with running mean and window of size width, using 'C' algorithm; 'fast' can't handle na

y <- runmean(y, 3, "C")

# Restore NAs to 0
y[is.na(y)] <- 0

# Fix problematic peaks if necessary
y <- rwaveform::peakfix(y)

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
