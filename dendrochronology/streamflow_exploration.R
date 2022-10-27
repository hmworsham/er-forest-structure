library(dplyr)
library(signal)
library(stats)
library(ggplot2)

er.q <- read.delim('/Volumes/GoogleDrive/.shortcut-targets-by-id/1HZhH3KecyMfW0wQ8V8iJr7rmCNYOzF7t/RMBL-East River Watershed Forest Data/Data/Streamflow/EastRiver_Cement/Streamflow_Monthly_EastRiver_Cement_09112200.txt', header=T, skip=35)
er.q <- er.q[2:nrow(er.q),]
rownames(er.q) <- NULL

er.q$mean_va <- as.numeric(er.q$mean_va)

er.q$dt <- as.Date(paste(er.q$year_nu, sprintf('%02d', as.numeric(er.q$month_nu)), as.numeric(1), sep='-'))


q.plot <- ggplot(er.q, aes(x=dt, y=mean_va)) +
  geom_line()

q.plot


x <- 1:4
plot(fft(x))
fft(fft(x), inverse = TRUE)/length(x)

## Slow Discrete Fourier Transform (DFT) - e.g., for checking the formula
fft0 <- function(z, inverse=FALSE) {
  n <- length(z)
  if(n == 0) return(z)
  k <- 0:(n-1)
  ff <- (if(inverse) 1 else -1) * 2*pi * 1i * k/n
  vapply(1:n, function(h) sum(z * exp(ff*(h-1))), complex(1))
}

relD <- function(x,y) 2* abs(x - y) / abs(x + y)
n <- 2^8
z <- complex(n, rnorm(n), rnorm(n))
# }
# NOT RUN {
## relative differences in the order of 4*10^{-14} :
summary(relD(fft(z), fft0(z)))
summary(relD(fft(z, inverse=TRUE), fft0(z, inverse=TRUE)))

