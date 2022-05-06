# Define scratch directory
scrdir <- '/global/scratch/users/worsham'

# Read in geolocated returns line count file
geolocr <- read.table(file.path(scrdir, 'geolocated_returns_wc.txt'))
geolocr <- geolocr[!geolocr$V2=='total',]

# Calculate total number of points
nre1 <- sum(geolocr[1])

# Read in gridded returns line count file
gridr <- read.table(file.path(scrdir, 'gridded_returns_wc.txt'))

# Subset to files containing point data
gridr <- gridr[gridr[1]>1,]

# Calculate total number of points
nre2 <- sum(gridr[1])

# Calculate total and percent loss
print(paste('Total points lost:', nre1-nre2))
print(paste('Percent loss:', round(100*(nre1-nre2)/nre1,5), '%'))

      