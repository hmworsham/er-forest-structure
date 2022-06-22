# Load libraries
library(terra)
library(tidyverse)
library(RColorBrewer)
library(lidR)
library(future)

shapedir <- '/global/scratch/users/worsham/EastRiver/RMBL_2020_EastRiver_SDP_Boundary/RMBL_2020_EastRiver_SDP_Boundary'
neondir <- '/global/scratch/users/worsham/neon_las'

bndpath <- list.files(shapedir, pattern='shp$', full.names=T)
bnd <- vect(bndpath)
plot(bnd)

e1 <- cbind(c(320000, 340000, 340000, 320000), c(4250000, 4250000, 4330000, 4330000))
e1 <- vect(e1, type='polygons')

geo <- vect(lascatrg$geometry)
geo$val <- 1
plot(geo)
View(geo)

sub <- erase(geo, e1)
sub2 <- sub[c(1:400, 406:410)]
sub3 <- sub[c(401:404, 411:636)]
plot(sub)
plot(sub[401], col='red', border='red', add=T)
plot(sub2, col='red', add=T)
plot(sub3, col='blue', add=T)

subl.a <- aggregate(buffer(sub2, 0.6), 'val')
subr.a <- aggregate(buffer(sub3, 0.6), 'val')
plot(sub, add=T)
plot(subl.a, col='red', border='red', add=T)
plot(subl.a)
plot(geom(subr.a)[,3:4], type='l')

bnd.sub <- erase(bnd, e1)
miss <- erase(bnd.sub, subl.a)
miss <- erase(miss, subr.a)
plot(miss, col='red')

da <- disagg(miss)[1]
neoncat <- readLAScatalog(list.files(neondir, full.names=T, pattern='*2018062013*')[1:5])

plot(da)
plot(neoncat, add=T)

writeVector(da, '/global/scratch/users/worsham/EastRiver/missing_flightpath.shp')
