
# Ingest gridded points
infiles <- list.files(datadir, full.names=T)
lascat <- readLAScatalog(infiles)
plot(lascat['Number.of.point.records'])

las1 <- readLAS(infiles[6])

myareas <- st_area(lascat@data$geometry)
mydst <- lascat['Number.of.point.records']$Number.of.point.records / myareas
mydst <- 1075077632 / sum(myareas)
mydst <- sum(lascat['Number.of.point.records']$Number.of.point.records) / sum(myareas)
mean(mydst)
base::mean(mydst)
base::mean()
mydst

neons <- list.files(neondir, full.names=T, pattern='.las')
neoncat <- readLAScatalog(neons)
las_check(neoncat)
sum(neoncat['Number.of.point.records']$Number.of.point.records)
plot(neoncat['Number.of.point.records'])
neonareas <- st_area(neoncat@data$geometry)
dst <- neoncat['Number.of.point.records']$Number.of.point.records / neonareas

mean(dst[!dst==Inf])

sapply()

neon1 <- readLAS(neons[4])