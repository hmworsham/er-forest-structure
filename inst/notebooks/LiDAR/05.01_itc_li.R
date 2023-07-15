### ITC optimization using Li 2012

## Workspace setup
## ---------------------------------------------------------------------------------------------------

# Load config
config <- config::get(file=file.path('~',
                                     'Repos',
                                     'er-forest-structure',
                                     'config',
                                     'config.yml'))

# Load local helper functions and packages
source(file.path('~', 'Repos', 'er-forest-structure', 'inst', 'notebooks', 'LiDAR', '05.00_itc_traintest_loadup.R'))

## Define vectors of parameters on which to run algorithm
## ---------------------------------------------------------------------------------------------------

# Li 2012 parameter sets
dt1.seq = seq(0.5, 2, 0.5)
dt2.seq = seq(0.5, 2, 0.5)
R.seq = 0
Zu.seq = seq(14, 16, 1)

# Expand into dataframe of full combinations
li.params <- expand.grid(dt1.seq, dt2.seq, R.seq, Zu.seq)

# Check dims
length(dt1.seq)*length(dt2.seq)*length(R.seq)*length(Zu.seq) == nrow(li.params)

## Run optimization
## ---------------------------------------------------------------------------------------------------
testli <- lapply(lasplots[1:2], li2012.opt, li.params[30:34,])

## Reformat results
## ---------------------------------------------------------------------------------------------------
# Unnest results of algorithm
testli <- unlist(testli, recursive=F)

# Create vector of ITD run IDs
li.runid <- expand.grid(names(lasplots)[1:2], '_p', row.names(li.params)[30:34])
li.runid <- li.runid[order(li.runid$Var1),]
li.runid <- paste(li.runid[,1],li.runid[,2], li.runid[,3], sep='')

# Add run IDs to unnested list of ITD outputs
names(testli) <- li.runid

# Filter out 0-length list members
testli <- Filter(function(x) nrow(x) > 0 , testli)

# Update run IDs after filtering
li.runid <- li.runid[li.runid %in% names(testli)]

## Bipartite matching
## ---------------------------------------------------------------------------------------------------
xx <- bipart.match2(li.runid[2], testli, stems.in.plots)

ggplot(xx, aes(x=X.x, y=Y.x, color=factor(src), size=Z.x, shape=factor(src))) +
  geom_point() +
  scale_color_manual(values=c('red', 'darkblue')) +
  scale_shape_manual(values=c(4,21))


library(data.table)
dmat <- xx@.Data
y.dmat <- cbind(data.frame(testli[[2]]), dmat)
# y.dmat<- y.dmat %>% mutate(dmin = case_when(Z<=10 ~ 3,
#                          10 < Z & Z <= 15 ~ 4,
#                          15 < Z & Z <=25 ~ 5,
#                          Z > 25 ~ 5))
y.dmat <- data.frame(y.dmat)
y.dmat2 <- y.dmat[,6:ncol(y.dmat)]



nobs <- ncol(dmat)
for(i in seq(nrow(y.dmat))) {
  z <- y.dmat[i,'Z']
  dmin <- case_when(z<=10 ~ 3,
                    10 < z & z <= 15 ~ 4,
                    15 < z & z <=25 ~ 5,
                    z > 25 ~ 5)
  y.dmat[i, 5+which(y.dmat[i, 6:nobs] > dmin)] <- NA
  match <- which.min(y.dmat[x,6:nobs])
  d <- y.dmat[x, match]
  y.dmat[,5+match] <- NA
  y.dmat[i, 'pred'] <- i
  y.dmat[i, 'obs'] <- match
  y.dmat[i, 'pair_id'] <- i
  }



for(i in y.dmat$obs) {
  print(paste('Obs', i))
  match <- which.min(y.dmat2[,i])
  print(paste('Pred', match))
}


candid <- function(x, distmat) {
  # by row, anything outside minimum search area is NA
  distmat[x, distmat[x, 6:(ncol(distmat)-1)] > distmat[x, 'dmin']] <- NA
  match <- which.min(distmat[x,6:(ncol(distmat)-1)])
  d <- distmat[x, match]
  distmat[,5+argmin] <- NA
  distmat$pred <- x
  distmat$obs <- match
  distmat$pair_id <- x
  distmat
}
lapply(1:3, candid, y.dmat)

y <- y.dmat[1,]
y[y[6:236]>y$dmin] <- NA
y['dmin']

candid2 <-function(j) {
  j[j[6:(length(j)-1)]>j$dmin] <- NA
}
View(apply(y.dmat, 1, candid2))


matches
x <- 1
y.dmat[x, y.dmat[x, 6:(ncol(y.dmat)-1)] > y.dmat[x, 'dmin']]

k <- which(y.dmat[,6:ncol(y.dmat)-1] > y.dmat[,'dmin'], arr.ind=TRUE)
y.dmat[,y.dmat[,6:ncol(y.dmat)-1] > y.dmat[,'dmin']]
m[k] <- rowMeans(m, na.rm=TRUE)[k[,1]]


t(apply(x, 1, function(xv) { xv[is.na(xv)] <-
  mean(xv, na.rm=TRUE)
return(xv)}
) ) # for a row-oriented sol'n



### Run matching
li.match <- mclapply(li.runid[1:3],
                     FUN=bipart.match,
                     lasset=testli,
                     obset=stems.in.plots,
                     mc.cores = getOption("mc.cores", length(workerNodes)-4)
                     )

# Reformat results
names(li.match) <- names(testli)
li.match <- data.frame(do.call('rbind', li.match))
li.match$quad <- unlist(lapply(strsplit(rownames(li.match), '_'), '[',1))
li.match$paramset <- unlist(lapply(strsplit(rownames(li.match), '_'), '[', 2))

## Write results
## ---------------------------------------------------------------------------------------------------
write.csv(li.match,
          file.path('/global',
                    'scratch',
                    'users',
                    'worsham',
                    'itc_results',
                    'li_itc_results.csv'),
          row.names=T)
