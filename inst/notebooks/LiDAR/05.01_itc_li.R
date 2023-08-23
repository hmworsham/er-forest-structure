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
testli <- lapply(lasplots, li2012.opt, li.params)

## Reformat results
## ---------------------------------------------------------------------------------------------------
# Unnest results of algorithm
testli <- unlist(testli, recursive=F)

# Create vector of ITD run IDs
li.runid <- expand.grid(names(lasplots), '_p', row.names(li.params))
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
yy <- bipart.match2(li.runid[2], testli, stems.in.plots)

### Run matching
li.match <- mclapply(li.runid,
                     FUN=bipart.match2,
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

## Scratch
## --------------------------------------------------------------------------------------------------
# View(yy[c(1:4, 241:246)])
# dxy.tmp <- xx[[1]]
# dz.tmp <- xx[[2]]
# dxyz.tmp <- xx[[3]]
#
# i=1
# tdxy.tmp <- data.frame(t(dxy.tmp)[5:ncol(dxy.tmp),])
# tdz.tmp <- data.frame(t(dz.tmp)[5:ncol(dz.tmp),])
# tdxyz.tmp <- data.frame(t(dxyz.tmp)[5:ncol(dxyz.tmp),])
# txy.cands <- which(tdxy.tmp[i,] <= dxy.min)
# tz.cands <- which(tdz.tmp[i,] <= dz.min)
# tcands <- intersect(txy.cands, tz.cands)
#
# tdxy.tmp[i, -c(tcands)] <- NA
# tdz.tmp[i, -c(tcands)] <- NA
# tdxyz.tmp[i, -c(tcands)] <- NA
# View(tdxyz.tmp)
#
# min(tdz.tmp[i,], na.rm=T)
# which.min(tdz.tmp[i,]) # 5
# min(tdxy.tmp[i,], na.rm=T)
# which.min(tdxy.tmp[i,]) # 4
#
# tdz.tmp[i,4]
# tdxy.tmp[i,5]
#
# min(tdxyz.tmp[i,], na.rm=T)
# which.min(tdxyz.tmp[i,])
#
# tdxyz.tmp[,17] <- NA
# tdxyz.tmp

# t(apply(x, 1, function(xv) { xv[is.na(xv)] <-
#   mean(xv, na.rm=TRUE)
# return(xv)}
# ) ) # for a row-oriented sol'n


