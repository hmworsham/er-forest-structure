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
testli <- lapply(lasplots[4:6], li2012.opt, li.params[36:39,])

## Reformat results
## ---------------------------------------------------------------------------------------------------
# Unnest results of algorithm
testli <- unlist(testli, recursive=F)

# Create vector of ITD run IDs
li.runid <- expand.grid(names(lasplots[4:6]), '_p', row.names(li.params[36:39,]))
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
yy <- bipart.match3(li.runid[12], testli, stems.in.plots)
m1 <- yy[[1]]
m2 <- yy[[2]]

View(m1)
View(m2)

m3 <- m1
m3[match(m2$treeID, m3$treeID), ] <- m2
View(m3)
### Run matching
li.match <- mclapply(li.runid,
                     FUN=bipart.match2,
                     lasset=testli,
                     obset=stems.in.plots,
                     plotdir=file.path('/global', 'scratch', 'users', 'worsham', 'itc_results', 'figs', 'li_itc_figs'),
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


## Bipartite match plotting
## --------------------------------------------------------------------------------------------------
li.match.plots <- mclapply(li.runid,
                     FUN=bipart.match.plot,
                     lasset=testli,
                     obset=stems.in.plots,
                     mc.cores = getOption("mc.cores", length(workerNodes)-4)
                     )

View(li.match.plots)

tst <- li.match.plots[[1]]

tst.dfp <- tst[[1]]
tst.dxyt <- tst[[2]]
tst.dxyt$obs <- as.integer(tst.dxyt$obs)

tst.dfm <- left_join(tst.dfp[tst.dfp$src==0,], tst.dxyt, by=c('treeID'='obs')) %>%
  select(pair_id, treeID, treeID.y, pred, Z.x, X.x, Y.x, Z.y, X.y, Y.y, dxy, dz, dxyz)

tst.dfm <- pivot_longer(tst.dfm, cols=c(treeID, treeID.y), names_to= 'src', values_to = 'treeID') %>%
  #filter(row_number() %% 2 != 0) %>%
  arrange(pair_id) %>%
  mutate(src = case_when(src=='treeID.y' ~ 'Modeled',
                         T ~ 'Observed')) %>%
  mutate(across(Z.x:Y.x, ~ ifelse(src=='Modeled', NA, .)),
         across(Z.y:Y.y, ~ ifelse(src=='Observed', NA, .)),
         Z = coalesce(Z.x, Z.y),
         X = coalesce(X.x, X.y),
         Y = coalesce(Y.x, Y.y)) %>%
  select(pair_id, src, treeID, pred, Z, X, Y, dxy, dz, dxyz)

nclr <- nrow(tst.dfm)/2
ggplot(tst.dfm,
              aes(x=X,
                  y=Y,
                  size=Z,
                  shape=factor(src),
                  color=factor(pair_id),
                  label=factor(pair_id))) +
       geom_point() +
       geom_text() +
       scale_color_manual(values=rainbow(nclr, s=.75)[sample(1:nclr, nclr)]) +
       scale_shape_manual(values=c(2,3))

tst.dfm.l <- tst.dfm %>%
  pivot_longer(cols=c(X,Y,Z),
               names_to='dim')

ggplot(tst.dfm.l, aes(x=value, group=src, color=factor(src))) +
    geom_density() +
    facet_wrap(~dim, nrow=3, scales='free')



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


