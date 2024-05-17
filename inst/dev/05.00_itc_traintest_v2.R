### Li 2012

##################
# Define vectors of parameters on which to run algorithm
##################

# Li 2012 parameter sets
dt1.seq = seq(0.5, 2, 0.5)
dt2.seq = seq(0.5, 2, 0.5)
R.seq = 0
Zu.seq = seq(14, 16, 1)

# Expand into dataframe of full combinations
li.params <- expand.grid(dt1.seq, dt2.seq, R.seq, Zu.seq)

# Check dims
length(dt1.seq)*length(dt2.seq)*length(R.seq)*length(Zu.seq) == nrow(li.params)



## ---------------------------------------------------------------------------------------------------
# Run optimization
testli <- lapply(lasplots, li2012.opt, li.params)


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



## ---------------------------------------------------------------------------------------------------


# length(dt1.seq)*length(dt2.seq)*length(R.seq)*length(Zu.seq)
# params <- expand.grid(dt1.seq, dt2.seq, R.seq, Zu.seq)



## ---------------------------------------------------------------------------------------------------
dalponte2016.init <- function(pc) {

  f = function(x) {
    y <- 2.2 * (-(exp(-0.08*(x-2)) - 1)) + 3
    y[x < 2] <- 3
    y[x > 20] <- 7
    return(y)
  }

  chm <- rasterize_canopy(pc, 0.5, algorithm=pitfree(subcircle=0.2))
  modtrees <- locate_trees(pc, lmf(f))
  algo <- dalponte2016(chm, modtrees, th_tree=1.3, th_seed=0.05)
  crowns <- algo()
  return(crowns)
}


## ---------------------------------------------------------------------------------------------------
aa <- dalponte2016.init(lasplots[[29]])


## ---------------------------------------------------------------------------------------------------
k.seq <- c(100, 80, 60, 40, 30, 25, 20, 15, 12, 10, 8, 7, 6, 5)
k.seq2 <- k.seq[order(k.seq)]
k.seq2
k.seq3 <- c()
k.seq4 <- c()
for(i in seq(length(k.seq))) {
  k.seq3[[i]] <- k.seq[1:i]
}

for(i in seq(length(k.seq2))) {
  k.seq4[[i]] <- k.seq2[1:i]
  }

pt.params <- c(k.seq3, k.seq4)


## ---------------------------------------------------------------------------------------------------
testpt <- lapply(lasplots, ptrees.opt, pt.params)


## ---------------------------------------------------------------------------------------------------
# Unnest results of algorithm
testpt <- unlist(testpt, recursive=F)

# Create vector of ITD run IDs
pt.runid <- expand.grid(names(lasplots), '_p', seq_along(pt.params))
pt.runid <- pt.runid[order(pt.runid$Var1),]
pt.runid <- paste(pt.runid[,1],pt.runid[,2], pt.runid[,3], sep='')

# Add run IDs to unnested list of ITD outputs
names(testpt) <- pt.runid

# Filter out 0-length list members
testpt <- Filter(function(x) nrow(x) > 0 , testpt)

# Update run IDs after filtering
pt.runid <- pt.runid[pt.runid %in% names(testpt)]


## ---------------------------------------------------------------------------------------------------

# LayerStacking parameter sets
# start.seq = seq(0.5, 2, 0.5)
# res.seq = c(0.5, 1 , 2)
# ws1.seq = seq(2, 4, 1)
# ws2.seq = seq(0.5, 1.5, 0.5)
# buf.seq = seq(0.25, 1.75, 0.5)

start.seq = seq(0.5, 2, 0.5)
res.seq = c(0.5, 1)
ws1.seq = c(2,3,4)
ws2.seq = c(1,1.5)
buf.seq = 0.5

# Expand into dataframe of full combinations
ls.params <- expand.grid(start.seq, res.seq, ws1.seq, ws2.seq, buf.seq)

# Check dims
length(start.seq)*length(res.seq)*length(ws1.seq)*length(ws2.seq)*length(buf.seq) == nrow(ls.params)


## ---------------------------------------------------------------------------------------------------
ls.init <- function(pc, start, res, ws1, ws2, buf_size, hmin=1.3, hardwood=F) {
  algo <- LayerStacking(start, res, ws1, ws2, buf_size, hmin, hardwood)
  ls.trees <- find_trees(pc, algo)
  return(ls.trees)
}

ls.opt <- function(x, params) {

  # Apply LayerStacking algorithm using all parameter sets
  modtrees <- mcmapply(ls.init,
                       params[,1],
                       params[,2],
                       params[,3],
                       params[,4],
                       params[,5],
                       MoreArgs=list(pc=x, hmin=1.3, hardwood=F),
                       mc.cores = getOption('mc.cores', 30)
                     )

  modtrees <- lapply(modtrees, st_as_sf)
  modtrees <- lapply(modtrees, function(x){
    tl <- x[!st_is_empty(x),]
    tl
  })

  return(modtrees)

}


## ---------------------------------------------------------------------------------------------------
testls <- lapply(lasplots, ls.opt, ls.params)


## ---------------------------------------------------------------------------------------------------
# Unnest results of algorithm
testls <- unlist(testls, recursive=F)

# Create vector of ITD run IDs
ls.runid <- expand.grid(names(lasplots), '_p', row.names(ls.params))
ls.runid <- ls.runid[order(ls.runid$Var1),]
ls.runid <- paste(ls.runid[,1],ls.runid[,2], ls.runid[,3], sep='')

# Add run IDs to unnested list of ITD outputs
names(testls) <- ls.runid

# Filter out 0-length list members
testls <- Filter(function(x) nrow(x) > 0 , testls)

# Update run IDs after filtering
ls.runid <- ls.runid[ls.runid %in% names(testls)]



## ---------------------------------------------------------------------------------------------------
res.seq <- seq(0.5, 2, 0.5)
layer_thickness.seq <- seq(0.5, 2, 0.5)
dist2d.seq <- seq(1, 5, 1)
dist3d.seq <- seq(sqrt(3), sqrt(27), 1)

mc.params <- expand.grid(res.seq, layer_thickness.seq, dist2d.seq, dist3d.seq)


## ---------------------------------------------------------------------------------------------------
mc.init <- function(pc, res, layer_thickness, dist_2d, dist_3d, use_max=F, ws=5) {
  algo <-  multichm(res, layer_thickness, dist_2d, dist_3d, use_max, ws)
  mc.trees <- find_trees(pc, algo)
  return(mc.trees)
}

mc.opt <- function(x, params) {

  # Apply multichm algorithm using all parameter sets
  modtrees <- mcmapply(mc.init,
                     params[,1],
                     params[,2],
                     params[,3],
                     params[,4],
                     MoreArgs=list(pc=x, use_max=F),
                     mc.cores = getOption('mc.cores', 30)
                     )

  # Clean results
  modtrees <- lapply(modtrees, st_as_sf)
  modtrees <- lapply(modtrees, function(x){
    tl <- x[!st_is_empty(x),]
    tl
  })

  return(modtrees)
}



## ---------------------------------------------------------------------------------------------------
testmc <- lapply(lasplots, mc.opt, mc.params[1:10,])


## ---------------------------------------------------------------------------------------------------
# Unnest results of algorithm
testmc <- unlist(testmc, recursive=F)

# Create vector of ITD run IDs
mc.runid <- expand.grid(names(lasplots), '_p', row.names(mc.params)[1:10])
mc.runid <- mc.runid[order(mc.runid$Var1),]
mc.runid <- paste(mc.runid[,1],mc.runid[,2], mc.runid[,3], sep='')

# Add run IDs to unnested list of ITD outputs
names(testmc) <- mc.runid

# Filter out 0-length list members
testmc <- Filter(function(x) nrow(x) > 0 , testmc)

# Update run IDs after filtering
mc.runid <- mc.runid[mc.runid %in% names(testmc)]


## ---------------------------------------------------------------------------------------------------
ws.seq <- seq(0.2, 10, 0.2)
shape.opts <- c('square', 'circular')
lmf.fw.params <- expand_grid(ws.seq, shape.opts)


## ---------------------------------------------------------------------------------------------------
# LMF fixed-window initialization
lmf.fw.init <- function(pc, ws, shape, hmin=1.3){
  algo = lmf(ws=ws, shape=shape, hmin=hmin)
  lmf.fw.trees <- find_trees(pc, algo)
  return(lmf.fw.trees)
}

# LMF fixed-window optimization
lmf.fw.opt <- function(x, params) {

  modtrees <- mcmapply(lmf.fw.init,
                       ws=params[,1][[1]],
                       shape=params[,2][[1]],
                       MoreArgs=list(pc=x, hmin=1.3),
                       mc.cores=getOption('mc.cores', 30)
                     )

  # Clean up results
  modtrees <- lapply(modtrees, st_as_sf)
  modtrees <- lapply(modtrees, function(x) {
    tl <- x[!st_is_empty(x),]
    tl
  })

  return(modtrees)
}



## ---------------------------------------------------------------------------------------------------
testlmf.fw <- lapply(lasplots, lmf.fw.opt, lmf.fw.params)


## ---------------------------------------------------------------------------------------------------
# Unnest results of algorithm
testlmf.fw <- unlist(testlmf.fw, recursive=F)

# Create vector of ITD run IDs
lmf.fw.runid <- expand.grid(names(lasplots), '_p', row.names(lmf.fw.params))
lmf.fw.runid <- lmf.fw.runid[order(lmf.fw.runid$Var1),]
lmf.fw.runid <- paste(lmf.fw.runid[,1],lmf.fw.runid[,2], lmf.fw.runid[,3], sep='')

# Add run IDs to unnested list of ITD outputs
names(testlmf.fw) <- lmf.fw.runid

# Filter out 0-length list members
testlmf.fw <- Filter(function(x) nrow(x) > 0 , testlmf.fw)

# Update run IDs after filtering
lmf.fw.runid <- lmf.fw.runid[lmf.fw.runid %in% names(testlmf.fw)]


## ---------------------------------------------------------------------------------------------------



## ---------------------------------------------------------------------------------------------------
# Define variable window size function

vws = function(x, p1, p2, p3) {
    y <- p1 * (-(exp(-p2*(x-2)) - 1)) + p3
    y[x < 2] <- 3
    y[x > 20] <- 5
    return(y)
}

lmf.vw.init <- function(pc, ws, p1, p2, p3, shape, hmin=1.3) {
  algo = lmf(ws=vws(p1,p2,p3), shape=shape, hmin=hmin)
  lmf.vw.trees <- find_trees(pc, algo)
  return(lmf.vw.trees)
}

lmf.vw.opt <- function(x, params) {
  modtrees <- mapply(lmf.vw.init,
                       ws=params[,1][[1]],
                       shape=params[,2][[1]],
                       MoreArgs=list(pc=x, hmin=1.3)#,
                      # mc.cores=getOption('mc.cores', 30)
                     )

  # Clean up results
  modtrees <- lapply(modtrees, st_as_sf)
  modtrees <- lapply(modtrees, function(x) {
    tl <- x[!st_is_empty(x),]
    tl
  })

  return(modtrees)
}



## ---------------------------------------------------------------------------------------------------
lmf.vw.init(lasplots[1], ws=vws, 0.5, 2, 1, shape='square')



## ---------------------------------------------------------------------------------------------------
li.match <- mclapply(lmf.fw.runid,
             FUN=bipart.match,
             lasset=testlmf.fw,
             obset=stems.in.quads,
             mc.cores = getOption("mc.cores", 30)
             )

names(li.match) <- names(testli)
li.match <- data.frame(do.call('rbind', li.match))
li.match$quad <- unlist(lapply(strsplit(rownames(li.match), '_'), '[',1))
li.match$paramset <- unlist(lapply(strsplit(rownames(li.match), '_'), '[', 2))


## ---------------------------------------------------------------------------------------------------
pt.match <- mclapply(pt.runid,
             FUN=bipart.match,
             lasset=testpt,
             obset=stems.in.quads,
             mc.cores = getOption("mc.cores", 30))

names(pt.match) <- names(testpt)
pt.match <- data.frame(do.call('rbind', pt.match))
pt.match$quad <- unlist(lapply(strsplit(rownames(pt.match), '_'), '[',1))
pt.match$paramset <- unlist(lapply(strsplit(rownames(pt.match), '_'), '[', 2))


## ---------------------------------------------------------------------------------------------------
mc.match <- mclapply(mc.runid,
             FUN=bipart.match,
             lasset=testmc,
             obset=stems.in.plots,
             mc.cores = getOption("mc.cores", 30))

names(mc.match) <- names(testmc)
mc.match <- data.frame(do.call('rbind', mc.match))
mc.match$quad <- unlist(lapply(strsplit(rownames(mc.match), '_'), '[',1))
mc.match$paramset <- unlist(lapply(strsplit(rownames(mc.match), '_'), '[', 2))


## ---------------------------------------------------------------------------------------------------
mc.match.sum <- mc.match %>%
  dplyr::group_by(paramset) %>%
  dplyr::summarise_all(mean, na.rm=T)
View(mc.match.sum)
View(mc.match)

