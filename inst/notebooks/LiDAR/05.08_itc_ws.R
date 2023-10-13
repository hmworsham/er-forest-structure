### ITC optimization using watershed segmentation

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

# Initialize watershed algorithm
ws.init <- function(pc, res, p2r.p, ker.size, hmin, pkg='raster') {
  chm = rasterize_canopy(pc, res=res, algorithm=p2r(0.3), pkg='raster')
  chm = raster::focal(chm, w=matrix(1,ker.size, ker.size), fun=mean, na.rm=T)
  ws.trees = segment_trees(pc, watershed(chm))
  crowns = crown_metrics(ws.trees, func = .stdtreemetrics, geom = "convex")
  crowns = crowns[st_is_valid(crowns),]
  ttops = st_centroid(crowns)
  ttops = ttops[ttops$Z >= hmin,]
  return(ttops)
}

ws.opt <- function(x, params, hmin=1.3) {
  modtrees <- mcmapply(ws.init,
                       res=params[,1],
                       p2r.p=params[,2],
                       ker.size=params[,3],
                       MoreArgs=list(pc=x, hmin=hmin),
                       mc.cores = getOption("mc.cores", 30))

  # Clean up results
  modtrees <- apply(modtrees, 2, data.frame)
  modtrees <- lapply(modtrees, st_as_sf)
  modtrees <- lapply(modtrees, function(x) {
    tl <- x[!st_is_empty(x),]
    tl
  })

  return(modtrees)
}

res.seq <- seq(0.5, 2, 0.5)
p2r.p.seq <- seq(0.1,0.8, 0.1)
ker.size.seq <- c(3,5,9,15)

# Expand into dataframe of full combinations
ws.params <- expand.grid(res.seq, p2r.p.seq, ker.size.seq)

# Check dims
length(res.seq)*length(p2r.p.seq)*length(ker.size.seq) == nrow(ws.params)

## Run optimization
## ---------------------------------------------------------------------------------------------------
testws <- lapply(lasplots[1:2], ws.opt, ws.params[10:12,], hmin=2)

## Reformat results
## ---------------------------------------------------------------------------------------------------
# Unnest results of algorithm
testws <- unlist(testws, recursive=F)

# Create vector of ITD run IDs
ws.runid <- expand.grid(names(lasplots[1:2]), '_p', row.names(ws.params[10:12,]))
ws.runid <- ws.runid[order(ws.runid$Var1),]
ws.runid <- paste(ws.runid[,1],ws.runid[,2], ws.runid[,3], sep='')

# Add run IDs to unnested list of ITD outputs
names(testws) <- ws.runid

# Filter out 0-length list members
testws <- Filter(function(x) nrow(x) > 0 , testws)

# Update run IDs after filtering
ws.runid <- ws.runid[ws.runid %in% names(testws)]

## Bipartite matching
## ---------------------------------------------------------------------------------------------------

### Run matching
ws.match <- lapply(ws.runid,
                   FUN=bipart.match3,
                   lasset=testws,
                   obset=stems.in.plots,
                   plotdir=file.path('/global', 'scratch', 'users', 'worsham', 'itc_results', 'figs', 'ws_itc_figs'),
                   mc.cores = getOption("mc.cores", length(workerNodes)-4)
                   )

# Reformat results
names(ws.match) <- names(testws)
ws.match <- data.frame(do.call('rbind', ws.match))
ws.match$quad <- unlist(lapply(strsplit(rownames(ws.match), '_'), '[',1))
ws.match$paramset <- unlist(lapply(strsplit(rownames(ws.match), '_'), '[', 2))

## Write results
## ---------------------------------------------------------------------------------------------------
write.csv(ws.match,
          file.path('/global',
                    'scratch',
                    'users',
                    'worsham',
                    'itc_results',
                    'ws_itc_results.csv'),
          row.names=T)
