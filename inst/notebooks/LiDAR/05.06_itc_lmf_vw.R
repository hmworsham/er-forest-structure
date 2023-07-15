### ITC optimization using LMF fixed window

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

# LMF fixed window parameters
ws.seq <- seq(0.2, 10, 0.2)
shape.opts <- c('square', 'circular')
lmf.vw.params <- expand_grid(ws.seq, shape.opts)

## Run optimization
## ---------------------------------------------------------------------------------------------------
# TODO: FIGURE THIS OUT...
lmf.vw.init(lasplots[1], ws=vws, 0.5, 2, 1, shape='square')

testlmf.vw <- lapply(lasplots, lmf.vw.opt, lmf.vw.params)

## Reformat results
## ---------------------------------------------------------------------------------------------------

# Unnest results of algorithm
testlmf.vw <- unlist(testlmf.vw, recursive=F)

# Create vector of ITD run IDs
lmf.vw.runid <- expand.grid(names(lasplots), '_p', row.names(lmf.vw.params))
lmf.vw.runid <- lmf.vw.runid[order(lmf.vw.runid$Var1),]
lmf.vw.runid <- paste(lmf.vw.runid[,1],lmf.vw.runid[,2], lmf.vw.runid[,3], sep='')

# Add run IDs to unnested list of ITD outputs
names(testlmf.vw) <- lmf.vw.runid

# Filter out 0-length list members
testlmf.vw <- Filter(function(x) nrow(x) > 0 , testlmf.vw)

# Update run IDs after filtering
lmf.vw.runid <- lmf.vw.runid[lmf.vw.runid %in% names(testlmf.vw)]

## Bipartite matching
## ---------------------------------------------------------------------------------------------------

### Run matching
lmf.vw.match <- mclapply(lmf.vw.runid,
                     FUN=bipart.match,
                     lasset=testlmf.vw,
                     obset=stems.in.plots,
                     mc.cores = getOption("mc.cores", length(workerNodes)-2)
                     )

# Reformat results
names(lmf.vw.match) <- names(testlmf.vw)
lmf.vw.match <- data.frame(do.call('rbind', lmf.vw.match))
lmf.vw.match$quad <- unlist(lapply(strsplit(rownames(lmf.vw.match), '_'), '[',1))
lmf.vw.match$paramset <- unlist(lapply(strsplit(rownames(lmf.vw.match), '_'), '[', 2))


## Write results
## ---------------------------------------------------------------------------------------------------
write.csv(lmf.vw.match,
          file.path('/global',
                    'scratch',
                    'users',
                    'worsham',
                    'itc_results',
                    'lmf_vw_itc_results.csv'),
          row.names=T)
