### ITC optimization using multichm

## Workspace setup
## ---------------------------------------------------------------------------------------------------

config <- config::get(file=file.path('~',
                                     'Repos',
                                     'er-forest-structure',
                                     'config',
                                     'config.yml'))

# Load local helper functions and packages
source(file.path('~', 'Repos', 'er-forest-structure', 'inst', 'notebooks', 'LiDAR', '05.00_itc_traintest_loadup.R'))


## Define vectors of parameters on which to run algorithm
## ---------------------------------------------------------------------------------------------------

# Multichm parameter sets
res.seq <- c(0.5, 1)
layer_thickness.seq <- c(0.25, 0.5)
dist2d.seq <- c(0.1, 0.25, 0.5, 1, 2)
dist3d.seq <- c(0.5,1,3,5)

mc.params <- expand.grid(res.seq, layer_thickness.seq, dist2d.seq, dist3d.seq)
dim(mc.params)

## Run optimization
## ---------------------------------------------------------------------------------------------------
testmc <- lapply(lasplots, mc.opt, mc.params)

## Reformat results
## ---------------------------------------------------------------------------------------------------

# Unnest results of algorithm
testmc <- unlist(testmc, recursive=F)

# Create vector of ITD run IDs
mc.runid <- expand.grid(names(lasplots), '_p', row.names(mc.params))
mc.runid <- mc.runid[order(mc.runid$Var1),]
mc.runid <- paste(mc.runid[,1],mc.runid[,2], mc.runid[,3], sep='')

# Add run IDs to unnested list of ITD outputs
names(testmc) <- mc.runid

# Filter out 0-length list members
testmc <- Filter(function(x) nrow(x) > 0 , testmc)

# Update run IDs after filtering
mc.runid <- mc.runid[mc.runid %in% names(testmc)]

## Bipartite matching
## ---------------------------------------------------------------------------------------------------

### Run matching
mc.match <- mclapply(mc.runid,
                     FUN=bipart.match2,
                     lasset=testmc,
                     obset=stems.in.plots,
                     mc.cores = getOption("mc.cores", length(workerNodes)-2)
                     )

# Reformat results
names(mc.match) <- names(testmc)
mc.match <- data.frame(do.call('rbind', mc.match))
mc.match$quad <- unlist(lapply(strsplit(rownames(mc.match), '_'), '[',1))
mc.match$paramset <- unlist(lapply(strsplit(rownames(mc.match), '_'), '[', 2))

## Write results
## ---------------------------------------------------------------------------------------------------
write.csv(mc.match,
          file.path('/global',
                    'scratch',
                    'users',
                    'worsham',
                    'itc_results',
                    'mc_itc_results.csv'),
          row.names=T)
