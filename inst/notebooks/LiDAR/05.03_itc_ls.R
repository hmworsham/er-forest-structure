### ITC optimization using LayerStacking

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

# LayerStacking parameter sets
start.seq = 0.5
res.seq = c(0.5, 1)
ws1.seq = seq(1, 3, 1)
ws2.seq = seq(1, 3, 1)
buf.seq = seq(0.2, 0.8, 0.2)

# Expand into dataframe of full combinations
ls.params <- expand.grid(start.seq, res.seq, ws1.seq, ws2.seq, buf.seq)

# Check dims
length(start.seq)*length(res.seq)*length(ws1.seq)*length(ws2.seq)*length(buf.seq) == nrow(ls.params)

## Run optimization
## ---------------------------------------------------------------------------------------------------
testls <- lapply(lasplots, ls.opt, ls.params)

## Reformat results
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

## Bipartite matching
## ---------------------------------------------------------------------------------------------------

### Run matching
ls.match <- mclapply(ls.runid,
                     FUN=bipart.match,
                     lasset=testls,
                     obset=stems.in.plots,
                     mc.cores = getOption("mc.cores", length(workerNodes)-2)
                     )

# Reformat results
names(ls.match) <- names(testls)
ls.match <- data.frame(do.call('rbind', ls.match))
ls.match$quad <- unlist(lapply(strsplit(rownames(ls.match), '_'), '[',1))
ls.match$paramset <- unlist(lapply(strsplit(rownames(ls.match), '_'), '[', 2))

## Write results
## ---------------------------------------------------------------------------------------------------
write.csv(ls.match,
          file.path('/global',
                    'scratch',
                    'users',
                    'worsham',
                    'itc_results',
                    'ls_itc_results.csv'),
          row.names=T)
