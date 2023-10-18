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

# LMF auto parameters
# No params

## Run optimization
## ---------------------------------------------------------------------------------------------------
testlmf.auto <- lapply(lasplots, lmf.auto.opt, hmin=1.3)

## Reformat results
## ---------------------------------------------------------------------------------------------------

# Unnest results of algorithm
# testlmf.auto <- unlist(testlmf.auto, recursive=F)

# Create vector of ITD run IDs
lmf.auto.runid <- paste(names(lasplots), '_p', 1, sep='')

# Add run IDs to unnested list of ITD outputs
names(testlmf.auto) <- lmf.auto.runid

# Filter out 0-length list members
testlmf.auto <- Filter(function(x) nrow(x) > 0 , testlmf.auto)

# Update run IDs after filtering
lmf.auto.runid <- lmf.auto.runid[lmf.auto.runid %in% names(testlmf.auto)]

## Bipartite matching
## ---------------------------------------------------------------------------------------------------

### Run matching
lmf.auto.match <- mclapply(lmf.auto.runid,
                     FUN=bipart.match3,
                     lasset=testlmf.auto,
                     obset=stems.in.plots,
                     plotdir=file.path('/global', 'scratch', 'users', 'worsham', 'itc_results', 'figs', 'lmfauto_itc_figs'),
                     mc.cores = getOption("mc.cores", length(workerNodes)-2)
                     )

# Reformat results
names(lmf.auto.match) <- names(testlmf.auto)
lmf.auto.match <- data.frame(do.call('rbind', lmf.auto.match))
lmf.auto.match$quad <- unlist(lapply(strsplit(rownames(lmf.auto.match), '_'), '[',1))
lmf.auto.match$paramset <- unlist(lapply(strsplit(rownames(lmf.auto.match), '_'), '[', 2))

## Write results
## ---------------------------------------------------------------------------------------------------
write.csv(lmf.auto.match,
          file.path('/global',
                    'scratch',
                    'users',
                    'worsham',
                    'itc_results',
                    'lmfauto_itc_results.csv'),
          row.names=T)
