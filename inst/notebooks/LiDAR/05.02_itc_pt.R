### ITC optimization using PTrees

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

# PTrees parameters
k.seq <- c(100, 80, 60, 40, 30, 25, 20, 15, 12, 10, 8, 7, 6, 5)
k.seq2 <- k.seq[order(k.seq)]
k.seq2
k.seq3 <- c()
k.seq4 <- c()
for(i in seq(length(k.seq))) {
  k.seq3[[i]] <- k.seq[1:i]
}

# Permute list of parameters
for(i in seq(length(k.seq2))) {
  k.seq4[[i]] <- k.seq2[1:i]
}

# Bind permutations into single list
pt.params <- c(k.seq3, k.seq4)

## Run optimization
## ---------------------------------------------------------------------------------------------------
testpt <- lapply(lasplots, ptrees.opt, pt.params, hmin=1.3)

## Reformat results
## ---------------------------------------------------------------------------------------------------
# Unnest results of algorithm
testpt <- unlist(testpt, recursive=F)

# Since there are extra detected trees outside the plot bound, within the buffer
# specified in the clip_roi function applied to las objects (in 05.00_itc_traintest_loadup.R),
# we must also remove those extra trees
pltid <- paste0(gsub("[^a-zA-Z-]", "", names(testpt)), substr(gsub("[^0-9]", '', names(testpt)), 1,1))
testpt <- lapply(seq_along(testpt), \(x) {
  trs <- st_as_sf(testpt[[x]], crs=32613)
  trs <- st_intersection(trs, plotsf[plotsf$PLOT_ID==pltid[x],])
  trs
})

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

## Bipartite matching
## ---------------------------------------------------------------------------------------------------

### Run matching
pt.match <- mclapply(pt.runid,
                     FUN=bipart.match3,
                     lasset=testpt,
                     obset=stems.in.plots,
                     plotdir=file.path('/global', 'scratch', 'users', 'worsham', 'itc_results', 'figs', 'pt_itc_figs'),
                     mc.cores = getOption("mc.cores", length(workerNodes)-2)
                     )

# Reformat results
names(pt.match) <- names(testpt)
pt.match <- data.frame(do.call('rbind', pt.match))
pt.match$quad <- unlist(lapply(strsplit(rownames(pt.match), '_'), '[',1))
pt.match$paramset <- unlist(lapply(strsplit(rownames(pt.match), '_'), '[', 2))

## Write results
## ---------------------------------------------------------------------------------------------------
write.csv(pt.match,
          file.path('/global',
                    'scratch',
                    'users',
                    'worsham',
                    'itc_results',
                    'pt_itc_results.csv'),
          row.names=T)
