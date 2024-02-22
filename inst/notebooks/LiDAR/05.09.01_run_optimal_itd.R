### Run LayerStacking with optimal parameters over all plots and export data

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

# Optimal parameters
# st = 0.5
# reso = 0.5
# ws1 = 1
# ws2 = 2
# buf = 0.2

## Run optimization
## ---------------------------------------------------------------------------------------------------
testls <- lapply(lasplots, ls.init, 0.5, 0.5, 1, 2, 0.2, hmin=1.3)

## Reformat results
## ---------------------------------------------------------------------------------------------------

# Unnest results of algorithm
testls <- unlist(testls, recursive=F)
names(testls) <- names(lasplots)

# Convert results to sf
# Since there are extra detected trees outside the plot bound, within the buffer
# specified in the clip_roi function applied to las objects (in 05.00_itc_traintest_loadup.R),
# we must also remove those extra trees
testls <- lapply(seq_along(testls), \(x) {
  trs <- st_as_sf(testls[[x]], crs=32613)
  trs <- st_intersection(trs, plotsf[plotsf$PLOT_ID==names(testls)[x],])
  trs
  })

# Create vector of ITD run IDs
ls.runid <- expand.grid(names(lasplots), '_p', 1)
ls.runid <- ls.runid[order(ls.runid$Var1),]
ls.runid <- paste(ls.runid[,1],ls.runid[,2], ls.runid[,3], sep='')

# Add run IDs to unnested list of ITD outputs
names(testls) <- ls.runid

## Bipartite matching
## ---------------------------------------------------------------------------------------------------

### Run matching
ls.match <- mclapply(ls.runid,
                     FUN=bipart.match3,
                     lasset=testls,
                     obset=stems.in.plots,
                     return.matches=T,
                     mc.cores = getOption("mc.cores", length(workerNodes)-2)
                     )

# Reformat results
names(ls.match) <- names(lasplots)
ls.match <- data.frame(do.call('rbind', ls.match))
ls.match$site <- unlist(lapply(strsplit(rownames(ls.match), '\\.'), '[', 1))

### Run matching and pull summary statistics
ls.match.sums <- mclapply(ls.runid,
                     FUN=bipart.match3,
                     lasset=testls,
                     obset=stems.in.plots,
                     return.matches=F,
                     mc.cores = getOption("mc.cores", length(workerNodes)-2)
)

# Generate summary stats for optimal model
opt.res <- do.call('rbind', ls.match.sums)

opt.mean <- opt.res %>%
  summarise(across(nobstrees:npredtrees, \(x) sum(x, na.rm=T)),
            across(c(ext.rt, match.rt, accuracy:f), \(x) round(sqrt(mean(x^2, na.rm=T)),2)),
  ) %>%
  mutate(ext.rt = round(npredtrees/nobstrees,2))

names(opt.mean) <- c('N detected trees',
                     'N reference trees',
                     'Extraction rate',
                     'Match rate',
                     'Overall accuracy',
                     'Omission rate',
                     'Commission rate',
                     'RMS ∆XY',
                     'RMS ∆Z',
                     'RMS ∆XYZ',
                     'Precision',
                     'Recall',
                     'F'
)

opt.sd <- opt.res %>%
  summarise(across(nobstrees:loss, \(x) sd(x, na.rm=T)))

## Export
## ---------------------------------------------------------------------------------------------------

# Write shapefiles of detected trees at each plot (pre-matching)
lapply(seq_along(testls), \(x) {
  st_write(testls[[x]],
            file.path(config$extdata$itc, 'opt_trees',
                      paste0(names(lasplots[x]), '_opt_trees.shp')))
  })

# Write CSV of matched field and detected trees (post-matching)
write.csv(ls.match,
          file.path(config$extdata$itc, 'opt_matches.csv'), row.names=F)
