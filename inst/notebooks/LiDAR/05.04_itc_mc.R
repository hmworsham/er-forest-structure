# ITD optimization using multichm
# Author: Marshall Worsham | worsham@berkeley.edu
# Created: 04-24-23
# Revised: 07-23-24

#############################
# Set up working environment
#############################

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Source loadup file
source(file.path('~', 'Repos', 'er-forest-structure', 'inst', 'notebooks', 'LiDAR', '05.00_itc_traintest_loadup.R'))

#############################
# Define parameters
#############################

# Multichm parameter sets
res.seq <- c(0.5, 1)
layer_thickness.seq <- c(0.25, 0.5)
dist2d.seq <- c(0.1, 0.25, 0.5, 1, 2)
dist3d.seq <- c(0.5,1,3,5)

mc.params <- expand.grid(res.seq, layer_thickness.seq, dist2d.seq, dist3d.seq)

#############################
# Run optimization
#############################

testmc <- lapply(lasplots, mc.opt, mc.params)

#############################
# Clean results
#############################

# Unnest results of algorithm
testmc <- unlist(testmc, recursive=F)

# Since there are extra detected trees outside the plot bound, within the buffer
# specified in the clip_roi function applied to las objects (in 05.00_itc_traintest_loadup.R),
# we must also remove those extra trees
pltid <- paste0(gsub("[^a-zA-Z-]", "", names(testmc)), substr(gsub("[^0-9]", '', names(testmc)), 1,1))
testmc <- lapply(seq_along(testmc), \(x) {
  trs <- st_as_sf(testmc[[x]], crs=32613)
  trs <- st_intersection(trs, plotsf[plotsf$PLOT_ID==pltid[x],])
  trs
})

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

#############################
# Bipartite matching
#############################

### Run matching
mc.match <- mclapply(mc.runid,
                     FUN=bipart.match3,
                     lasset=testmc,
                     obset=stems.in.plots,
                     plotdir=file.path('/global', 'scratch', 'users', 'worsham', 'itc_results', 'figs', 'mc_itc_figs'),
                     mc.cores = getOption("mc.cores", length(workerNodes)-2)
                     )

# Reformat results
names(mc.match) <- names(testmc)
mc.match <- data.frame(do.call('rbind', mc.match))
mc.match$quad <- unlist(lapply(strsplit(rownames(mc.match), '_'), '[',1))
mc.match$paramset <- unlist(lapply(strsplit(rownames(mc.match), '_'), '[', 2))

#############################
# Write results
#############################

write.csv(mc.match,
          file.path(config$extdata$scratch,
                    'itc_results',
                    'mc_itc_results.csv'),
          row.names=T)
