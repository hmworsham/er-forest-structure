# ITD optimization using LMF fixed window
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

# LMF fixed window parameters
ws.seq <- seq(0.2, 10, 0.2)
shape.opts <- c('square', 'circular')
lmf.fw.params <- expand_grid(ws.seq, shape.opts)

#############################
# Run optimization
#############################

testlmf.fw <- lapply(lasplots, lmf.fw.opt, lmf.fw.params, hmin=1.3)

#############################
# Clean results
#############################

# Unnest results of algorithm
testlmf.fw <- unlist(testlmf.fw, recursive=F)

# Since there are extra detected trees outside the plot bound, within the buffer
# specified in the clip_roi function applied to las objects (in 05.00_itc_traintest_loadup.R),
# we must also remove those extra trees
pltid <- paste0(gsub("[^a-zA-Z-]", "", names(testlmf.fw)), substr(gsub("[^0-9]", '', names(testlmf.fw)), 1,1))
testlmf.fw <- lapply(seq_along(testlmf.fw), \(x) {
  trs <- st_as_sf(testlmf.fw[[x]], crs=32613)
  trs <- st_intersection(trs, plotsf[plotsf$PLOT_ID==pltid[x],])
  trs
})

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

#############################
# Bipartite matching
#############################

### Run matching
lmf.fw.match <- mclapply(lmf.fw.runid,
                     FUN=bipart.match3,
                     lasset=testlmf.fw,
                     obset=stems.in.plots,
                     plotdir=file.path('/global', 'scratch', 'users', 'worsham', 'itc_results', 'figs', 'lmffw_itc_figs'),
                     mc.cores = getOption("mc.cores", length(workerNodes)-4)
                     )

# Reformat results
names(lmf.fw.match) <- names(testlmf.fw)
lmf.fw.match <- data.frame(do.call('rbind', lmf.fw.match))
lmf.fw.match$quad <- unlist(lapply(strsplit(rownames(lmf.fw.match), '_'), '[',1))
lmf.fw.match$paramset <- unlist(lapply(strsplit(rownames(lmf.fw.match), '_'), '[', 2))

#############################
# Write results
#############################

write.csv(lmf.fw.match,
          file.path(config$extdata$scratch,
                    'itc_results',
                    'lmffw_itc_results.csv'),
          row.names=T)
