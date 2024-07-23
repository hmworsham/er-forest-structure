# ITD optimization using Li 2012
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

# Li 2012 parameter sets
dt1.seq = seq(0.5, 2, 0.5)
dt2.seq = seq(0.5, 2, 0.5)
R.seq = 0
Zu.seq = seq(14, 16, 1)

# Expand into dataframe of full combinations
li.params <- expand.grid(dt1.seq, dt2.seq, R.seq, Zu.seq)

# Check dims
length(dt1.seq)*length(dt2.seq)*length(R.seq)*length(Zu.seq) == nrow(li.params)

#############################
# Run optimization
#############################

testli <- lapply(lasplots, li2012.opt, li.params, hmin=1.3)


#############################
# Clean results
#############################

# Unnest results of algorithm
testli <- unlist(testli, recursive=F)

# Since there are extra detected trees outside the plot bound, within the buffer
# specified in the clip_roi function applied to las objects (in 05.00_itc_traintest_loadup.R),
# we must also remove those extra trees
pltid <- paste0(gsub("[^a-zA-Z-]", "", names(testli)), substr(gsub("[^0-9]", '', names(testli)), 1,1))
testli <- lapply(seq_along(testli), \(x) {
  trs <- st_as_sf(testli[[x]], crs=32613)
  trs <- st_intersection(trs, plotsf[plotsf$PLOT_ID==pltid[x],])
  trs
})

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

#############################
# Bipartite matching
#############################

### Run matching
li.match <- mclapply(li.runid,
                     FUN=bipart.match3,
                     lasset=testli,
                     obset=stems.in.plots,
                     plotdir=file.path('/global', 'scratch', 'users', 'worsham', 'itc_results', 'figs', 'li_itc_figs'),
                     mc.cores = getOption("mc.cores", length(workerNodes)-4)
                     )

# Reformat results
names(li.match) <- names(testli)
li.match <- data.frame(do.call('rbind', li.match))
li.match$quad <- unlist(lapply(strsplit(rownames(li.match), '_'), '[',1))
li.match$paramset <- unlist(lapply(strsplit(rownames(li.match), '_'), '[', 2))


#############################
# Write results
#############################

write.csv(li.match,
          file.path(config$extdata$scratch,
                    'itc_results',
                    'li_itc_results.csv'),
          row.names=T)
