# Script for writing out images of forest structure and abiotic explanatory variables

#############################
# Set up working environment
#############################

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

source(file.path('~', 'Repos', 'er', 'er-forest-structure', 'inst', 'notebooks', 'regressions', '01.00_stats_ingest_data.R'))


#############################
# Plot forest structure
#############################
par(mfrow=c(3,2),
    mar=c(1,1,1,1)+.5)
for(i in seq_along(response)) {
  plot(response[[i]], col=viridis(n=20, option=LETTERS[i]),
  title(names(response)[i], line=2, cex=3),
  cex.main=3)
  }

#############################
# Plot explainers
#############################
opar <- par()
par(mfcol=c(5,7), mar=rep(1,4))
for(i in seq_along(explainers)) {
  opt <- rep(LETTERS[1:6],5)
  plot(explainers[[i]], col=viridis(10, option=opt[i], direction=-1), main=names(explainers[[i]]), asp=1)
}
par(opar)
