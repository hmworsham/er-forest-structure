# ITC model selection

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

# Read itc optimization results
itc.res.files <- list.files(config$extdata$itc, pattern='csv', full.names=T)
itc.res <- lapply(itc.res.files, read.csv)
itc.res <- bind_rows(itc.res, .id='mi')

# Get model names
mod.names <- unlist(lapply(str_split(itc.res.files, '/|_'), '[', 8))
mod.names <- data.frame(mi=as.character(1:5), model=mod.names)
itc.res <- left_join(itc.res, mod.names, by='mi')
itc.res$paramset <- as.integer(str_replace(itc.res$paramset, 'p', ''))

#
itc.means <- itc.res %>%
  group_by(model, paramset) %>%
  summarise_all(mean, na.rm=T)

# Find model that maximizes f score (or minimizes loss)
optmod <- itc.means[which.max(itc.means$f),]
optmod <- itc.means[which.min(itc.means$loss),]
optmod

# Generate summary stats for optimal model
opt.res <- itc.res[itc.res$model==3 & itc.res$paramset=='p9',]

opt.sum <- summary(opt.res)

opt.mean <- opt.res %>%
  summarise_all(mean)

opt.sd <- opt.res %>%
  summarise_all(sd)

