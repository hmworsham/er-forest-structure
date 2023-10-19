# ITC model selection

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

# Read itc optimization results
itc.res.files <- list.files('/global/scratch/users/worsham/itc_results', pattern='csv', full.names=T)
itc.res <- lapply(itc.res.files, read.csv)

# Bind into one dataframe
itc.res <- bind_rows(itc.res, .id='mi')

# Get model names and parameter set IDs and assign to columns
mod.names <- unlist(lapply(str_split(itc.res.files, '/|_'), '[', 8))
mod.names <- data.frame(mi=as.character(1:8), model=mod.names)
itc.res <- left_join(itc.res, mod.names, by='mi')
itc.res$paramset <- as.integer(str_replace(itc.res$paramset, 'p', ''))

# Check number of unique parameter combinations per model
itc.pcombs <- itc.res %>%
  group_by(model) %>%
  summarise(n=n_distinct(paramset))

# Check number of models run at each site
itc.modn <- itc.res %>%
  group_by(quad) %>%
  summarise(n=n_distinct(model))

# Check total number of iterations run at each site
itc.modruns <- itc.res %>%
  group_by(model) %>%
  summarise(n=n())

# Check total number of parameter combinations run at each site
itc.modsiten <- itc.res %>%
  group_by(model, quad) %>%
  summarise(n=n_distinct(paramset))

# Get means
itc.means <- itc.res %>%
  group_by(model, paramset) %>%
  #summarise(across(nobstrees:loss, \(x) mean(x, na.rm=T)))
  summarise(across(nobstrees:loss, \(x) sqrt(mean(x^2, na.rm=T))))

# Find model that maximizes f score (or minimizes loss)
optmod <- itc.means[which.max(itc.means$f),]
optmod
#optmod <- itc.means[which.min(itc.means$loss),]


ggplot(itc.res, aes(x=model, y=f)) +
  geom_boxplot() +
  facet_wrap(~quad)

chkmod <- itc.res[itc.res$model=='ls' & itc.res$paramset==12,]

ggplot(chkmod, aes(x=quad, y=f)) +
  geom_point()


# Generate summary stats for optimal model
opt.res <- itc.res[itc.res$model=='ls' & itc.res$paramset==11,]

opt.sum <- summary(opt.res)
opt.sum
opt.mean <- opt.res %>%
  summarise(across(nobstrees:loss, \(x) mean(x, na.rm=T)))

opt.sd <- opt.res %>%
  summarise(across(nobstrees:loss, \(x) sd(x, na.rm=T)))

# Generate summary stats for best-performing run of each model
View(itc.means %>%
  filter(f==max(f)))
