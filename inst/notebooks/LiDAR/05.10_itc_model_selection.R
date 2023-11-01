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
opt.res <- itc.res[itc.res$model=='ls' & itc.res$paramset==7,]

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

# Generate summary stats for best-performing run of each model
itc.best <- itc.means %>%
  filter(f==max(f)) %>%
  group_by(model) %>%
  filter(row_number()==1) %>%
  mutate(best=paste0(model, paramset))

best.res <- itc.res %>%
  mutate(modp=paste0(model,paramset)) %>%
  filter(modp %in% itc.best$best)

best.mean <- best.res %>%
  group_by(model) %>%
  summarise(across(nobstrees:npredtrees, \(x) sum(x, na.rm=T)),
            across(c(ext.rt, match.rt, accuracy:f), \(x) round(sqrt(mean(x^2, na.rm=T)),2)),
            .groups='drop'
  ) %>%
  mutate(ext.rt = round(npredtrees/nobstrees,2),
         model=c('Li (2012)',
                 'LMF-auto',
                 'LMF fixed window',
                 'LMF variable window',
                 'LayerStacking',
                 'MultiCHM',
                 'PTrees',
                 'Watershed'))
names(best.mean) <- c('Model', names(opt.mean))

# Flextables
opt.mean.ft <- flextable(opt.mean)
best.mean.ft <- flextable(best.mean)

opt.mean.ft
best.mean.ft
