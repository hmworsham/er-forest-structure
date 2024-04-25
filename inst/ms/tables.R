# Generate tables for East River forest structure manuscript

#####################
## Workspace setup
#####################

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

# Configure Drive auth
drive_auth(path=config$drivesa)

############################
# Define flextable formatting
############################

make.ft <- function(ft, pgwidth = 6.5){

  ft_out <- ft %>%
    set_table_properties(layout='autofit',
                         opts_word = list(split=F,
                                          keep_with_next=T)) %>%
    font(fontname='Times New Roman',
         part='all') %>%
    fontsize(size=11, part='all') %>%
    colformat_md()

  # Adjust widths manually
  # ft_out <- width(ft_out,
  #                 width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))

  return(ft_out)
}

##########
# Table 1
##########

# Descriptions of variables
tbl1 <- read.csv('~/Desktop/tbl1.csv', stringsAsFactors = F,
                 na = "", encoding='UTF-8')

tbl1 <- make.ft(flextable(tbl1))

###########
# Table 2
##########

tbl2 <- read.csv('~/Desktop/tbl2.csv', stringsAsFactors = F,
             na = "", encoding='UTF-8')

tbl2 <- make.ft(flextable(tbl2))

############
# Table 3
###########
tbl3 <- read.csv('~/Desktop/tbl3.csv', stringsAsFactors = F,
                 na = "", encoding='UTF-8', check.names = F)

tbl3 <- make.ft(flextable(tbl3))

##########
# Table 4
##########

# Read itc optimization results
itc.res.files <- list.files(file.path(config$data$int, 'itc_results'),
                            pattern='results.csv', full.names=T)
itc.res <- lapply(itc.res.files, read.csv)

# Bind into one dataframe
itc.res <- bind_rows(itc.res, .id='mi')

# Get model names and parameter set IDs and assign to columns
mod.names <- unlist(lapply(str_split(itc.res.files, '/|_'), '[', 6))
mod.names <- data.frame(mi=as.character(1:8), model=mod.names)
itc.res <- left_join(itc.res, mod.names, by='mi')
itc.res$paramset <- as.integer(str_replace(itc.res$paramset, 'p', ''))

# Get means
itc.means <- itc.res %>%
  group_by(model, paramset) %>%
  summarise(across(nobstrees:npredtrees, \(x) sum(x, na.rm=T)),
            across(c(ext.rt, match.rt, accuracy:f), \(x) round(sqrt(mean(x^2, na.rm=T)),2)),
            .groups='drop'
  ) %>%
  mutate(ext.rt = round(npredtrees/nobstrees,2))

# Find model that maximizes f score (or minimizes loss)
optmod <- itc.means[which.max(itc.means$f),]

# Generate summary stats for best-performing run of each model
itc.best <- itc.means %>%
  group_by(model) %>%
  filter(f==max(f)) %>%
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
best.mean <- best.mean[c(5, 1:4, 6:8),]

# Flextables
tbl4 <- make.ft(flextable(best.mean) %>%
                          bold(i=1))

##########
# Table 5
##########

tbl5 <- read.csv('~/Desktop/tbl5.csv', stringsAsFactors = F,
                 na = "", encoding='UTF-8')

tbl5 <- make.ft(flextable(tbl5))

##########
# Table 6
##########



##########
# Table 7
##########

# Ingest stored GAMs
gams.rda <- list.files('models', pattern='gam', full.names=T)
gams <- lapply(gams.rda, readRDS)

# Clean up and append names
gam.names <- names(gams) <- tools::file_path_sans_ext(basename(gams.rda))
gam.names.new <- data.frame('o'=gam.names,
                            'n'=c('Fir density',
                                  'Basal area',
                                  'Total density',
                                  'QMD',
                                  'Height 95P',
                                  'Height skew',
                                  'Pine density',
                                  'Spruce density'))

x1 <- match(names(gams), gam.names.new$o)
names(gams) <- gam.names.new$n[x1]

# Summaries
gam.sum <- lapply(gams, summary)
gam.pde <- lapply(gams, \(x) return(summary(x)$dev.expl))

# Make table
gam.perf.df <- data.frame(cbind('Response'=names(gams),
                                'PDE'=unlist(gam.pde)),
                          check.names=F) %>%
  mutate(PDE=as.numeric(PDE))

# Ingest stored GBMs
gbms.rda <- list.files('models', pattern='gbm', full.names=T)
gbms <- lapply(gbms.rda, readRDS)

# Clean up and append names
gbm.names <- names(gbms) <- tools::file_path_sans_ext(basename(gbms.rda))
gbm.names.new <- data.frame('o'=gbm.names,
                            'n'=c('Fir density',
                                  'Basal area',
                                  'Total density',
                                  'QMD',
                                  'Height 95P',
                                  'Height skew',
                                  'Pine density',
                                  'Spruce density'))
x1 <- match(names(gbms), gbm.names.new$o)
names(gbms) <- gbm.names.new$n[x1]

# Train error
gbm.trainerr <- unlist(lapply(gbms, \(x) {
  trerr <- sqrt(min(x$finalModel$train.error, na.rm=T))
  trerr
}))

# Test error
gbm.testerr <- unlist(lapply(gbms, \(x) {
  testerr <- sqrt(min(x$finalModel$valid.error, na.rm=T))
  testerr
}))

# Cross-validation error
gbm.cverr <- unlist(lapply(gbms, \(x) {
  gbmres <- x$results
  gbmres <- gbmres[order(as.numeric(row.names(gbmres))),]
  gbmres <- gbmres[which.min(gbmres$RMSE),]
  gbmres$RMSE
}))

# Make table
gbm.perf.df <- data.frame(cbind('Response'=names(gbms),
                                'Train error'=gbm.trainerr,
                                'CV error'= gbm.cverr),
                          check.names=F)  %>%
  mutate(across(`Train error`:`CV error`, as.numeric))

tbl7 <- gbm.perf.df %>%
  left_join(gam.perf.df, by='Response') %>%
  rename(`Training error`='Train error',
         `CV error`='CV error',
         `PDE`='PDE'
         ) %>%
  mutate(across(`Training error`:`PDE`, \(x) round(x,2)),
         Response= factor(Response, levels=c('Basal area',
                         'Height 95P',
                         'Height skew',
                         'QMD',
                         'Total density',
                         'Fir density',
                         'Spruce density',
                         'Pine density'))) %>%
  arrange(Response)

tbl7 <- make.ft(flextable(tbl7) %>%
                  add_header_row(values=c('', 'GBM','GAM'),
                                 colwidths=c(1,2,1)) %>%
                  theme_booktabs() %>%
                  align(i=1, align='center', part='header') %>%
                  hline(i=1, j=2:3, part='header') %>%
                  hline(i=1, j=4, part='header')
                )
tbl7

############
# Table S1
############

# Best models in tuning
gbm.best <- bind_rows(sapply(gbms, '[', 'bestTune', simplify=T), .id='Response') %>%
  mutate(Response=str_replace_all(Response, '.bestTune', ''))
names(gbm.best) <- c('Response', 'N trees', 'Interaction depth',
                     'Shrinkage', 'Min obs. in node')

tbls1 <- gbm.best %>%
  mutate(Response=factor(Response,
                         levels=c('Basal area',
                                  'Height 95P',
                                  'Height skew',
                                  'QMD',
                                  'Total density',
                                  'Fir density',
                                  'Spruce density',
                                  'Pine density'))) %>%
  arrange(Response)
