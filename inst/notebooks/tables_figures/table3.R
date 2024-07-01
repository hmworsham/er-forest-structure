# Table 7

#####################
## Workspace setup
#####################

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

###############################
# Define flextable formatting
###############################

make.ft <- function(ft, pgwidth = 6.5){

  ft_out <- ft %>%
    set_table_properties(layout='autofit',
                         opts_word = list(split=F,
                                          keep_with_next=T)) %>%
    font(fontname='Times New Roman',
         part='all') %>%
    fontsize(size=10, part='all') %>%
    colformat_md(part='all', metadata=list())

  # Adjust widths manually
  # ft_out <- width(ft_out,
  #                 width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))

  return(ft_out)
}

###############
# Data ingest
###############

# Ingest stored GAM objects
gams.rda <- list.files('models', pattern='gam', full.names=T)
gams <- lapply(gams.rda, readRDS)

# Clean up and append names
gam.names <- names(gams) <- tools::file_path_sans_ext(basename(gams.rda))
gam.names.new <- data.frame('o'=gam.names,
                            'n'=c('Fir density (stems ha^–1^)',
                                  'Basal area (m^2^ ha^–1^)',
                                  'Total density (stems ha ^–1^)',
                                  'QMD (cm)',
                                  'Height 95P (m)',
                                  'Height skew (unitless)',
                                  'Pine density (stems ha^–1^)',
                                  'Spruce density (stems ha^–1^)'))

x1 <- match(names(gams), gam.names.new$o)
names(gams) <- gam.names.new$n[x1]

# Summaries
gam.sum <- lapply(gams, summary)
gam.pde <- lapply(gams, \(x) return(summary(x)$dev.expl))

# Make table
gam.perf.df <- data.frame(cbind('Response'=names(gams),
                                'PDE'=unlist(gam.pde)),
                          check.names=F) %>%
  mutate(PDE=round(as.numeric(PDE)*100,1))

# Ingest stored GBMs
gbms.rda <- list.files('models', pattern='gbm', full.names=T)
gbms <- lapply(gbms.rda, readRDS)

# Clean up and append names
gbm.names <- names(gbms) <- tools::file_path_sans_ext(basename(gbms.rda))
gbm.names.new <- data.frame('o'=gbm.names,
                            'n'=c('Fir density (stems ha^–1^)',
                                  'Basal area (m^2^ ha^–1^)',
                                  'Total density (stems ha ^–1^)',
                                  'QMD (cm)',
                                  'Height 95P (m)',
                                  'Height skew (unitless)',
                                  'Pine density (stems ha^–1^)',
                                  'Spruce density (stems ha^–1^)'))
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

tbl3 <- gbm.perf.df %>%
  left_join(gam.perf.df, by='Response') %>%
  rename(`Training error`='Train error',
         `CV error`='CV error',
         `PDE`='PDE'
  ) %>%
  mutate(across(`Training error`:`CV error`, \(x) round(x,2)),
         ord = factor(c(6,1,5,4,2,3,8,7))
         ) %>%
  arrange(ord) %>%
  select(-ord)

tbl3 <- make.ft(flextable(tbl3) %>%
                  add_header_row(values=c('', 'GBM','GAM'),
                                 colwidths=c(1,2,1)) %>%
                  flextable::theme_booktabs() %>%
                  flextable::align(i=1, align='center', part='header') %>%
                  flextable::hline(i=1, j=2:3, part='header') %>%
                  flextable::hline(i=1, j=4, part='header')
)

save_as_image(tbl3, file.path('inst', 'ms', 'tables', 'tbl3.svg'))
