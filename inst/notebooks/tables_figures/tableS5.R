# SI tables and figures for East River forest structure manuscript

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
    fontsize(size=8, part='all') %>%
    colformat_md(part='all', metadata=list())

  # Adjust widths manually
  # ft_out <- width(ft_out,
  #                 width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))

  return(ft_out)
}


############
# Table S5
############

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

# Best GBMs in tuning
gbm.best <- bind_rows(sapply(gbms, '[', 'bestTune', simplify=T), .id='Response') %>%
  mutate(Response=str_replace_all(Response, '.bestTune', ''),
         ord = factor(c(6,1,5,4,2,3,8,7))) %>%
  arrange(ord) %>%
  select(-ord)

names(gbm.best) <- c('Response', 'N trees', 'Interaction depth',
                     'Shrinkage', 'Min obs. in node')

tbls5 <- make.ft(flextable(gbm.best))
save_as_image(tbls5, file.path('inst', 'ms', 'tables', 'tbls5.svg'))
