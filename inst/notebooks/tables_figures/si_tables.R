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
# Ingest qualitative table data
###############################

download.file('https://drive.google.com/uc?export=download&id=12eSdo6XbUqcXf_u7yeyTeZxvetyLZ2j6&usp=drive_fs',
              destfile=file.path(tempdir(), 'table_data.tar.gz'),
              method='wget')
untar(file.path(tempdir(), 'table_data.tar.gz'), exdir=file.path(tempdir(), 'table_data'))

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

############
# Table S1
############

# Descriptions of variables
tbls1 <- read.csv(file.path(config$data$raw, 'variable_definitions.csv'), stringsAsFactors = F,
                 na = "", encoding='UTF-8')

tbls1 <- make.ft(flextable(tbls1))
save_as_image(tbls1, file.path('inst', 'ms', 'tables', 'tbls1.svg'))

############
# Table S2
############

# Descriptions of field methods
tbls2 <- read.csv(file.path(config$data$raw, 'field_methods.csv'), stringsAsFactors = F,
                 na = "", encoding='UTF-8')

tbls2 <- make.ft(flextable(tbls2))
save_as_image(tbls2, file.path('inst', 'ms', 'tables', 'tbls2.svg'))

############
# Table S3
############

# All ITD parameters
tbls3 <- read.csv(file.path(tempdir(), 'table_data', 'itd_algorithms.csv'), stringsAsFactors = F,
                  na = "", encoding='UTF-8', check.names=F)
tbls3 <- make.ft(flextable(tbls3))
save_as_image(tbls3, file.path('inst', 'ms', 'tables', 'tbls3.svg'))


############
# Table S4
###########

# ∆XYZ matching criteria
tbls4 <- read.csv(file.path(config$data$raw, 'match_criteria.csv'), stringsAsFactors = F,
                 na = "", encoding='UTF-8', check.names = F)

tbls4 <- make.ft(flextable(tbls4))
save_as_image(tbls4, file.path('inst', 'ms', 'tables', 'tbls4.svg'))

############
# Table S5
############

# Results for all Layer Stacking parameter permutations

# Read itc optimization results
download.file('https://drive.google.com/uc?export=download&id=1zJUsIbIaxOe1CQH7ny7dD69bNII_kvZI&usp=drive_fs',
              destfile=file.path(tempdir(), 'itd_results.tar.gz'),
              method='wget')
untar(file.path(tempdir(), 'itd_results.tar.gz'), exdir=file.path(tempdir(), 'itd_results'))
itc.res.files <- list.files(file.path(tempdir(), 'itd_results'), full.names=T)
itc.res <- lapply(itc.res.files, read.csv)

# Bind into one dataframe
itc.res <- bind_rows(itc.res, .id='mi')

# Get model names and parameter set IDs and assign to columns
mod.names <- unlist(lapply(str_split(itc.res.files, '/|_'), '[', 12))
mod.names <- data.frame(mi=as.character(1:8), model=mod.names)
itc.res <- left_join(itc.res, mod.names, by='mi')
itc.res$paramset <- as.integer(str_replace(itc.res$paramset, 'p', ''))

# Get means
ls.means <- itc.res %>%
  group_by(model, paramset) %>%
  summarise(across(nobstrees:npredtrees, \(x) sum(x, na.rm=T)),
            across(c(ext.rt, match.rt, accuracy:f), \(x) round(sqrt(mean(x^2, na.rm=T)),2)),
            .groups='drop'
  ) %>%
  mutate(ext.rt = round(npredtrees/nobstrees,2)) %>%
  filter(model=='ls')

names(ls.means) <- c('Model',
                     'Parameter set',
                      'N reference trees',
                      'N detected trees',
                      'Extraction rate',
                      'Match rate',
                      'Overall accuracy',
                      'Omission rate',
                      'Commission rate',
                      'RMS ~∆XY~',
                      'RMS ~∆Z~',
                      'RMS ~∆XYZ~',
                      'Precision',
                      'Recall',
                      'F'
)
ls.means <- ls.means[,c(2:length(ls.means))]

tbls5 <- make.ft(flextable(ls.means))
save_as_image(tbls5, file.path('inst', 'ms', 'tables', 'tbls5.svg'))

##########
# Table S6
##########

# Layer Stacking parameters and optimal values
tbls6 <- read.csv(file.path(config$data$raw, 'layerstacking_params.csv'),
                 stringsAsFactors = F, na = "", encoding='UTF-8', check.names=F)

tbls6$ID <- paste0('λ', '~', 1:7, '~')

tbls6 <- make.ft(flextable(tbls6))
save_as_image(tbls6, file.path('inst', 'ms', 'tables', 'tbls6.svg'))

############
# Table S7
############

# GAM variables

# Ingest variable table
tbls7.in <- read.csv(file.path(tempdir(), 'table_data', 'gam_specs.csv'), stringsAsFactors = F,
                  na = "", encoding='UTF-8', check.names=F)

# Ingest stored GAMs
gams.rda <- list.files('models', pattern='gam', full.names=T)
gams <- lapply(gams.rda, readRDS)

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

# Reorder GAMs for reporting
gams <- gams[c(2,5,6,4,3,1,8,7)]

# Check basis functions, basis dimensions, and EDF
basis.check <- function(b, k.sample = 5000, k.rep = 200) {
  mgcv:::k.check(b, subsample = k.sample, n.rep = k.rep)
}

basis <- lapply(gams, basis.check)

basis <- lapply(basis, \(x) {
  x <- data.frame(x, check.names=F) %>%
    rownames_to_column(var='Call')
})

names(basis) <- names(gams)

basis.df <- do.call(cbind, basis)
names(basis.df)[1] <- 'Call'

tbls7 <- left_join(tbls7.in, basis.df, by='Call') %>%
  select(-contains(c('.Call', 'k-index', 'p-value'))) %>%
  mutate_at(vars(contains('edf')), ~round(.,2))

tbls7 <- make.ft(flextable(tbls7) %>%
                   ftExtra::span_header(sep='\\.') %>%
                   flextable::theme_booktabs() %>%
                   flextable::align(i=1, align='center', part='header'))

save_as_image(tbls7, file.path('inst', 'ms', 'tables', 'tbls7.svg'))

############
# Table S8
############

# Optimal GBM tuning parameters

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

tbls8 <- make.ft(flextable(gbm.best))
save_as_image(tbls8, file.path('inst', 'ms', 'tables', 'tbls8.svg'))

############
# Table SX1
############

# tblsx1 <- read.csv(file.path(tempdir(), 'table_data', 'all_variables.csv'), stringsAsFactors = F,
#                   na = "", encoding='UTF-8', check.names=F)
#
# tblsx1 <- make.ft(flextable(tbls4))
# save_as_image(tblsx1, file.path('inst', 'ms', 'tables', 'tblsx1.svg'))

############
# Table SX2
############
# Summary stats by species for field inventory

# full_unq_inv <- read.csv(file.path('~', 'Repos', 'er', 'er-forest-inventory', 'data', 'processed', 'census1_collated', 'EastRiver_Census1_Data_Collated.csv'))
# full_unq_inv <- full_unq_inv[full_unq_inv$Status %in% c('L', 'Live', 'LIVE'),]
# full_unq_inv <- full_unq_inv[!full_unq_inv$Site_Name %in% c('XX-CAR1',
#                                                             'XX-CAR2',
#                                                             'XX-PLN1',
#                                                             'XX-PLN2'),]
# full_unq_inv <- full_unq_inv[!full_unq_inv$Sp_Code %in% c(NA, 'UNKN'),]
# tblsx2 <- full_unq_inv %>%
#         group_by(Sp_Code) %>%
#         summarise('N'=n(),
#                   'Median height (m)'=round(median(Height_Avg_M, na.rm=T),0),
#                   'Median DBH (cm)'=round(median(DBH_Avg_CM, na.rm=T),1),
#                   'Stem Density (stems ha^-1)'=round(n()/2.72,0),
#                   'Basal area (m^2 ha^-1)'=round(sum(pi*(DBH_Avg_CM/2)^2, na.rm=T)/2.72*(10^-4),1))
#
# tblsx2 <- make.ft(flextable(tblsx2))

############
# Table SX3
############

# Summary statistics by plot for trees observed in field census.
# tblsx3 <- full_unq_inv %>%
#         group_by(Site_Name) %>%
#         summarise('N'=n(),
#                   'N\ species'=length(unique(Sp_Code)),
#                   'Median height (m)'=round(median(Height_Avg_M, na.rm=T),0),
#                   'Median DBH (cm)'=round(median(DBH_Avg_CM, na.rm=T),1),
#                   'Stem Density'=round(n()/.16,0),
#                   'Basal area (m^2 ha^-1)'=round((sum(pi*(DBH_Avg_CM/2)^2, na.rm=T)/.16)*(10^-4),1))
#
# tblsx3 <- make.ft(flextable(tblsx3))
