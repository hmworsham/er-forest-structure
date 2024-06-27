# Table 4

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
    colformat_md(part='all')

  # Adjust widths manually
  # ft_out <- width(ft_out,
  #                 width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))

  return(ft_out)
}

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

names(best.mean) <- c('Model',
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
best.mean <- best.mean[c(5, 1:4, 6:8),]

# Flextables
tbl4 <- make.ft(flextable(best.mean) %>%
                  bold(i=1))
save_as_image(tbl4, file.path('inst', 'ms', 'tables', 'tbl4.svg'))
