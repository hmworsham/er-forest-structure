# Table 6

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

# Data ingest
download.file('https://drive.google.com/uc?export=download&id=1B45pvtLi7e4bq7A81r8YfKenK1T-CNDK&usp=drive_fs',
              destfile=file.path(tempdir(), 'spp_class_data.csv'),
              method='wget')
spp.class <- read.csv(file.path(tempdir(), 'spp_class_data.csv'), stringsAsFactors = T)

cm.spp <- confusionMatrix(spp.class$Reference, spp.class$Classified)

cm.spp.overall <- cm.spp$overall

data.frame(t(round(cm.spp.overall,2)))[c(2,1,3,4)] %>%
  flextable()

tbl2 <- data.frame(round(cm.spp$byClass,2), check.names=F) %>%
  rownames_to_column(var='Class') %>%
  mutate(Class=factor(c('Fir', 'Pine', 'Spruce'),
                      levels=c('Fir', 'Spruce', 'Pine'))) %>%
  select(Class, Sensitivity, Specificity,
         Precision, Recall, F1, `Detection Rate`,
         `Balanced Accuracy`) %>%
  rename(`Detection rate`=`Detection Rate`,
         `Balanced accuracy`=`Balanced Accuracy`)

tbl2 <- make.ft(flextable(tbl2))
save_as_image(tbl2, file.path('inst', 'ms', 'tables', 'tbl2.svg'))
