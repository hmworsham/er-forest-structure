# Generate tables for East River forest structure manuscript

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

##########
# Table 1
##########

# Descriptions of variables
tbl1 <- read.csv(file.path(config$data$raw, 'variable_definitions.csv'), stringsAsFactors = F,
                 na = "", encoding='UTF-8')

tbl1 <- make.ft(flextable(tbl1))
save_as_image(tbl1, file.path('inst', 'ms', 'tables', 'tbl1.svg'))

###########
# Table 2
##########

tbl2 <- read.csv(file.path(config$data$raw, 'field_methods.csv'), stringsAsFactors = F,
                 na = "", encoding='UTF-8')

tbl2 <- make.ft(flextable(tbl2))
save_as_image(tbl2, file.path('inst', 'ms', 'tables', 'tbl2.svg'))

############
# Table 3
###########
tbl3 <- read.csv(file.path(config$data$raw, 'match_criteria.csv'), stringsAsFactors = F,
                 na = "", encoding='UTF-8', check.names = F)

tbl3 <- make.ft(flextable(tbl3))
save_as_image(tbl3, file.path('inst', 'ms', 'tables', 'tbl3.svg'))

##########
# Table 5
##########

tbl5 <- read.csv(file.path(config$data$raw, 'layerstacking_params.csv'),
                 stringsAsFactors = F, na = "", encoding='UTF-8', check.names=F)

tbl5$ID <- paste0('λ', '~', 1:7, '~')

tbl5 <- make.ft(flextable(tbl5))
save_as_image(tbl5, file.path('inst', 'ms', 'tables', 'tbl5.svg'))
