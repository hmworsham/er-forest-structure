# Figure 8

#############################
# Set up working environment
#############################

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

#############################
# Ingest data
#############################

# Read saved models and prep data
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

# Read GBM relative influence data
# TODO: Get file link
download.file('https://drive.usercontent.google.com/download?id=TKTKTK&confirm=true',
              destfile=file.path(tempdir(), 'gbm_relative_influence.csv'),
              method='wget')
gbm.sums <- read.csv(file.path(tempdir(), 'gbm_relative_influence.csv'))

# Read local variable names reference table
varnames <- read.csv(file.path(config$data$int, 'explainer_names_table.csv'),
                     row.names=1)
varnames[varnames$label=='\xc6SWE', 'label'] <- '\u0394SWE' # Coerce UTF delta symbol

#############################################
# ID 5 most influential explainers from GBMs
#############################################

# Join gbm.sums to varnames reference table
gbm.sums <- left_join(gbm.sums, varnames, by=c('var'='varnames'))

# Model name as factor for ordering
gbm.sums <- gbm.sums %>%
  mutate(Model=factor(Model, levels=c('Basal area',
                                      'Height 95P',
                                      'Height skew',
                                      'QMD',
                                      'Total density',
                                      'Fir density',
                                      'Spruce density',
                                      'Pine density')))

# Subset to top 5 most influential variables for each model
gbm.summaries.5 <- gbm.sums %>%
  group_by(Model) %>%
  top_n(5, wt=rel.inf) %>%
  mutate(ranking=rank(rel.inf))

##################################
# Partial-effects
##################################

# Define target variables for partial-effects (i.e., variables included in modframe)
target.vars <- c('heat_load', 'elevation', 'twi', 'tpi', 'curvature', #5
                 'awc', 'cec', 'silt', 'ksat', 'ph', #10
                 'om', 'swe', 'delta_swe', 'cwd', 'aet', #15
                 'geology', 'x', 'y') #18

# Slice the model frames for each response to estimate partial effects
# of each variable, holding all other variables constant at
# median (for continuous) or mode (for factors)
pe.dfs <- lapply(seq_along(gams), \(i) {
  slices <- lapply(target.vars, pe.slice, gams[[i]])
  slices <- slices[!unlist(lapply(slices, is.null))]
  slices.df <- reduce(slices, dplyr::left_join, by='v')
  pv <- gbm.summaries.5[gbm.summaries.5$Model==names(gams)[i],]$var
  pe.df <- pe.pivot(slices.df, pv)
  pe.df
})

# Assign model names to list members from GAM names
names(pe.dfs) <- gam.names.new$n[x1]

# Coerce some variables to factor for specific order
peplot.df <- bind_rows(pe.dfs, .id='Model') %>%
  mutate(Model=factor(Model, levels=c('Basal area',
                                      'Height 95P',
                                      'Height skew',
                                      'QMD',
                                      'Total density',
                                      'Fir density',
                                      'Spruce density',
                                      'Pine density')),
         category=factor(category, levels=c('Climate', 'Topography',
                                            'Soil', 'Geology', 'Spatial')),
         label = factor(label,
                        levels=c('AET', 'CWD', 'SWE', '\u0394SWE',
                                 'Curvature', 'Elevation', 'Heat load',
                                 'TPI', 'TWI',
                                 'AWC', 'CEC', 'Organic matter',
                                 'Silt content','ksat', 'pH',
                                 'Geology', 'X', 'Y'),
                        labels=c('AET', 'CWD', 'SWE', '\u0394SWE',
                                 'Curvature', 'Elevation', 'Heat load',
                                 'TPI', 'TWI',
                                 'AWC', 'CEC', expression(k['sat']),
                                 'Organic matter', 'pH', 'Silt content',
                                 'Geology', 'X', 'Y')),
         var=factor(var)
  ) %>%
  arrange(category,label)

peplot.dfs <- peplot.df %>%
  mutate(Set=case_when(Model %in% c('Fir density', 'Spruce density', 'Pine density') ~ 'Species',
                       T ~ 'Full')) %>%
  group_split(Model)

lblr <- c(`Basal area`=bquote('Basal area ('*m^2~ha^-1*')'),
          `Height 95P`='Height 95P (m)',
          `Height skew`='Height skew',
          QMD='QMD (cm)',
          `Total density`=bquote('Total density (stems'~ha^-1*')'),
          `Fir density`=bquote('Fir density (stems'~ha^-1*')'),
          `Spruce density`=bquote('Spruce density (stems'~ha^-1*')'),
          `Pine density`=bquote('Pine density (stems'~ha^-1*')'))

#########
# Plot
#########

# Plot PE for full-forest structure variables by model
peplots <- lapply(peplot.dfs, \(p) {
  ggplot(p) +
    geom_line(aes(x=v, y=fit, color=label), linewidth=1) +
    geom_line(aes(x=v, y=u95, color=label), linetype=2, linewidth=0.4) +
    geom_line(aes(x=v, y=l95, color=label), linetype=2, linewidth=0.4) +
    scale_color_manual(limits=p$label,
                       values=p$pdcolors,
                       name=NULL) +
    scale_y_continuous(limits=c(0,max(p$fit))) +
    ggthemes::theme_calc(base_size=8,
                         base_family='Arial') +
    labs(x=NULL,
         y=lblr[names(lblr)==p$Model[1]][[1]],
         title=p$Model[[1]]) +
    theme(legend.position='none',
          aspect.ratio=1,
          plot.background=element_rect(fill=NA, color=NA, linewidth=0),
          plot.title=element_text(hjust=0.5)
    )
})

# Assign x-axis label separately
peplot_xaxis <- cowplot::get_plot_component(
  ggplot() +
    labs(x = 'Zero-centered values'), 'xlab-b')

# Produce legend separately
peplot.leg <- grab.legend(
  ggplot(peplot.df, aes(x=v, y=fit, color=label)) +
    geom_line(linewidth=1.2) +
    scale_color_manual(limits=peplot.df$label,
                       values=peplot.df$pdcolors,
                       name=NULL) +
    ggthemes::theme_calc(base_size=8)
  +
    theme(legend.margin=margin(r=1, l=1, t=10, b=10),
          legend.background = element_rect(fill=NA),
          #legend.spacing.y = unit(0.001, 'npc'),
          # legend.text=element_text(size=7),
          legend.key.size = unit(0.025, 'npc'))
)

################################
# Assemble plot grid and write
################################

cairo_pdf(file.path('inst', 'ms', 'figures', 'Fig8.pdf'),
          width=190/25.4, height=190/25.4, onefile=T,
          family='Arial', bg='white')

design = "
ABC
DEF
GHI
#J#
"

pw <- list(peplot.leg, peplots[[1]], peplots[[2]],
           peplots[[3]], peplots[[4]], peplots[[5]],
           peplots[[6]], peplots[[7]], peplots[[8]],
           peplot_xaxis) %>%
  wrap_plots() +
  patchwork::plot_layout(heights=c(20,20,20,1), design=design) +
  plot_annotation(tag_levels = list(c('',
                                      paste0('(', LETTERS[1:8], ')'),
                                      ''))) &
  theme(plot.tag = element_text(face = 'bold'))

pw

dev.off()
