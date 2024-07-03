# SI tables and figures for East River forest structure manuscript

#####################
## Workspace setup
#####################

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load packages and local functions
devtools::load_all()
load.pkgs(config$pkgs)

# Configure drive auth
drive_auth(path=config$drivesa)

#####################
# Fig S1
#####################

# Generates figures to depict the distribution of conifer forest plots along
# defined geophysical gradients

# Ingest 2020 Kueppers plot characteristics CSVs
tmpfile <- drive_download(
  as_id(config$extdata$siteindex),
  type='csv',
  tempfile())$local_path
siteinfo <- read.csv(tmpfile)

# Select variables of interest from 2021 site info
topos <- siteinfo[c(
  'Location_ID', 'Established', 'Within_SDP_Boundary', 'Sensors',
  'Cored', 'Elevation_m', 'Slope', 'Aspect', 'Heat_Load',
  'Folded_Aspect_205', 'Southness_205', 'TWI_100', 'TWI_1000', 'TPI_1000'
)]

# Make df of topo variables
topos <- as.data.frame(topos)

# Specify which plots to exclude
#outs <- c()
outs <- topos$Location_ID[grepl('XX|BME3|NWS1', topos$Location_ID)]
topos <- topos[!topos$Location_ID %in% outs, ]

# Specify vars to include
topos.no <- c('Aspect',
              'Southness_205',
              'TWI_1000',
              'TPI_2000')
kplots <- topos[,!names(topos) %in% topos.no]

# Specify panel labels
var.labs <- c(Elevation_m='Elevation (m. a. s. l.)',
              Folded_Aspect_205='Folded aspect (unitless)',
              Heat_Load='Heat load (unitless)',
              Slope='Slope (º)',
              TPI_1000='TPI (unitless)',
              TWI_100='TWI (unitless)')

# Gather to long format
kplots_long <- kplots %>%
  pivot_longer(cols = Elevation_m:TPI_1000,
               names_to = 'variable') %>%
  arrange(variable, Location_ID)

kplots_long <- kplots_long %>%
  mutate(Elevation = as.numeric(
    flatten(
      rep(kplots_long[kplots_long$variable == 'Elevation_m', 'value'],
          length(unique(kplots_long$variable))
      )))) %>%
  arrange(variable, value) %>%
  group_by(variable) %>%
  mutate(Order = 1:17,
         LIO = paste(Order, Location_ID, sep='_')) %>%
  ungroup()

locids <- function(x) sub("[^_]*_","", x)

# Set up facet grid with all variables
varsgrid <- ggplot(kplots_long, aes(x = reorder(LIO, Order), y = value)) +
  geom_point(size = 3, fill='grey50', shape = 21) +
  scale_fill_viridis(name = 'Elevation', discrete=T) +
  scale_x_discrete(labels = locids) +
  geom_smooth(
    aes(x = Order, y = value),
    method = 'lm',
    se = FALSE,
    color = 'black'
  ) +
  ggpmisc::stat_poly_eq(
    aes(
      x = Order,
      y = value,
      label = paste(after_stat(eq.label), sep = "~~~")
    ),
    label.x.npc = "right",
    label.y.npc = 0.15,
    eq.with.lhs = "italic(hat(y))~`=`~",
    eq.x.rhs = "~italic(x)",
    formula = y ~ x,
    parse = TRUE,
    size = 3
  ) +
  ggpmisc::stat_poly_eq(
    aes(
      x = Order,
      y = value,
      label = paste(after_stat(rr.label), sep = "~~")
    ),
    label.x.npc = "right",
    label.y.npc = "bottom",
    formula = y ~ x,
    parse = TRUE,
    size = 3
  ) +
  # scale_x_discrete(labels=kplots_long$Location_ID) +
  facet_wrap( ~ variable, scales = 'free',
              labeller= labeller(variable=var.labs),
              strip.position = 'left') +
  labs(x = 'Site', y = NULL) +
  guides(color = 'none', fill = 'none', size = 'none') +
  ggthemes::theme_calc(base_size=8,
                       base_family='Arial') +
  theme(
    aspect.ratio=1,
    axis.text.x = element_text(
      #size = 9,
      angle = 60,
      hjust = 1,
    ),
    strip.background=element_blank(),
    strip.placement='outside'
  )

topo.grads <- lapply(names(var.labs), \(i) {

  df <- kplots[c('Location_ID', i)] %>%
    arrange(.[[2]]) %>%
    mutate(Order=1:17)

  ggplot(df, aes(x=reorder(Location_ID, Order), y=.data[[i]])) +
    geom_point(size = 3, fill='grey50', shape = 21) +
    scale_x_discrete(labels = locids) +
    geom_smooth(
      aes(x = Order, y = .data[[i]]),
      method = 'lm',
      se = FALSE,
      color = 'black'
    ) +
    ggpmisc::stat_poly_eq(
      aes(
        x = Order,
        y = .data[[i]],
        label = paste(after_stat(eq.label), sep = "~~~")
      ),
      label.x.npc = "right",
      label.y.npc = 0.15,
      eq.with.lhs = "italic(hat(y))~`=`~",
      eq.x.rhs = "~italic(x)",
      formula = y ~ x,
      parse = TRUE,
      size = 3
    ) +
    ggpmisc::stat_poly_eq(
      aes(
        x = Order,
        y = .data[[i]],
        label = paste(after_stat(rr.label), sep = "~~")
      ),
      label.x.npc = "right",
      label.y.npc = "bottom",
      formula = y ~ x,
      parse = TRUE,
      size = 3
    ) +
    labs(x = 'Site', y = var.labs[i]) +
    guides(color = 'none', fill = 'none', size = 'none') +
    ggthemes::theme_calc(base_size=8,
                         base_family='Arial') +
    theme(
      aspect.ratio=1,
      axis.text.x = element_text(
        #size = 9,
        angle = 60,
        hjust = 1,
      )
    )

})

# Write facet grid
cairo_pdf('~/Desktop/FigS1.pdf', width=190/25.4, height=140/25.4, onefile=T,
          family='Arial', bg='white')

design = "
ABC
DEF
"

wrap_plots(topo.grads) +
  patchwork::plot_layout(design=design) +
  plot_annotation(tag_levels = 'A',
                  tag_prefix = '(',
                  tag_suffix = ')') +
  theme(plot.tag = element_text(face = 'bold'))

dev.off()

#####################
## Fig S3
#####################

# Load models
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

# Ingest variable names
varnames <- read.csv(file.path(config$data$raw, 'explainer_names_table.csv'),
                     row.names=1)
varnames[varnames$label=='\xc6SWE', 'label'] <- '\u0394SWE'

# Ingest unscaled variables

###############################
# Ingest qualitative table data
###############################
download.file('https://drive.google.com/uc?export=download&id=1lQGjd1ZPV6sxmHLUWosu3eZv55cfnxKq&usp=drive_fs',
              destfile=file.path(tempdir(), 'all_variables_unscaled.csv'),
              method='wget')
vars <- read.csv(file.path(tempdir(), 'all_variables_unscaled.csv'))

# Slice model frames for all GAMs and collapse to one dataframe
pe.dfs <- lapply(seq_along(gams), \(i) {
  slices <- lapply(target.vars, pe.slice, gams[[i]])
  slices <- slices[!unlist(lapply(slices, is.null))]
  slices.df <- reduce(slices, dplyr::left_join, by='v')
  #pv <- gbm.summaries.5[gbm.summaries.5$Model==names(gams)[i],]$var
  #pe.df <- pe.pivot(slices.df, pv)
  pe.df <- pe.pivot(slices.df, target.vars)
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
                                 'AWC', 'CEC', 'ksat',
                                 'Organic matter', 'pH', 'Silt content',
                                 'Geology', 'X', 'Y')),
         var=factor(var)
  ) %>%
  arrange(category,label)

peplot.dfs <- peplot.df %>%
  group_split(Model)

lblr <- c(`Basal area`=bquote('Basal area ('*m^2~ha^-1*')'),
          `Height 95P`='Height 95P (m)',
          `Height skew`='Height skew',
          QMD='QMD (cm)',
          `Total density`=bquote('Total density (stems'~ha^-1*')'),
          `Fir density`=bquote('Fir density (stems'~ha^-1*')'),
          `Spruce density`=bquote('Spruce density (stems'~ha^-1*')'),
          `Pine density`=bquote('Pine density (stems'~ha^-1*')'))

peplots <- lapply(1:8, \(i) {
  p <- peplot.dfs[[i]]
  ggplot(p) +
    geom_line(aes(x=v, y=fit, color=label), linewidth=1) +
    geom_line(aes(x=v, y=u95, color=label), linetype=2, linewidth=0.4) +
    geom_line(aes(x=v, y=l95, color=label), linetype=2, linewidth=0.4) +
    scale_color_manual(limits=p$label,
                       values=p$pdcolors,
                       name=NULL) +
    scale_y_continuous(limits=c(0,max(p$fit))) +
    facet_wrap(~label, scales='fixed') +
    ggthemes::theme_calc(base_size=8,
                         base_family='Arial') +
    labs(x=NULL,
         y=lblr[names(lblr)==p$Model[1]][[1]],
         title=p$Model[[1]],
         tag=paste0('(', LETTERS[i], ')')) +
    theme(legend.position='none',
          aspect.ratio=1,
          plot.background=element_rect(fill=NA, color=NA, linewidth=0),
          plot.title=element_text(hjust=0.5),
          plot.tag=element_text(face='bold')
    )
})

# Write facet grid
cairo_pdf('~/Desktop/FigS3.pdf', width=140/25.4, height=140/25.4,
          family='Arial', bg='white', onefile = T)

print(peplots)

dev.off()
