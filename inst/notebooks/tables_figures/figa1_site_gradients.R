# SI tables and figures for East River forest structure manuscript

#####################
## Workspace setup
#####################

# Load config
config <- config::get(file=file.path('ft_repro_config.yml'))

# Load packages and local functions
devtools::load_all()
load.pkgs(config$pkgs)

#####################
# Fig A1
#####################

# Generates figures to depict the distribution of conifer forest plots along
# defined geophysical gradients

# Ingest Kueppers plot characteristics csv
site.index <- read.csv(file.path(config$data$raw, 'site_index.csv'))

# Select variables of interest from site index
topos <- site.index[c(
  'Location_ID', 'Established', 'Within_SDP_Boundary', 'Sensors',
  'Cored', 'Elevation_m', 'Slope', 'Aspect', 'Heat_Load',
  'Folded_Aspect_205', 'Southness_205', 'TWI_100', 'TWI_1000', 'TPI_1000'
)]

# Make df of topo variables
topos <- as.data.frame(topos)

# Specify which plots to exclude
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
cairo_pdf(file.path('inst', 'ms', 'figures', 'Figure_A1.pdf'),
          width=190/25.4, height=140/25.4, onefile=T,
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
