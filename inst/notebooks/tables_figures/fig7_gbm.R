# Figure 7

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

# Read GBM relative influence data
# TODO: Get file link
download.file('https://drive.usercontent.google.com/download?id=TKTKTK&confirm=true',
              destfile=file.path(tempdir(), 'gbm_relative_influence.csv'),
              method='wget')
gbm.sums <- read.csv(file.path(tempdir(), 'gbm_relative_influence.csv'))
gbm.sums <- read.csv('/Users/hmworsham/Desktop/ER_ForestStructure_RepData/gbm_relative_influence.csv')

# Read local variable names reference table
varnames <- read.csv(file.path(config$data$int, 'explainer_names_table.csv'),
                     row.names=1)
varnames[varnames$label=='\xc6SWE', 'label'] <- '\u0394SWE' # Coerce UTF delta symbol

########################################
# Generate GBM performance summaries
########################################

# Join to varnames reference table
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

######################
# Plot
######################

# Group by category
gbm.summaries.cat <- gbm.sums %>%
  mutate(category = factor(category, levels=c('Climate', 'Topography',
                                              'Substrate')),
         label = factor(label,
                        levels=c('AET', 'CWD', 'SWE', '\u0394SWE',
                                 'Curvature', 'Elevation', 'Heat load',
                                 'TPI', 'TWI',
                                 'AWC', 'CEC', 'ksat',
                                 'Organic matter', 'pH', 'Silt content',
                                 'Geology'),
                        labels=c('AET', 'CWD', 'SWE', '\u0394SWE',
                                 'Curvature', 'Elevation', 'Heat load',
                                 'TPI', 'TWI',
                                 'AWC', 'CEC', expression(k['sat']),
                                 'Organic matter', 'pH', 'Silt content',
                                 'Geology')),
         var = factor(var)) %>%
  arrange(category, label)

soil.labs <- c('AWC', 'CEC', expression(k['sat']),
               'Organic matter', 'pH', 'Silt content')


# Panel A: Plot importance for each structure metric

p1 <- ggplot(gbm.summaries.cat, aes(x=Model, y=rel.inf, fill=interaction(category, label, sep=': '))) +
  geom_col(position='stack') +
  scale_fill_manual(limits=interaction(gbm.summaries.cat$category, gbm.summaries.cat$label, sep=': '),
                    values=gbm.summaries.cat$pdcolors, name=NULL) +
  #scale_fill_manual(values=unname(nifty::icolors('pantone')[c(1,4,5,7)])) +
  #scale_fill_manual(values = c(mypal(4), mypal2(4), mypal3(4), mypal4(1))) +
  labs(x='Response', y='Relative influence (%)') +
  ggthemes::theme_calc(base_size=8,
                       base_family='Arial') +
  guides(fill = guide_legend(byrow = TRUE)) +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        legend.position='none',
        # legend.direction = 'horizontal',
        legend.spacing.y = unit(0.1, 'mm'),
        # legend.text = element_text(size=rel(0.7)),
        legend.key.size = unit(0.8, 'lines'),
        legend.margin = margin(r=1, l=0, t=10, b=10),
        plot.background = element_rect(fill=NA, color = NA, linewidth = 0)
  )

# Build legend separately
gbm.sum.tbl <- split(gbm.summaries.cat, gbm.summaries.cat$category)

gbm.col.tbl <- lapply(gbm.sum.tbl, \(x) {
  x %>%
    select(label, pdcolors) %>%
    deframe()
})

p1.leg <- ggplot(mapping=aes(x=Model, y=rel.inf)) +
  geom_col(data=gbm.sum.tbl$Climate, position='stack', aes(fill=label)) +
  scale_fill_manual(values=gbm.col.tbl$Climate,
                    name='Climate',
                    guide=guide_legend(order=1)) +
  ggnewscale::new_scale_fill() +
  geom_col(data=gbm.sum.tbl$Topography, position='stack', aes(fill=label)) +
  scale_fill_manual(values=gbm.col.tbl$Topography,
                    name='Topography',
                    guide=guide_legend(order=2)) +
  ggnewscale::new_scale_fill() +
  geom_col(data=gbm.sum.tbl$Substrate, position='stack', aes(fill=label)) +
  scale_fill_manual(values=gbm.col.tbl$Substrate,
                    name='Substrate',
                    guide=guide_legend(order=3),
                    labels=c(soil.labs, 'Geology')) +
  # ggnewscale::new_scale_fill() +
  # geom_col(data=gbm.sum.tbl$Geology, position='stack', aes(fill=label)) +
  # scale_fill_manual(values=gbm.col.tbl$Geology,
  #                   name='Geology',
  #                   guide=guide_legend(order=4)) +
  theme(legend.position = "left",
        legend.box = "vertical",
        legend.direction='vertical',
        legend.box.just = "left",
        legend.title = element_text(size=rel(1))) +
  labs(x='Model', y='Relative influence (%)') +
  ggthemes::theme_calc(base_size=7,
                       base_family='Arial') +
  guides(fill = guide_legend(byrow = TRUE)) +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        legend.position='left',
        legend.spacing.y = unit(0.1, 'mm'),
        legend.text = element_text(hjust=0),
        legend.key.size = unit(0.8, 'lines'),
        legend.margin = margin(r=1, l=1, t=2, b=5),
        legend.title = element_text(size=rel(1),
                                    face='bold'),
        plot.background = element_rect(color = NA)
  )

p1.leg <- grab.legend(p1.leg)

# Panel B: Top 5 faceted by full-forest model

# Function for ordering within categories
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

# Function for assigning categorically ordered labels on x-axis
scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

gbm.summaries.5.all <- gbm.summaries.5[!gbm.summaries.5$Model %in%
                                         c('Fir density', 'Spruce density',
                                           'Pine density'),]
# Plot panel B
p2 <- ggplot(gbm.summaries.5.all, aes(x=reorder_within(label, rel.inf, Model), y=rel.inf,
                                      fill=label)) +
  geom_col() +
  scale_x_reordered() +
  coord_flip() +
  scale_fill_manual(limits=gbm.summaries.5$label, values=gbm.summaries.5$pdcolors,
                    guide='none') +
  labs(x='Abiotic explanatory variable', y='Relative influence (%)') +
  facet_wrap(~Model, scales='free_y') +
  ggthemes::theme_calc(base_size=8,
                       base_family='Arial') +
  theme(plot.background=element_rect(fill=NA, color=NA, linewidth=0))

# Panel C: Top 5 faceted by model for species densities
gbm.summaries.5.spp <- gbm.summaries.5[gbm.summaries.5$Model %in%
                                         c('Fir density', 'Spruce density',
                                           'Pine density'),]

# Plot panel C
p3 <- ggplot(gbm.summaries.5.spp, aes(x=reorder_within(label, rel.inf, Model),
                                      y=rel.inf, fill=label)) +
  geom_col() +
  scale_x_reordered() +
  coord_flip() +
  scale_fill_manual(limits=gbm.summaries.5$label, values=gbm.summaries.5$pdcolors,
                    guide='none') +
  labs(x='Abiotic explanatory variable', y='Relative influence (%)') +
  facet_wrap(~Model, scales='free_y') +
  ggthemes::theme_calc(base_size=8,
                       base_family='Arial') +
  theme(plot.background=element_rect(fill=NA, color=NA, linewidth=0))

################################
# Assemble plot grid and write
################################

# Open device
cairo_pdf(file.path('inst', 'ms', 'figures', 'Fig7.pdf'),
          width=190/25.4, height=120/25.4, onefile=T,
          family='Arial', bg='white')

# Set up grid
grid.newpage()

# Panel A
vp1 <- viewport(x = 0, y = 0,
                height = 1, width = 0.12,
                just = c("left", "bottom"),
                name = "leg1")
pushViewport(vp1)
grid.draw(p1.leg)
grid.text(expression(bold('(A)')), x=0.04, y=0.96, just=c('left', 'bottom'))

upViewport(1)
vp2 <- viewport(x=0.12, y=0,
                height=1, width=0.38,
                just = c('left', 'bottom'),
                name='p1')
pushViewport(vp2)
print(p1, newpage=F)

# Panel B
upViewport(1)
vp3 <- viewport(x=1, y=0.4,
                height=0.6, width=0.5,
                just=c('right', 'bottom'),
                name='p2')
pushViewport(vp3)
print(p2, newpage=F)
grid.text(expression(bold('(B)')), x=0.01, y=0.93, just=c('left', 'bottom'))

# Panel C
upViewport(1)
vp4 <- viewport(x=1, y=0,
                height=0.4, width=0.5,
                just=c('right', 'bottom'),
                name='p3')
pushViewport(vp4)
print(p3, newpage=F)
grid.text(expression(bold('(C)')), x=0.01, y=0.90, just=c('left', 'bottom'))

upViewport(1)
vp5 <- viewport(x=0, y=0,
                height=1, width=0.5,
                just=c('left', 'bottom'),
                name='vp4')
pushViewport(vp5)
#grid.rect(gp=gpar(fill=NA))

dev.off()
