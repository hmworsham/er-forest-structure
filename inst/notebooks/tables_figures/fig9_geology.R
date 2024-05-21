# Figure 9

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

# Ingest unscaled variables
download.file('https://drive.usercontent.google.com/download?id=1lQGjd1ZPV6sxmHLUWosu3eZv55cfnxKq&confirm=true',
              destfile=file.path(tempdir(), 'all_variables_unscaled.csv'),
              method='wget')
vars <- read.csv(file.path(tempdir(), 'all_variables_unscaled.csv'))

#############################################
# Clean geology data
#############################################

# Pull model summaries
gam.sum <- lapply(gams, summary)

geol.tbl <- lapply(gam.sum, \(x) {
  gdf <- data.frame(x$p.table) %>%
    rownames_to_column(., 'var') %>%
    mutate(var=str_replace_all(var, 'geology', ''))
  gdf
}) %>%
  bind_rows(.id='Model')

geol.ref <- data.frame('geol.idx'=1:9,
                       'geol.name'=c('Dakota Sandstone',
                                     'Mancos Shale',
                                     'Mesa Verde Formation (Sand/Silt/Coal)',
                                     'Gothic Formation (Sand/Shale)',
                                     'Maroon Formation (Red Sand/Mud/Conglomerate)',
                                     'Glacial Drift',
                                     'Landslide Deposits',
                                     'Middle-Tertiary Granodioritic Laccoliths',
                                     'Wasatch Formation (Claystone-Shale)'),
                       'geol.code'=c('KJde',
                                     'Km',
                                     'Kmv',
                                     'Pm',
                                     'PPm',
                                     'Qd',
                                     'Ql',
                                     'Tmi',
                                     'Two'))

geol.ct <- left_join(vars, geol.ref, by=c('geology'='geol.idx')) %>%
  group_by(geol.code) %>%
  summarise(n=n())

geol.tbl <- left_join(geol.tbl, geol.ref, by=c('var'='geol.name')) %>%
  left_join(geol.ct, by='geol.code')

geol.tbl.sig <- geol.tbl %>%
  filter(!var=='(Intercept)' &
           Pr...t.. < 0.01) %>%
  mutate(ub=Estimate+Std..Error,
         lb=Estimate-Std..Error,
         Model=factor(Model, levels=c('Basal area',
                                      'Height 95P',
                                      'Height skew',
                                      'QMD',
                                      'Total density',
                                      'Fir density',
                                      'Spruce density',
                                      'Pine density')),
         tag=as.numeric(Model))

#########
# Plot
#########
geol.plt <- ggplot(geol.tbl.sig[!geol.tbl.sig$Model %in% c('Fir density',
                                                           'Spruce density',
                                                           'Pine density'),],
                   aes(x=geol.code)) +
  geom_boxplot(aes(
    lower=Estimate-Std..Error,
    upper=Estimate+Std..Error,
    middle=Estimate,
    ymin=Estimate-2*Std..Error,
    ymax=Estimate+2*Std..Error),
    stat='identity',
    width=0.75,
    position = position_dodge(preserve = "single")) +
  geom_text(aes(y=Estimate, label=n),
            position=position_dodge(width=1),
            size=1.2, vjust=-0.5) +
  geom_hline(yintercept=0) +
  facet_wrap(~Model, scales='free_y') +
  tag_facets(tag_levels='1',
             tag_prefix='(',
             tag_suffix=')',
             position='bl') +
  labs(y='Anomaly from mean response',
       x='Geological unit code') +
  ggthemes::theme_calc(base_size=8)

geol.spp.plt <- ggplot(geol.tbl.sig[geol.tbl.sig$Model %in% c('Fir density',
                                                              'Spruce density',
                                                              'Pine density'),],
                       aes(x=geol.code)) +
  geom_boxplot(aes(
    lower=Estimate-Std..Error,
    upper=Estimate+Std..Error,
    middle=Estimate,
    ymin=Estimate-2*Std..Error,
    ymax=Estimate+2*Std..Error),
    stat='identity',
    width=0.75,
    position = position_dodge(preserve = "single")) +
  geom_text(aes(y=Estimate, label=n),
            position=position_dodge(width=1),
            size=1.2, vjust=-0.5) +
  geom_hline(yintercept=0) +
  coord_cartesian(clip='off') +
  facet_wrap(~Model, scales='free_y') +
  tag_facets(tag_levels='1',
             tag_prefix='(',
             tag_suffix=')',
             position='bl') +
  labs(y='Anomaly from mean response',
       x='Geological unit code') +
  ggthemes::theme_calc(base_size=8)

################################
# Assemble plot grid and write
################################

cairo_pdf(file.path('inst', 'ms', 'figures', 'Fig9.pdf'),
          width=140/25.4, height=140/25.4, onefile=T,
          family='Arial', bg='white')

design='
AAA
AAA
BBB'

wrap_plots(geol.plt, geol.spp.plt) +
  plot_layout(design=design) +
  plot_annotation(tag_levels = c('A', '1'),
                  tag_prefix = '(',
                  tag_suffix = ')') &
  theme(plot.tag = element_text(face = 'bold'))

dev.off()
