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

# Ingest variable names
varnames <- read.csv(file.path(config$data$raw, 'explainer_names_table.csv'),
                     row.names=1)
varnames[varnames$label=='\xc6SWE', 'label'] <- '\u0394SWE'

# Ingest unscaled variables
vars <- read.csv(file.path(config$data$pro, 'all_variables_unscaled.csv'))
download.file('https://drive.usercontent.google.com/download?id=1lQGjd1ZPV6sxmHLUWosu3eZv55cfnxKq&confirm=true',
              destfile=file.path(tempdir(), 'all_variables_unscaled.csv'),
              method='wget')
vars <- read.csv(file.path(tempdir(), 'all_variables_unscaled.csv'))

##################################
# Partial-effects interactions
##################################

# Specify target interactions
target.itx <-c('elevation, heat_load', 'elevation, tpi', 'elevation, swe',
               'elevation, delta_swe', 'elevation, cwd', 'elevation, aet',
               'tpi, swe', 'tpi, delta_swe', 'tpi, cwd',
               'tpi, aet', 'awc, swe', 'awc, delta_swe',
               'awc, aet', 'awc, cwd', 'heat_load, swe',
               'heat_load, delta_swe', 'heat_load, awc', 'heat_load, aet',
               'heat_load, cwd', 'x, y')

# Slice model frames for all GAMs and collapse to one dataframe
# NOTE: This process takes 10-15 minutes on one thread/core
pe.itx.dfs <- lapply(seq_along(gams), \(i) {
  itxs <- strsplit(target.itx, ', ')
  slices <- lapply(itxs, pe.slice.itx, gams[[i]])
  slices <- slices[!unlist(lapply(slices, is.null))]
  slices.df <- Reduce(cbind, slices) %>%
    select(-c(v1,v2))
  slices.df <- cbind(expand.grid(v1=seq(-4.9,5,0.1),
                                 v2=seq(-4.9,5,0.1)),
                     slices.df)
  pe.df <- pe.pivot.itx(slices.df, target.vars)
  pe.df
})

# Assign model names to list members from GAM names
names(pe.itx.dfs) <- gam.names.new$n[x1]

# Coerce some variables to factor for specific order
peplot.itx.df <- bind_rows(pe.itx.dfs, .id='Model') %>%
  mutate(Model=factor(Model, levels=c('Basal area',
                                      'Height 95P',
                                      'Height skew',
                                      'QMD',
                                      'Total density',
                                      'Fir density',
                                      'Spruce density',
                                      'Pine density')),
         var=factor(var),
         l1=str_split(var, '-', simplify=T)[,1],
         l2=str_split(var, '-', simplify=T)[,2],
         lab1=factor(l1, levels=c('awc', 'elevation', 'heat_load',
                                  'tpi', 'x'),
                     labels=c('AWC', 'Elevation', 'Heat load',
                              'TPI', 'X')),
         lab2=factor(l2, levels=c('aet', 'cwd', 'delta_swe', 'swe',
                                  'heat_load', 'tpi', 'awc', 'y'),
                     labels=c('AET', 'CWD', '\u0394SWE', 'SWE',
                              'Heat load', 'TPI', 'AWC', 'Y')),
         model_orig=factor(Model, labels=c('ba', 'height', 'height.skew',
                                           'diam', 'density',
                                           'abla_density', 'pien_density', 'pico_density'))
  ) %>%
  arrange(Model, var) %>%
  drop_na()

# Reshape and filter the data frame
peplot.itx.sub <- peplot.itx.df %>%
  filter(!var=='x-y') %>%
  group_by(Model, var) %>%
  summarise(rng=diff(range(fit, na.rm=T))) %>%
  arrange(Model, rng) %>%
  top_n(3) %>%
  left_join(peplot.itx.df, by=c('Model', 'var')) %>%
  ungroup() %>%
  filter(!Model %in% c('Fir density', 'Pine density', 'Spruce density')) %>%
  group_by(Model) %>%
  mutate(# liml=min(range(fit, na.rm=T)[1]),
    # limu=max(range(fit, na.rm=T)[2]),
    liml=min(vars[names(vars)==first(model_orig)], na.rm=T),
    limu=max(vars[names(vars)==first(model_orig)], na.rm=T)) %>%
  ungroup() %>%
  group_split(Model, var)

# Specify axis and legend labels
lblr <- c(`Basal area`=bquote('Basal area ('*m^2~ha^-1*')'),
          `Height 95P`='Height 95P (m)',
          `Height skew`='Height skew',
          QMD='QMD (cm)',
          `Total density`=bquote('Total density (stems'~ha^-1*')'),
          `Fir density`=bquote('Fir density (stems'~ha^-1*')'),
          `Spruce density`=bquote('Spruce density (stems'~ha^-1*')'),
          `Pine density`=bquote('Pine density (stems'~ha^-1*')'))

exp.lblr <- c('awc'=bquote('AWC ('*cm~cm^-1*')'),
              'elevation'='Elevation (m)',
              'heat_load'='Heat load',
              'tpi'='TPI',
              'x'='X',
              'aet'='AET (mm)',
              'cwd'='CWD (mm)',
              'delta_swe'=bquote('\u0394SWE (%'*d^-1*')'),
              'swe'='SWE (m)',
              'y'='Y'
)

#########
# Plot
#########

# Plot PE for full-forest structure variables by model
peplots <- lapply(peplot.itx.sub, \(i) {
  g <- ggplot(i, aes(x=v1, y=v2, z=fit)) +
    geom_raster(aes(fill=fit), interpolate=F) +
    geom_contour(color='white', bins=20, alpha=0.25) +
    scale_fill_viridis(option='F', na.value=NA, direction=1,
                       name=lblr[names(lblr)==i$Model[1]][[1]],
                       limits=c(i$liml[[1]], i$limu[[1]]),
                       guide=guide_colorbar(title.position='right',
                                            title.vjust=0.5)) +
    labs(title=paste(i$lab1, i$lab2, sep=' : '),
         #x=exp.lblr[names(exp.lblr)==i$l1[1]][[1]],
         #y=exp.lblr[names(exp.lblr)==i$l2[1]][[1]]
         x=paste(i$lab1, '(zero-centered)'),
         y=paste(i$lab2, '(zero-centered)')) +
    ggthemes::theme_calc(base_size=8) +
    theme(#aspect.ratio=1,
      legend.title = element_text(angle = -90,
                                  hjust = 0.5),
      legend.key.width = unit(0.005, 'npc'),
      legend.key.height = unit(0.03, 'npc'),
      #legend.margin=margin(t=0, b=0, r=0 l=0.0002, 'npc'),
      legend.text=element_text(size=6, hjust=1),
      plot.margin=margin(l=0.025, b=0.01, unit='npc'),
      plot.background=element_rect(fill=NA, color=NA, linewidth=0),
      plot.title=element_text(face='bold',
                              hjust=0.5))
  g
})

cairo_pdf(file.path('inst', 'ms', 'figures', 'Fig9.pdf'),
          width=190/25.4, height=220/25.4, onefile=T,
          family='Arial', bg='white')

p <- wrap_plots(peplots, nrow=5, ncol=3, byrow=T) +
  plot_annotation(tag_levels = list(paste0('(', LETTERS[1:15], ')'))) &
  theme(plot.tag = element_text(face = 'bold'))

p

grid.text(levels(peplot.itx.df$Model)[1], x=unit(0.015, 'npc'), y=unit(0.9, 'npc'), rot=90, just='center', gp=gpar(fontface='bold'))

grid.text(levels(peplot.itx.df$Model)[2], x=unit(0.015, 'npc'), y=unit(0.7, 'npc'), rot=90, just='center',
          gp=gpar(fontface='bold'))
grid.text(levels(peplot.itx.df$Model)[3], x=unit(0.015, 'npc'), y=unit(0.5, 'npc'), rot=90, just='center',
          gp=gpar(fontface='bold'))
grid.text(levels(peplot.itx.df$Model)[4], x=unit(0.015, 'npc'), y=unit(0.3, 'npc'), rot=90, just='center',
          gp=gpar(fontface='bold'))
grid.text(levels(peplot.itx.df$Model)[5], x=unit(0.015, 'npc'), y=unit(0.1, 'npc'), rot=90, just='center',
          gp=gpar(fontface='bold'))

# grid.rect(x=unit(0, 'npc'), y=unit(0, 'npc'), width = 1, height = 0.21, gp = gpar(lwd = 1, col = "black", fill = NA),
#           just=c('left', 'bottom'))
# grid.rect(x=unit(0, 'npc'), y=unit(0.405, 'npc'), width = 1, height = 0.2, gp = gpar(lwd = 1, col = "black", fill = NA),
#           just=c('left', 'bottom'))
# grid.rect(x=unit(0, 'npc'), y=unit(0.8, 'npc'), width = 1, height = 0.2, gp = gpar(lwd = 1, col = "black", fill = NA),
#           just=c('left', 'bottom'))
# grid.rect(x=unit(0, 'npc'), y=unit(0, 'npc'), width = 1, height = 1, gp = gpar(lwd = 1.1, col = "black", fill = NA),
#           just=c('left', 'bottom'))

dev.off()
