# Script for pulling GAM results and figures

##################################
# Set up workspace
##################################
# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

# Specify number of cores
nCores <- as.integer(availableCores()-6)

#############
# Load models
#############

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
gam.names.new
x1 <- match(names(gams), gam.names.new$o)
names(gams) <- gam.names.new$n[x1]

# Ingest variable names
varnames <- read.csv(file.path(config$data$int, 'explainer_names_table.csv'),
                     row.names=1)

# Ingest unscaled variables
vars <- read.csv(file.path(config$data$pro, 'all_variables_unscaled.csv'))

#############
# Summaries
#############

gam.sum <- lapply(gams, summary)
gam.pde <- lapply(gams, \(x) return(summary(x)$dev.expl))

# lapply(1:8, \(i) {
#   sink(file.path(config$data$pro, 'gam_performance', paste0(names(gams)[[i]], '_summary.txt')))
#   print(gam.sum[[i]])
#   sink()
#   })

#############
# Table
#############

gam.perf.df <- data.frame(cbind('Response'=names(gams),
                                'PDE'= unlist(gam.pde)),
                          check.names=F)

# write.csv(gam.perf.df, file.path(config$extdata$scratch, 'gam_performance', 'gam_perf_df.csv'),
#           row.names=F)

###################
# Residual checks
##################

# Write gam.check output
# lapply(1:8, \(i) {
#   sink(file.path(config$data$pro, 'gam_performance', paste0(names(gams)[[i]], '_check.txt')))
#   print(gam.check(gams[[i]]))
#   sink()
# })

# Write residual plots
# lapply(1:8, \(i) {
#   png(file.path(config$data$pro, 'gam_performance', paste0(names(gams)[[i]], '_residplots.png')))
#   par(mfrow=c(2,2), mar=rep(2.5,4))
#   gam.check(gams[[i]])
#   dev.off()
# })

##################################
# Visualizations
##################################

# Slice the model frame for one response to estimate partial effects
# of each variable, holding all other variables constant at
# median (for continuous) or mode (for factors)
slices <- lapply(target.vars, pe.slice, gams[[3]])
slices <- slices[!unlist(lapply(slices, is.null))]
slices.df <- reduce(slices, dplyr::left_join, by='v')

# Slice model frames for all GAMs and collapse to one dataframe
pe.dfs <- lapply(seq_along(gams), \(i) {
  slices <- lapply(target.vars, pe.slice, gams[[i]])
  slices <- slices[!unlist(lapply(slices, is.null))]
  slices.df <- reduce(slices, dplyr::left_join, by='v')
  pv <- gbm.summaries.5[gbm.summaries.5$Model==names(gams)[i],]$var
  pe.df <- pe.pivot(slices.df, pv)
  #pe.df <- pe.pivot(slices.df, target.vars)
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
         label = factor(label, levels=c('AET', 'CWD', 'SWE', '∆SWE',
                                        'Curvature', 'Elevation', 'Heat load',
                                        'TPI', 'TWI',
                                        'AWC', 'CEC', 'Organic matter',
                                        'Silt content','ksat', 'pH',
                                        'Geology', 'X', 'Y')),
         var=factor(var)
  ) %>%
  arrange(category,label)

peplot.dfs <- peplot.df %>%
  mutate(Set=case_when(Model %in% c('Fir density', 'Spruce density', 'Pine density') ~ 'Species',
                       T ~ 'Full')) %>%
  group_split(Model)

lblr <- c(`Basal area`=bquote('Basal area ('*m^2~m^-2*')'),
           `Height 95P`='Height 95P (m)',
           `Height skew`='Height skew',
           QMD='QMD (cm)',
           `Total density`=bquote('Total density (stems'~ha^-1*')'),
           `Fir density`=bquote('Fir density (stems'~ha^-1*')'),
           `Spruce density`=bquote('Spruce density (stems'~ha^-1*')'),
           `Pine density`=bquote('Pine density (stems'~ha^-1*')'))

# Plot PE for full-forest structure variables by model
peplots <- lapply(peplot.dfs, \(p) {
  #lblr <- lblr[names(lblr) %in% unique(p$Model)]
  ggplot(p) +
    geom_line(aes(x=v, y=fit, color=label), linewidth=4) +
    geom_line(aes(x=v, y=u95, color=label), linetype=3, linewidth=2) +
    geom_line(aes(x=v, y=l95, color=label), linetype=3, linewidth=2) +
    # geom_text(
    #   aes(label=Model), hjust=0.5, vjust=3.5,
    #   x=min(p$v) + diff(range(p$v))/2,
    #   y=Inf, color='black', fontface='bold',
    # ) +
    scale_color_manual(limits=p$label,
                       values=p$pdcolors,
                       name=NULL) +
    scale_y_continuous(limits=c(0,max(p$fit)),
                       #labels=scientific
                       ) +
    coord_cartesian(clip='off') +
    # facet_wrap(~Model, scales='free_y',
    #            strip.position = 'left',
    #            labeller=as_labeller(\(x) {x=lblr[names(lblr)==x]; x},
    #                                 default=identity))  +
    ggthemes::theme_calc(base_size=48,
                         base_family='Arial') +
    labs(#x='Centered values of explanatory variables',
         x=NULL,
         y=lblr[names(lblr)==p$Model[1]][[1]],
         # y=NULL,
         title=p$Model[[1]]
    ) +
    theme(legend.position='none',
          legend.text=element_text(size=30),
          strip.background = element_blank(),
          strip.placement = "outside",
          aspect.ratio=1,
          #panel.grid = element_blank()
          plot.background=element_rect(fill=NA, color=NA, linewidth=4),
          panel.background=element_rect(fill=NA, color=NA, linewidth=4),
          #plot.margin = margin(-, 50, -50, 50, "pt")
          )
})



library(patchwork)
(peplots[[1]]) +
    (plot_spacer() /
    peplots[[2]]) +
  plot_layout(heights=c(4,4))


peplots[[1]]+peplots[[2]]+plot_layout(design=layout,
                                      guides='collect')+
  plot_annotation(tag_levels = 'A',
                  tag_prefix='(',
                  tag_suffix=')')

layout=c(area(t=1, l=1, b=16, r=4),
         area(t=18, l=1, b=28, r=4))

pp <- peplots[[1]]+peplots[[2]]+peplots[[3]]+peplots[[4]]+ peplots[[5]] /
  peplots[[6]]|peplots[[7]]|peplots[[8]] +
  plot_layout(design=layout) +
  plot_annotation(tag_levels = 'A',
                  tag_prefix='(',tag_suffix=')')

pp[[7]] <- pp[[7]] + xlab('Centered values')

pp

pp /
  (peplots[[6]]|peplots[[7]]|peplots[[8]]) +
  plot_layout(design=layout)

theme_border <-
  ggthemes::theme_calc() +
  theme(plot.background = element_rect(fill = NA, colour = 'black', linewidth = 3))

wrap_elements(full=peplots[[1]]+peplots[[2]]+peplots[[3]]+peplots[[4]]+peplots[[5]] +
                plot_annotation(theme=theme_border)) /
  wrap_elements(full=peplots[[6]]+peplots[[7]]+peplots[[8]] +
                  plot_annotation(theme=theme_border)) +
  plot_annotation(tag_levels = 'A',
                  tag_prefix='(',
                  tag_suffix=')') +
  plot_layout(design=layout)

# Run plots faceted by variable for each model
for(i in unique(peplot.df$Model)) {

  png(file.path(config$data$pro, 'peplots', paste0(i, '_pdp2.png')),
      width=1200, height=900)

  gx <- ggplot(peplot.df[peplot.df$Model==i,]) +
    geom_line(aes(x=v, y=fit, color=interaction(category, label, sep=': ')), linewidth=1) +
    geom_line(aes(x=v, y=u95, color=interaction(category, label, sep=': ')), linetype=3, linewidth=0.5) +
    geom_line(aes(x=v, y=l95, color=interaction(category, label, sep=': ')), linetype=3, linewidth=0.5) +
    scale_color_manual(limits=interaction(peplot.df$category, peplot.df$label, sep=': '),
                       values=peplot.df$pdcolors,
                       name=i) +
    facet_wrap(~label, scales='fixed') +
    ggthemes::theme_calc(base_size=18,
                         base_family='Arial') +
    labs(x='Centered values of explanatory variables',
         y=NULL) +
    theme(legend.position='left',
          #legend.text=element_text(size=12)
          )

  print(gx)
  dev.off()
}

###############
# Geology
###############

geol.tbl <- lapply(gam.sum, \(x) {
  gdf <- data.frame(x$p.table) %>%
    rownames_to_column(., 'var') %>%
    mutate(var=str_replace_all(var, 'geology', ''))
  gdf
  })

geol.tbl <- bind_rows(geol.tbl, .id='Model')

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

geol.tbl <- left_join(geol.tbl, geol.ref, by=c('var'='geol.name'))

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
                                             'Pine density')))

ggplot(geol.tbl.sig, aes(x=geol.code)) +
  geom_boxplot(aes(
                   lower=Estimate-Std..Error,
                   upper=Estimate+Std..Error,
                   middle=Estimate,
                   ymin=Estimate-2*Std..Error,
                   ymax=Estimate+2*Std..Error),
               stat='identity',
               width=0.6,
               position = position_dodge(preserve = "single")) +
  geom_hline(yintercept=0) +
  facet_wrap(~Model, scales='free') +
  labs(y='Anomaly from mean',
       x='Geological unit code') +
  ggthemes::theme_calc(base_size=18)


###############
# Interactions
###############

## One response ##

# Slice the model frame for one response to estimate partial effects
# of each variable interaction, holding all other variables constant at
# median (for continuous) or mode (for factors)
itxs <- strsplit(target.itx, ', ')
itx.slices <- lapply(itxs, pe.slice.itx, gams[[3]])
itx.slices <- itx.slices[!unlist(lapply(itx.slices, is.null))]
itx.slices.df <- Reduce(cbind, itx.slices) %>%
  select(-c(v1,v2))
itx.slices.df <- cbind(expand.grid(v1=seq(-4.9,5,0.1),
                                   v2=seq(-4.9,5,0.1)),
                       itx.slices.df)

# Pivot for variable comparison
itx.plot.df <- pe.pivot.itx(itx.slices.df, itxs)

# Plot
ggplot(itx.plot.df, aes(x=v1, y=v2, z=fit)) +
  geom_raster(aes(fill=fit)) +
  geom_contour(color='white', bins=20, alpha=0.25) +
  geom_contour(aes(x=v1, y=v2, z=u95), color='white', bins=20, alpha=0.25) +
  scale_fill_viridis(option='G', na.value=NA) +
  facet_wrap(~var, scales='free') +
  ggthemes::theme_calc(base_size=18)

## All responses ##

# Slice model frames for all GAMs and collapse to one dataframe
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
         l1=str_split(var, '-')[[1]][1],
         l2=str_split(var, '-')[[1]][2]
  ) %>%
  arrange(Model, var) %>%
  drop_na()

for(i in unique(peplot.itx.df$Model)) {

  png(file.path(config$data$pro, 'itx_peplots', paste0(i, '_itx_peplot.png')),
      width=1200, height=900)

  gx <- ggplot(peplot.itx.df[peplot.itx.df$Model==i,],
               aes(x=v1, y=v2, z=fit)) +
    geom_raster(aes(fill=fit)) +
    geom_contour(color='white', bins=20, alpha=0.25) +
    scale_fill_viridis(option='C', na.value=NA, name=i, direction=1) +
    facet_wrap(~var, scales='free') +
    ggthemes::theme_calc(base_size=18)

  print(gx)

  dev.off()
}

peplot.itx.sub <- peplot.itx.df %>%
  group_by(Model, var) %>%
  summarise(rng=diff(range(fit, na.rm=T))) %>%
  arrange(Model, rng) %>%
  top_n(3) %>%
  left_join(peplot.itx.df, by=c('Model', 'var')) %>%
  ungroup() %>%
  filter(!Model %in% c('Fir density', 'Pine density', 'Spruce density')) %>%
  group_split(Model, var)

peplots <- lapply(peplot.itx.sub, \(i) {
  g <- ggplot(i, aes(x=v1, y=v2, z=fit)) +
  geom_raster(aes(fill=fit)) +
  geom_contour(color='white', bins=20, alpha=0.25) +
  scale_fill_viridis(option='C', na.value=NA, direction=1,
                     name=i$Model[1]) +
  labs(x=i$l1[1], y=i$l2[1]) +
  #facet_wrap(~var, ncol=5, scales='free') +
  ggthemes::theme_calc(base_size=18)
  g
  })

patchwork::wrap_plots(peplots, nrow=3, ncol=5, byrow=F)

 ###############
# Scratch
###############

vis.gam(gams[[7]],
        view=unlist(strsplit(target.itx[20], ', ')),
        type='response',
        plot.type='persp',
        too.far=1.5,
        phi=24,
        theta=60,
        border=NA,
        color='heat',
        zlab='stem density'
        )

names(gams[[7]]$coefficients)
par(mfcol=c(12,2), mar=c(rep(1,2), rep(1,2)))
plot.gam(gams[[7]], scheme=1, ylim=c(-5,5))

varnms <- c('Elevation',
            'Folded Aspect',
            'Slope',
            'TPI',
            'Heat Load',
            'Elevation:Folded Aspect',
            'Elevation:TPI',
            'Elevation:SWE',
            'Folded Aspect:TPI',
            'Elevation:KJde',
            'Elevation:Km',
            'Elevation:Kmv',
            'Elevation:Pm',
            'Elevation:PPm',
            'Elevation:Qd',
            'Elevation:Ql',
            'Elevation:Tmi',
            'Elevation:Two',
            'Soil AWC',
            'Soil Percent OM',
            'Soil k',
            'Soil Total Depth',
            'SWE')

par(mfcol=c(3,4), mar=c(2,2,2,1), lwd=2)
for(i in 19:22){
  plot.gam(gams[[2]],
           scheme=1,
           ylim=c(-5,5),
           select=i,
           main=varnms[i],
           ylab='Stand density (stems/ha)',
           xlab=paste('Standardized', varnms[i]),
           cex.lab=2,
           cex.main=2.5)
}

inter.density <- visreg2d(gams[[3]], xvar='delta_swe', yvar='elevation', plot.type='gg') +
  scale_fill_viridis(name='stems ha^-1^', limits=c(100, 2500)) +
  ggtitle('Density') +
  xlab('∆SWE') +
  ylab('Elevation') +
  theme_minimal(base_size = 16) +
  theme(legend.title=element_text(size=14), legend.position = 'right')

inter.height <- visreg2d(gam.height, xvar='awc', yvar='elevation_10m', plot.type='gg') +
  scale_fill_viridis(name='m') +
  ggtitle('Height') +
  xlab('Soil AWC') +
  ylab('Elevation') +
  theme_minimal(base_size = 16) +
  theme(legend.title=element_text(size=14), legend.position = 'right')

inter.diam <- visreg2d(gam.diam, xvar='awc', yvar='elevation_10m', plot.type='gg') +
  scale_fill_viridis(name = 'cm') +
  ggtitle('QMD') +
  xlab('Soil AWC') +
  ylab('Elevation') +
  theme_minimal(base_size = 16) +
  theme(legend.title=element_text(size=14), legend.position = 'right')

inter.ba <- visreg2d(gam.ba, xvar='delta_swe', yvar='elevation_10m', plot.type='gg') +
  scale_fill_viridis(name = 'm^2^ m^-2^') +
  ggtitle('Height skew') +
  xlab('∆SWE') +
  ylab('Elevation') +
  theme_minimal(base_size = 16) +
  theme(legend.title=element_text(size=14), legend.position = 'right')

inter.height.skew <- visreg2d(gam.height.skew, xvar='delta_swe', yvar='elevation_10m', plot.type='gg') +
  scale_fill_viridis(name='skewness') +
  ggtitle('Height skew') +
  xlab('∆SWE') +
  ylab('Elevation') +
  theme_minimal(base_size = 16) +
  theme(legend.title=element_text(size=14), legend.position = 'right')

g <- c(inter.density, inter.height, inter.ba, inter.diam, inter.height.skew)

gridExtra::grid.arrange(inter.density, inter.height, inter.ba,
                        inter.diam, inter.height.skew,
                        ncol=3, widths=c(1,1,1))
