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

#############
# Summaries
#############

gam.sum <- lapply(gams, summary)
gam.pde <- lapply(gams, \(x) return(summary(x)$dev.expl))

#############
# Table
#############

gam.perf.df <- data.frame(cbind('Response'=names(gams),
                                'PDE'= unlist(gam.pde)),
                          check.names=F)

write.csv(gam.perf.df, file.path(config$extdata$scratch, 'gam_performance', 'gam_perf_df.csv'),
          row.names=F)

###################
# Residual checks
##################

# Write gam.check output
lapply(1:8, \(i) {
  sink(file.path(config$extdata$scratch, paste0(names(gams)[[i]], '.txt')))
  print(gam.check(gams[[i]]))
  sink()
})

# Write residual plots
lapply(1:8, \(i) {
  png(file.path(config$extdata$scratch, 'gam_performance', paste0(names(gams)[[i]], '_check.png')))
  par(mfrow=c(2,2), mar=rep(2.5,4))
  gam.check(gams[[i]])
  dev.off()
})

##################################
# Visualizations
##################################

# Pull variable names
varnames <- read.csv(file.path(config$data$int, 'explainer_names_table.csv'),
                     row.names=1)

# # Specify color palettes
# mypal <- colorRampPalette(brewer.pal(6, "PuBu")[3:6])
# mypal2 <- colorRampPalette(brewer.pal(8, "BuGn")[3:8])
# mypal3 <- colorRampPalette(brewer.pal(6, "YlOrRd")[3:6])
# mypal4 <- colorRampPalette(brewer.pal(6, "Greys")[5])
#
# my_colors <- c("SWE" = mypal(4)[3],
#                "∆SWE" = mypal(4)[4],
#                "Soil AWC" = mypal3(4)[2],
#                "Elevation" = mypal2(4)[1],
#                "Soil total depth" = mypal3(4)[4],
#                "Slope" = mypal2(4)[3],
#                'Geology' = mypal4(1),
#                'TPI' = mypal2(4)[4],
#                'Soil Ksat' = mypal3(4)[3])

pd.slice <- function(v, mod) {
  if(!v=='geology') {
    data <- model.frame(mod)
    data <- data[-1]
    meds <- apply(data, 2, \(x) median(as.numeric(x), na.rm=T))
    r.max <- max(data[v], na.rm=T)
    r.min <- min(data[v], na.rm=T)
    geol.mode <- modal(data['geology'], na.rm=T)
    df <- data.frame(pseq=seq(-4.99,5,0.01))
    df <- cbind(df, t(data.frame(meds)))
    df$geology <- geol.mode
    df[(df$pseq<r.min | df$pseq>r.max),] <- NA
    df <- df[!names(df)==v]
    names(df)[1] <- v
    pred.v <- predict(mod, df, se.fit=T)
    slice.v <- data.frame(sup=seq(-4.99,5,0.01),
                          fit=pred.v$fit,
                          u95=pred.v$fit+pred.v$se.fit,
                          l95=pred.v$fit-pred.v$se.fit)
    names(slice.v) <- c('v', paste0(v,'.fit'), paste0(v, '.u95'), paste0(v, '.l95'))
    slice.v
  }
}

slices <- lapply(target.vars, pd.slice, gams[[3]])
slices <- slices[!unlist(lapply(slices, is.null))]
slices.df <- reduce(slices, dplyr::left_join, by='v')

ggplot(slices.df) +
  geom_line(aes(x=v, y=elevation.fit)) +
  geom_line(aes(x=v, y=elevation.u95)) +
  geom_line(aes(x=v, y=elevation.l95))

pd.plot <- function(sdf, plot.vars) {
  fl <- sdf %>%
    pivot_longer(cols=contains('fit'),
                 names_to='var',
                 values_to='fit') %>%
    mutate(var=str_replace_all(var, '.fit', '')) %>%
    dplyr::select(c(v,var,fit)) %>%
    left_join(varnames, by=c('var'='varnames'))

  ul <- slices.df %>%
    pivot_longer(cols=contains('u95'),
                 names_to='var',
                 values_to='u95') %>%
    mutate(var=str_replace_all(var, '.u95', '')) %>%
    dplyr::select(c(v,var, u95))

  ll <- slices.df %>%
    pivot_longer(cols=contains('l95'),
                 names_to='var',
                 values_to='l95') %>%
    mutate(var=str_replace_all(var, '.l95', '')) %>%
    dplyr::select(c(v,var, l95))

  slices.df.l <- reduce(list(fl,ul,ll), left_join, by=c('v', 'var'))
  slices.df.l <- slices.df.l %>% filter(var %in% plot.vars)

  slices.df.l
}

dxy <- pd.plot(slices.df, target.vars)

plot.dfs <- lapply(1:3, \(i) {
  slices <- lapply(target.vars, pd.slice, gams[[i]])
  slices <- slices[!unlist(lapply(slices, is.null))]
  slices
  #slices.df <- reduce(slices, dplyr::left_join, by='v')
  #pv <- gbm.summaries.5[gbm.summaries.5$Model==names(gams)[i],]$var
  #plot.df <- pd.plot(slices.df, pv)
  #plot.df
  })

pdplot.df <- bind_rows(plot.dfs, .id='Model')

ggplot(pdplot.df) +
  geom_line(aes(x=v, y=fit, color=label)) +
  geom_line(aes(x=v, y=u95, color=label), linetype=3, linewidth=0.25) +
  geom_line(aes(x=v, y=l95, color=label), linetype=3, linewidth=0.25) +
  #scale_color_manual(limits=fl$label, values=fl$pdcolors) +
  facet_wrap(~Model)

# Density
dens.long <- vars %>%
  dplyr::select(c(density, delta_swe, swe, awc, elevation)) %>%
  pivot_longer(cols=delta_swe:elevation)

pe.density <-  ggplot(dens.long, aes(x=value, y=density, color=name)) +
  geom_smooth(formula=y~s(x, bs="tp"),
              se=T) +
  scale_color_manual(values = unname(my_colors[c('Soil AWC', '∆SWE', 'Elevation', 'SWE')]),
                     labels=c('Soil AWC', '∆SWE', 'Elevation', 'SWE')) +
  labs(title='Density',
       x='Standardized values',
       y='Density (stems ha^-1^)') +
  jtools::theme_apa()

# Height
height.long <- test.height %>%
  dplyr::select(c(yhat, delta_swe, swe, awc, elevation, td)) %>%
  pivot_longer(cols=delta_swe:td)

pred.height.est <- plot.gam(gam.height, select=1)
tmin <- pred.height.est[[1]]
tmin.x <- tmin$x
tmin.raw <- tmin$raw
tmin.fit <- tmin$fit
tmin.se <- tmin$se
tmin.u95 <- tmin.fit+tmin.se
tmin.l95 <- tmin.fit-tmin.se

tmin.df <- data.frame(Tmin=tmin.x,
                      #'RWI'=tmin.raw,
                      Fit=tmin.fit,
                      U95=tmin.u95,
                      L95=tmin.l95)

ggplot(tmin.df, aes(x=Tmin, y=Fit)) +
  geom_ribbon(aes(ymin=L95, ymax=U95)) +
  geom_line()

pe.height <- ggplot(height.long, aes(x=value, y=yhat, color=name)) +
  # geom_point() +
  geom_smooth(formula=y~s(x, bs="tp"),
              se=T) +
  geom_rug() +
  scale_color_manual(values = unname(my_colors[c('Soil AWC', '∆SWE', 'Elevation',
                                                 'SWE', 'Soil total depth')]),
                     labels=c('Soil AWC', '∆SWE', 'Elevation',
                              'SWE', 'Soil total depth')) +
  scale_y_continuous(limits=c(0,25)) +
  labs(title='Height',
       x='Standardized values',
       y='Maximum height (m)') +
  jtools::theme_apa()

pe.height

diam.long <- vars %>%
  dplyr::select(c(diam, delta_swe, swe, awc, elevation, td)) %>%
  pivot_longer(cols=delta_swe:td)

pe.diam <-  ggplot(diam.long, aes(x=value, y=diam, color=name)) +
  geom_smooth(formula=y~s(x, bs="tp"),
              se=T) +
  scale_color_manual(values = unname(my_colors[c('Soil AWC', '∆SWE', 'Elevation',
                                                 'SWE', 'Soil total depth')]),
                     labels=c('Soil AWC', '∆SWE', 'Elevation',
                              'SWE', 'Soil total depth')) +
  labs(title='QMD',
       x='Standardized values',
       y='QMD (cm)') +
  jtools::theme_apa()

ba.long <- vars %>%
  dplyr::select(c(ba, delta_swe, swe, awc, elevation, k)) %>%
  pivot_longer(cols=delta_swe:k)

pe.ba <-  ggplot(ba.long, aes(x=value, y=ba, color=name)) +
  geom_smooth(formula=y~s(x, bs="tp"),
              se=T) +
  scale_color_manual(values = unname(my_colors[c('Soil AWC', '∆SWE', 'Elevation', 'Soil K', 'SWE')]),
                     labels=c('Soil AWC', '∆SWE', 'Elevation', 'Soil K', 'SWE')) +
  labs(title='BA',
       x='Standardized values',
       y='BA (m^2^ha^-1^)') +
  jtools::theme_apa()

height.skew.long <- vars %>%
  dplyr::select(c(height.skew, delta_swe, swe, awc, tpi, td)) %>%
  pivot_longer(cols=delta_swe:td)

pe.height.skew <-  ggplot(height.skew.long, aes(x=value, y=height.skew, color=name)) +
  geom_smooth(formula=y~s(x, bs="tp"),
              se=T) +
  scale_color_manual(values = unname(my_colors[c('Soil AWC', '∆SWE', 'SWE',
                                                 'Soil total depth', 'TPI')]),
                     labels=c('Soil AWC', '∆SWE',
                              'SWE', 'Soil total depth', 'TPI')) +
  labs(title='Height skew',
       x='Standardized values',
       y='Skewness of height') +
  jtools::theme_apa()


# ABLA Density
abla.dens.long <- vars %>%
  dplyr::select(c(abla_density, delta_swe, swe, awc, om, elevation)) %>%
  pivot_longer(cols=delta_swe:elevation)

pe.abla.density <- ggplot(abla.dens.long, aes(x=value, y=abla_density, color=name)) +
  geom_smooth(formula=y~s(x, bs="tp"),
              se=T) +
  scale_color_manual(values = unname(my_colors[c('Soil AWC', '∆SWE', 'Elevation', 'OM', 'SWE')]),
                     labels=c('Soil AWC', '∆SWE', 'Elevation', 'OM', 'SWE')) +
  labs(title='ABLA Density',
       x='Standardized values',
       y='Density (stems ha^-1^)') +
  jtools::theme_apa()

pe.abla.density

pien.dens.long <- vars %>%
  dplyr::select(c(pien_density, delta_swe, swe, awc, om, elevation)) %>%
  pivot_longer(cols=delta_swe:elevation)

pe.pien.density <-  ggplot(pien.dens.long, aes(x=value, y=pien_density, color=name)) +
  geom_smooth(formula=y~s(x, bs="tp"),
              se=T) +
  scale_color_manual(values = unname(my_colors[c('Soil AWC', '∆SWE', 'Elevation', 'OM', 'SWE')]),
                     labels=c('Soil AWC', '∆SWE', 'Elevation', 'OM', 'SWE')) +
  labs(title='PIEN Density',
       x='Standardized values',
       y='Density (stems ha^-1^)') +
  jtools::theme_apa()

pe.pien.density

pico.dens.long <- vars %>%
  dplyr::select(c(pico_density, delta_swe, swe, awc, om, elevation)) %>%
  pivot_longer(cols=delta_swe:elevation)

pe.pico.density <-  ggplot(pico.dens.long, aes(x=value, y=pico_density, color=name)) +
  geom_smooth(formula=y~s(x, bs="tp"),
              se=T) +
  scale_color_manual(values = unname(my_colors[c('Soil AWC', '∆SWE', 'Elevation', 'OM', 'SWE')]),
                     labels=c('Soil AWC', '∆SWE', 'Elevation', 'OM', 'SWE')) +
  labs(title='PICO Density',
       x='Standardized values',
       y='Density (stems ha^-1^)') +
  jtools::theme_apa()

pe.pico.density
plot.gam(gam.pico.density, pages=1)

gridExtra::grid.arrange(pe.density, pe.height, pe.diam,
                        pe.ba, pe.height.skew,
                        ncol=2, widths=c(1,1))

gridExtra::grid.arrange(pe.abla.density, pe.pien.density, pe.pico.density,
                        ncol=2, widths=c(1,1))


# ggplot(subset(dens.long, !is.na(geology)), aes(x=geology, y=height)) +
#   geom_boxplot() +
#   ggthemes::theme_calc()
#
# vars %>%
#   group_by(geology) %>%
#   summarise(n=n())


vis.gam(mod_gam1,
        view=c('swe','folded_aspect_205'),
        type='response',
        plot.type='persp',
        phi=10,
        theta=24,
        border=NA,
        color='heat',
        zlab='stem density')



names(mod_gam2$coefficients)
par(mfcol=c(12,2), mar=c(rep(1,2), rep(1,2)))
plot.gam(mod_gam2, scheme=1, ylim=c(-5,5))

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
?mar
par(mfcol=c(12,4), mar=c(4,2,4,1), lwd=2)
for(i in 19:22){
  plot.gam(mod_gam2,
           scheme=1,
           ylim=c(-5,5),
           select=i,
           main=varnms[i],
           ylab='Stand density (stems/ha)',
           xlab=paste('Standardized', varnms[i]),
           cex.lab=2,
           cex.main=2.5)
}

ggplot(vars, aes(x=density, y=elevation, color=geology)) +
  geom_point() +
  geom_abline()

visreg(gam.ba)

termplot(gam.ba)

AIC(mod_lm)
AIC(mod_gam2)

summary(mod_lm)$sp.criterion
summary(mod_gam2)$sp.criterion
gam.height$var.summary

inter.density <- visreg2d(gam.density, xvar='delta_swe', yvar='elevation_10m', plot.type='gg') +
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

mod_gam2$coefficients
par(mfcol=c(1,3), mar=c(2,2,4,2))
vis.gam(mod_gam2, view=c('elevation','swe'), type='response', plot.type='persp', phi=20, theta=48, border=NA, color='topo', zlab='90th pctl height', contour.col='black', main='Density v Elevation v TPI', xlab='Standardized elevation', ylab='standardized TPI', cex.lab=2, cex.main=2.5)

summary(mod_gam2)
summary(mod_lm)$r.sq
summary(mod_gam1)$r.sq
