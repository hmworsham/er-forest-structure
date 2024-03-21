# Script for generating generalized additive models on forest structure and explainers

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

# Specify number of cores
nCores <- as.integer(availableCores()-10)

source(file.path('inst', 'notebooks',
                 'regressions', '01.00_stats_ingest_data.R'))

# Define variables we want to use in model
target.vars <- c('heat_load',
                 'elevation',
                 'twi',
                 #'folded_aspect_205',
                 #'slope', #5
                 'tpi',
                 'curvature',
                 'awc',
                 'cec',
                 #'sand', #10
                 #'total_depth',
                 'silt',
                 'ksat',
                 'ph',
                 #'clay', #15
                 'om',
                 'swe',
                 'delta_swe',
                 'cwd',
                 'aet', #20
                 'geology',
                 'x',
                 'y')

target.itx <-c('elevation, heat_load',
               'elevation, tpi',
               'elevation, swe',
               'elevation, delta_swe',
               'elevation, cwd',
               'elevation, aet',
               'tpi, swe',
               'tpi, delta_swe',
               'tpi, cwd',
               'tpi, aet',
               'awc, swe',
               'awc, delta_swe',
               'awc, aet',
               'awc, cwd',
               'heat_load, swe',
               'heat_load, delta_swe',
               'heat_load, awc',
               'heat_load, aet',
               'heat_load, cwd'#,
               #'x, y'
               )

##################################
# Height GAM
##################################
height.mf <- make.modframe('height', vars, 'gam', target.vars, itx=target.itx)

gam.height <- gamm(height.mf$formula,
                  data=height.mf$data,
                  method='REML',
                  select=T,
                  family='gaussian',
                  correlation = corExp(form=~x+y),
                  control=list(nthreads=nCores))

sum.height <- summary(gam.height)
concurv <- concurvity(gam.height)
de.height <- summary(gam.height)$dev.expl

concurv.height.full <- concurvity(gam.height, full=T)
concurv.height <- concurvity(gam.height, full=F)$estimate
corrplot(concurv.height)

##################################
# BA GAM
##################################
ba.mf <- make.modframe('ba', vars, 'gam', target.vars, itx=target.itx)

gam.ba <- gam(ba.mf$formula,
                  data=ba.mf$data,
                  method='REML',
                  select=T,
                  family='gaussian',
                  control=list(nthreads=nCores))

sum.ba <- summary(gam.ba)
concurv <- concurvity(gam.ba)
de.ba <- summary(gam.ba)$dev.expl

concurv.ba.full <- concurvity(gam.ba, full=T)
concurv.ba <- concurvity(gam.ba, full=F)$estimate
corrplot(concurv.ba)

##################################
# QMD GAM
##################################
diam.mf <- make.modframe('diam', vars, 'gam', target.vars, itx=target.itx)

gam.diam <- gam(diam.mf$formula,
                  data=diam.mf$data,
                  method='REML',
                  select=T,
                  family='gaussian',
                  control=list(nthreads=nCores))

sum.diam <- summary(gam.diam)
concurv <- concurvity(gam.diam)
de.diam <- summary(gam.diam)$dev.expl

concurv.diam.full <- concurvity(gam.diam, full=T)
concurv.diam <- concurvity(gam.diam, full=F)$estimate
corrplot(concurv.diam)

##################################
# Height skew GAM
##################################
height.skew.mf <- make.modframe('height.skew', vars, 'gam', target.vars, itx=target.itx)

gam.height.skew <- gam(height.skew.mf$formula,
                  data=height.skew.mf$data,
                  method='REML',
                  select=T,
                  family='gamma',
                  control=list(nthreads=nCores))

sum.height.skew <- summary(gam.height.skew)
concurv <- concurvity(gam.height.skew)
de.height.skew <- summary(gam.height.skew)$dev.expl

concurv.height.skew.full <- concurvity(gam.height.skew, full=T)
concurv.height.skew <- concurvity(gam.height.skew, full=F)$estimate
corrplot(concurv.height.skew)

##################################
# Density GAM
##################################
density.mf <- make.modframe('density', vars, 'gam', target.vars, itx=target.itx)

gam.density <- gam(density.mf$formula,
                  data=density.mf$data,
                  method='REML',
                  select=T,
                  family='poisson',
                  control=list(nthreads=nCores))

sum.density <- summary(gam.density)
concurv <- concurvity(gam.density)
de.density <- summary(gam.density)$dev.expl

concurv.density.full <- concurvity(gam.density, full=T)
concurv.density <- concurvity(gam.density, full=F)$estimate
corrplot(concurv.density)

##################################
# ABLA density GAM
##################################
abla.density.mf <- make.modframe('abla.density', vars, 'gam', target.vars, itx=target.itx)

gam.abla.density <- gam(abla.density.mf$formula,
                  data=abla.density.mf$data,
                  method='REML',
                  select=T,
                  family='poisson',
                  control=list(nthreads=nCores))

sum.abla.density <- summary(gam.abla.density)
concurv <- concurvity(gam.abla.density)
de.abla.density <- summary(gam.abla.density)$dev.expl

concurv.abla.density.full <- concurvity(gam.abla.density, full=T)
concurv.abla.density <- concurvity(gam.abla.density, full=F)$estimate
corrplot(concurv.abla.density)

##################################
# PIEN density GAM
##################################
pien.density.mf <- make.modframe('pien.density', vars, 'gam', target.vars, itx=target.itx)

gam.pien.density <- gam(pien.density.mf$formula,
                  data=pien.density.mf$data,
                  method='REML',
                  select=T,
                  family='poisson',
                  control=list(nthreads=nCores))

sum.pien.density <- summary(gam.pien.density)
concurv <- concurvity(gam.pien.density)
de.pien.density <- summary(gam.pien.density)$dev.expl

concurv.pien.density.full <- concurvity(gam.pien.density, full=T)
concurv.pien.density <- concurvity(gam.pien.density, full=F)$estimate
corrplot(concurv.pien.density)

##################################
# PICO density GAM
##################################
pico.density.mf <- make.modframe('pico.density', vars, 'gam', target.vars, itx=target.itx)

gam.pico.density <- gam(pico.density.mf$formula,
                  data=pico.density.mf$data,
                  method='REML',
                  select=T,
                  family='poisson',
                  control=list(nthreads=nCores))

sum.pico.density <- summary(gam.pico.density)
concurv <- concurvity(gam.pico.density)
de.pico.density <- summary(gam.pico.density)$dev.expl

concurv.pico.density.full <- concurvity(gam.pico.density, full=T)
concurv.pico.density <- concurvity(gam.pico.density, full=F)$estimate
corrplot(concurv.pico.density)

#####################
# Aggregate objects
#####################
gam.objs <- list('Height 90p'=gam.height,
                 'Basal area'=gam.ba,
                 'QMD'=gam.diam,
                 'Height skew'=gam.height.skew,
                 'Density'=gam.density,
                 'ABLA_density'=gam.abla.density,
                 'PIEN_density'=gam.pien.density,
                 'PICO_density'=gam.pico.density)

lapply(seq_along(gam.objs), function(x) saveRDS(x, file.path('models', paste0(names(gam.objs)[x], '_gam.rda'))))

#############
# Summaries
#############

gam.pde <- c(de.height,
             de.ba,
             de.diam,
             de.height.skew,
             de.density,
             de.abla.density,
             de.pien.density,
             de.pico.density)

# gam.trmse <- c(trmse.gam.height,
#                trmse.gam.ba,
#                trmse.gam.diam,
#                trmse.gam.height.skew,
#                trmse.gam.density,
#                trmse.abla.density,
#                trmse.pien.density,
#                trmse.pico.density)


#############
# Table
#############

gam.perf.df <- as.data.frame(cbind('Response'=names(gam.objs),
                                   'PDE'= gam.pde),
                             check.names=F)

write.csv(gam.perf.df, file.path(config$extdata$scratch, 'gam_perf_df.csv'), row.names=F)

##################################
# Visualizations
##################################

mypal <- colorRampPalette(brewer.pal(6, "PuBu")[3:6])
mypal2 <- colorRampPalette(brewer.pal(8, "BuGn")[3:8])
mypal3 <- colorRampPalette(brewer.pal(6, "YlOrRd")[3:6])
mypal4 <- colorRampPalette(brewer.pal(6, "Greys")[5])

my_colors <- c("SWE" = mypal(4)[3],
               "∆SWE" = mypal(4)[4],
               "Soil AWC" = mypal3(4)[2],
               "Elevation" = mypal2(4)[1],
               "Soil total depth" = mypal3(4)[4],
               "Slope" = mypal2(4)[3],
               'Geology' = mypal4(1),
               'TPI' = mypal2(4)[4],
               'Soil Ksat' = mypal3(4)[3])

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
