# Script for generating generalized additive models on forest structure and explainers

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

source(file.path('~', 'Repos', 'er', 'er-forest-structure', 'inst', 'notebooks', 'regressions', '01.00_stats_ingest_data.R'))


##################################
# Height GAM
##################################
sample.int(10000, 5)
set.seed(6099)
height.train <- createDataPartition(y=vars$height, p=0.75, list=F)
train.height <- vars[height.train,]
test.height <- vars[-height.train,]

gam.height <- gam(height ~
                  s(elevation_10m, bs='tp') +
                  #s(folded_aspect_205, bs='cc') +
                  s(slope, bs='tp') +
                  s(tpi_1km, bs='tp') +
                  #s(twi_100m, bs='cc') +
                  s(heat_load, bs='tp') +
                  s(awc, bs='tp') +
                  s(om, bs='tp') +
                  s(k, bs='tp') +
                  #s(ksat, bs='cc') +
                  s(td, bs='cc') +
                  s(swe, bs='cc') +
                  s(delta_swe, bs='cc') +
                  s(awc, bs='tp') +
                  s(om, bs='tp') +
                  s(k, bs='tp') +
                  #s(ksat, bs='cc') +
                  s(td, bs='cc') +
                  s(swe, bs='cc') +
                  s(delta_swe, bs='cc') +
                  s(cwd, bs='cc') +
                  s(cwd, bs='cc') +
                  s(elevation_10m, by = tpi_1km, bs='tp') +
                  s(elevation_10m, by = swe, bs='tp') +
                  s(elevation_10m, by = delta_swe, bs='tp') +
                  s(elevation_10m, by = awc, bs='tp') +
                  geology,
                data=train.height,
                family='gaussian')

de.height <- summary(gam.height)$dev.expl
gam.height.pred <- predict(object=gam.height,
                       newdata=test.height,
                       se.fit=T)
plot(gam.height.pred$fit, test.height$height)
trmse.gam.height <- RMSE(gam.height.pred$fit, test.height$height, na.rm=T)
trmse.gam.height

##################################
# BA GAM
##################################
sample.int(10000, 5)
set.seed(6099)
ba.train <- createDataPartition(y=vars$ba, p=0.75, list=F)
train.ba <- vars[ba.train,]
test.ba <- vars[-ba.train,]

gam.ba <- gam(ba ~
                    s(elevation_10m, bs='tp') +
                    #s(folded_aspect_205, bs='cc') +
                    s(slope, bs='tp') +
                    s(tpi_1km, bs='tp') +
                    #s(twi_100m, bs='cc') +
                    s(heat_load, bs='tp') +
                    s(awc, bs='tp') +
                    s(om, bs='tp') +
                    s(k, bs='tp') +
                    #s(ksat, bs='cc') +
                    s(td, bs='cc') +
                    s(swe, bs='cc') +
                    s(delta_swe, bs='cc') +
                    s(awc, bs='tp') +
                    s(om, bs='tp') +
                    s(k, bs='tp') +
                    #s(ksat, bs='cc') +
                    s(td, bs='cc') +
                    s(swe, bs='cc') +
                    s(delta_swe, bs='cc') +
                    s(cwd, bs='cc') +
                    s(cwd, bs='cc') +
                    s(elevation_10m, by = tpi_1km, bs='tp') +
                    s(elevation_10m, by = swe, bs='tp') +
                    s(elevation_10m, by = delta_swe, bs='tp') +
                    s(elevation_10m, by = awc, bs='tp') +
                    geology,
                  data=train.ba,
                  family='gaussian')
sum.ba <- summary(gam.ba)
sum.ba
de.ba <- summary(gam.ba)$dev.expl
de.ba
gam.ba.pred <- predict(object=gam.ba,
                           newdata=test.ba,
                           se.fit=T)
plot(gam.ba.pred$fit, test.ba$ba)
trmse.gam.ba <- RMSE(gam.ba.pred$fit, test.ba$ba, na.rm=T)
trmse.gam.ba


##################################
# QMD GAM
##################################
sample.int(10000, 5)
set.seed(6099)
diam.train <- createDataPartition(y=vars$diam, p=0.75, list=F)
train.diam <- vars[diam.train,]
test.diam <- vars[-diam.train,]

gam.diam <- gam(diam ~
                s(elevation_10m, bs='tp') +
                #s(folded_aspect_205, bs='cc') +
                s(slope, bs='tp') +
                s(tpi_1km, bs='tp') +
                #s(twi_100m, bs='cc') +
                s(heat_load, bs='tp') +
                s(awc, bs='tp') +
                s(om, bs='tp') +
                s(k, bs='tp') +
                #s(ksat, bs='cc') +
                s(td, bs='cc') +
                s(swe, bs='cc') +
                s(delta_swe, bs='cc') +
                s(awc, bs='tp') +
                s(om, bs='tp') +
                s(k, bs='tp') +
                #s(ksat, bs='cc') +
                s(td, bs='cc') +
                s(swe, bs='cc') +
                s(delta_swe, bs='cc') +
                s(cwd, bs='cc') +
                s(cwd, bs='cc') +
                s(elevation_10m, by = tpi_1km, bs='tp') +
                s(elevation_10m, by = swe, bs='tp') +
                s(elevation_10m, by = delta_swe, bs='tp') +
                s(elevation_10m, by = awc, bs='tp') +
                geology,
              data=train.diam,
              family='gaussian')

sum.diam <- summary(gam.diam)
sum.diam
de.diam <- summary(gam.diam)$dev.expl
gam.diam.pred <- predict(object=gam.diam,
                       newdata=test.diam,
                       se.fit=T)
plot(gam.diam.pred$fit, test.diam$diam)
trmse.gam.diam <- RMSE(gam.diam.pred$fit, test.diam$diam, na.rm=T)
trmse.gam.diam

##################################
# Height skew GAM
##################################
sample.int(10000, 5)
set.seed(6099)
height.skew.train <- createDataPartition(y=vars$height.skew, p=0.75, list=F)
train.height.skew <- vars[height.skew.train,]
test.height.skew <- vars[-height.skew.train,]

gam.height.skew <- gam(height.skew ~
                s(elevation_10m, bs='tp') +
                #s(folded_aspect_205, bs='cc') +
                s(slope, bs='tp') +
                s(tpi_1km, bs='tp') +
                #s(twi_100m, bs='cc') +
                s(heat_load, bs='tp') +
                s(awc, bs='tp') +
                s(om, bs='tp') +
                s(k, bs='tp') +
                #s(ksat, bs='cc') +
                s(td, bs='cc') +
                s(swe, bs='cc') +
                s(delta_swe, bs='cc') +
                s(awc, bs='tp') +
                s(om, bs='tp') +
                s(k, bs='tp') +
                #s(ksat, bs='cc') +
                s(td, bs='cc') +
                s(swe, bs='cc') +
                s(delta_swe, bs='cc') +
                s(cwd, bs='cc') +
                s(cwd, bs='cc') +
                s(elevation_10m, by = tpi_1km, bs='tp') +
                s(elevation_10m, by = swe, bs='tp') +
                s(elevation_10m, by = delta_swe, bs='tp') +
                s(elevation_10m, by = awc, bs='tp') +
                geology,
              data=train.height.skew,
              family='gaussian')

sum.height.skew <- summary(gam.height.skew)
sum.height.skew
de.height.skew <- summary(gam.height.skew)$dev.expl
gam.height.skew.pred <- predict(object=gam.height.skew,
                       newdata=test.height.skew,
                       se.fit=T)
plot(gam.height.skew.pred$fit, test.height.skew$height.skew)
trmse.gam.height.skew <- RMSE(gam.height.skew.pred$fit, test.height.skew$height.skew, na.rm=T)
trmse.gam.height.skew

##################################
# Density GAM
##################################
sample.int(10000, 5)
set.seed(6099)
density.train <- createDataPartition(y=vars$density, p=0.75, list=F)
train.density <- vars[density.train,]
test.density <- vars[-density.train,]

gam.density <- gam(density ~
                s(elevation_10m, bs='tp') +
                #s(folded_aspect_205, bs='cc') +
                s(slope, bs='tp') +
                s(tpi_1km, bs='tp') +
                #s(twi_100m, bs='cc') +
                s(heat_load, bs='tp') +
                s(awc, bs='tp') +
                s(om, bs='tp') +
                s(k, bs='tp') +
                #s(ksat, bs='cc') +
                s(td, bs='cc') +
                s(swe, bs='cc') +
                s(delta_swe, bs='cc') +
                s(awc, bs='tp') +
                s(om, bs='tp') +
                s(k, bs='tp') +
                #s(ksat, bs='cc') +
                s(td, bs='cc') +
                s(swe, bs='cc') +
                s(delta_swe, bs='cc') +
                s(cwd, bs='cc') +
                s(cwd, bs='cc') +
                s(elevation_10m, by = tpi_1km, bs='tp') +
                s(elevation_10m, by = swe, bs='tp') +
                s(elevation_10m, by = delta_swe, bs='tp') +
                s(elevation_10m, by = awc, bs='tp') +
                geology,
              data=train.density,
              family='gaussian')

sum.density <- summary(gam.density)
de.density <- summary(gam.density)$dev.expl
gam.density.pred <- predict(object=gam.density,
                       newdata=test.density,
                       se.fit=T)
plot(gam.density.pred$fit, test.density$density)
trmse.gam.density <- RMSE(gam.density.pred$fit, test.density$density, na.rm=T)
trmse.gam.density


#####################
# Aggregate objects
#####################
gam.objs <- list('Height 90p'=gam.height,
                 'Basal area'=gam.ba,
                 'QMD'=gam.diam,
                 'Height skew'=gam.height.skew,
                 'Density'=gam.density)

lapply(seq_along(gam.objs), function(x) saveRDS(x, file.path('models', paste(names(gbm.objs)[x], '_gam.rda'))))

#############
# Summaries
#############

# gbm.summaries <- list('Height 90p'=gam.height.sum,
#                       'Basal area'=gam.ba.sum,
#                       'QMD'=gam.diam.sum,
#                       'Height skew'=gam.height.skew.sum,
#                       'Density'=gam.density.sum)

gam.pde <- c(de.height,
             de.ba,
             de.diam,
             de.height.skew,
             de.density)

gam.trmse <- c(trmse.gam.height,
               trmse.gam.ba,
               trmse.gam.diam,
               trmse.gam.height.skew,
               trmse.gam.density)


#############
# Table
#############

gam.perf.df <- as.data.frame(cbind('Response'=names(gam.objs),
                                   'PDE'= gam.pde,
                                   'Test error'=gam.trmse),
                             check.names=F)

write.csv(gam.perf.df, file.path(config$data$pro, 'gam_perf_df.csv'), row.names=F)

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
               'Soil K' = mypal3(4)[3])

# Density
dens.long <- vars %>%
  dplyr::select(c(density, delta_swe, swe, awc, elevation_10m)) %>%
  pivot_longer(cols=delta_swe:elevation_10m)

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
height.long <- vars %>%
  dplyr::select(c(height, delta_swe, swe, awc, elevation_10m, td)) %>%
  pivot_longer(cols=delta_swe:td)

pe.height <-  ggplot(height.long, aes(x=value, y=height, color=name)) +
  geom_smooth(formula=y~s(x, bs="tp"),
              se=T) +
  scale_color_manual(values = unname(my_colors[c('Soil AWC', '∆SWE', 'Elevation',
                                                 'SWE', 'Soil total depth')]),
                     labels=c('Soil AWC', '∆SWE', 'Elevation',
                              'SWE', 'Soil total depth')) +
  labs(title='Height',
       x='Standardized values',
       y='Maximum height (m)') +
  jtools::theme_apa()

diam.long <- vars %>%
  dplyr::select(c(diam, delta_swe, swe, awc, elevation_10m, td)) %>%
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
  dplyr::select(c(ba, delta_swe, swe, awc, elevation_10m, k)) %>%
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
  dplyr::select(c(height.skew, delta_swe, swe, awc, tpi_1km, td)) %>%
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


gridExtra::grid.arrange(pe.density, pe.height, pe.diam,
                        pe.ba, pe.height.skew,
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
