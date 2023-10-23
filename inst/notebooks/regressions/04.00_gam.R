# Script for generating generalized additive models on forest structure and explainers

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

source(file.path('~', 'Repos', 'er', 'er-forest-structure', 'inst', 'notebooks', 'regressions', '01.00_stats_ingest_data.R'))

##################################
# EDA
##################################

dens.long <- vars %>%
  pivot_longer(cols=folded_aspect_205:cwd)

ggplot(dens.long, aes(x=value, y=diam)) +
  geom_point(shape=3, size=0.5, color='dodgerblue', alpha=0.5, data=dens.long%>%sample_frac(0.5)) +
  geom_smooth(color='red', se=T) +
  facet_wrap(~name, scales='free') +
  labs(title='QMD') +
  ggthemes::theme_calc()

ggplot(subset(dens.long, !is.na(geology)), aes(x=geology, y=height)) +
  geom_boxplot() +
  ggthemes::theme_calc()

vars %>%
  group_by(geology) %>%
  summarise(n=n())

##################################
# GAM 1 - couple of explainers
##################################
mod_gam1 <- gam(density ~
                  s(swe, bs='cc', k=6) +
                  s(folded_aspect_205, bs='cs') +
                  s(elevation_10m, bs=)
                , data=vars)

opar <- par()
par(mfcol=c(3,1), mar=rep(4,4))
plot(mod_gam1)
visreg(mod_gam1)
par(opar)

vis.gam(mod_gam1,
        view=c('swe','folded_aspect_205'),
        type='response',
        plot.type='persp',
        phi=10,
        theta=24,
        border=NA,
        color='heat',
        zlab='stem density')

##################################
# GAM 2 - many explainers
##################################

mod_gam2 <- gam(density ~
                  s(aet, bs='cc', k=10) +
                  s(elevation_10m, bs='cc') +
                  s(folded_aspect_205, bs='cc') +
                  s(slope, bs='cc') +
                  s(tpi_1km, bs='cc') +
                  s(twi_100m, bs='cc') +
                  s(heat_load, bs='cc') +
                  s(elevation_10m, by=folded_aspect_205) +
                  s(elevation_10m, by = tpi_1km) +
                  s(elevation_10m, by = swe) +
                  # s(folded_aspect_205, by = tpi_1km) +
                  # s(awc, by=geology) +
                  #s(swe, by=geology) +
                  # s(folded_aspect_205, by=geology) +
                  # s(tpi_1km, by=geology) +
                  geology +
                  s(awc, bs='cc') +
                  s(om, bs='cc') +
                  s(k, bs='cc') +
                  #s(ksat, bs='cc') +
                  s(td, bs='cc') +
                  s(swe, bs='cc') +
                  s(delta_swe, bs='cc') +
                  s(cwd, bs='cc'),
                data=vars)

summary(mod_gam2)
as_flextable(mod_gam2)

par(mfrow=c(4,4), mar=c(4,1,2,1))
plot(mod_gam2)
par(opar)

mod.frame <- formula(density ~
                  s(aet, bs='cc', k=10) +
                  s(elevation_10m, bs='cc') +
                  s(folded_aspect_205, bs='cc') +
                  s(slope, bs='cc') +
                  s(tpi_1km, bs='cc') +
                  s(twi_100m, bs='cc') +
                  s(heat_load, bs='cc') +
                  s(elevation_10m, by=folded_aspect_205) +
                  s(elevation_10m, by = tpi_1km) +
                  s(elevation_10m, by = swe) +
                  s(elevation_10m, by=awc) +
                  geology +
                  s(awc, bs='cc') +
                  s(om, bs='cc') +
                  s(k, bs='cc') +
                  s(td, bs='cc') +
                  s(swe, bs='cc') +
                  s(delta_swe, bs='cc') +
                  s(cwd, bs='cc'),
                data=vars)

dens.gam <- gam(mod.frame)

ht_gam <- gam(height ~
                s(aet, bs='cc', k=10) +
                s(elevation_10m, bs='cc') +
                s(folded_aspect_205, bs='cc') +
                s(slope, bs='cc') +
                s(tpi_1km, bs='cc') +
                s(twi_100m, bs='cc') +
                s(heat_load, bs='cc') +
                s(elevation_10m, by=folded_aspect_205) +
                s(elevation_10m, by = tpi_1km) +
                s(elevation_10m, by = swe) +
                # s(folded_aspect_205, by = tpi_1km) +
                # s(awc, by=geology) +
                #s(swe, by=geology) +
                # s(folded_aspect_205, by=geology) +
                # s(tpi_1km, by=geology) +
                geology +
                s(awc, bs='cc') +
                s(om, bs='cc') +
                s(k, bs='cc') +
                #s(ksat, bs='cc') +
                s(td, bs='cc') +
                s(swe, bs='cc') +
                s(delta_swe, bs='cc') +
                s(cwd, bs='cc'),
              data=vars)

diam_gam <- gam(diam ~
                  s(elevation, bs='cc') +
                  s(folded_aspect_205, bs='cc') +
                  s(slope, bs='cc') +
                  s(tpi_1km, bs='cc') +
                  s(twi_100m, bs='cc') +
                  s(heat_load, bs='cc') +
                  s(elevation, by=folded_aspect_205) +
                  s(elevation, by = tpi_1km) +
                  s(elevation, by = swe) +
                  s(folded_aspect_205, by = tpi_1km) +
                  #s(elevation, by=geology) +
                  #s(folded_aspect_205, by=geology) +
                  #s(tpi_1km, by=geology) +
                  geology +
                  s(awc, bs='cc') +
                  s(om, bs='cc') +
                  s(ksat, bs='cc') +
                  #cec +
                  s(td, bs='cc') +
                  s(swe, bs='cc'),
                data=vars)

ba_gam <- gam(ba ~
                  s(elevation, bs='cc') +
                  s(folded_aspect_205, bs='cc') +
                  s(slope, bs='cc') +
                  s(tpi_1km, bs='cc') +
                  #s(twi_100m, bs='cc') +
                  s(heat_load, bs='cc') +
                  s(elevation, by=folded_aspect_205) +
                  s(elevation, by = tpi_1km) +
                  s(elevation, by = swe) +
                  s(folded_aspect_205, by = tpi_1km) +
                  #s(elevation, by=geology) +
                  #s(folded_aspect_205, by=geology) +
                  #s(tpi_1km, by=geology) +
                  geology +
                  s(awc, bs='cc') +
                  s(om, bs='cc') +
                  s(ksat, bs='cc') +
                  #cec +
                  s(td, bs='cc') +
                  s(swe, bs='cc'),
                data=vars)

summary(mod_gam2)
summary(dens_gam)
summary(ht_gam)
as_flextable(ht_gam)
summary(diam_gam)
summary(ba_gam)
coef(dens_gam)
plot.gam(dens_gam)
plot.gam(ht_gam)
plot.gam(diam_gam)
plot.gam(ba_gam)


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

visreg(mod_gam2)

termplot(dens_gam)

AIC(mod_lm)
AIC(mod_gam2)

summary(mod_lm)$sp.criterion
summary(mod_gam2)$sp.criterion

g <- list(
visreg2d(dens_gam, xvar='elevation', yvar='swe', plot.type='gg') +
  scale_fill_viridis(name='density') +
  theme_minimal(base_size = 16) +
  theme(legend.title=element_text(size=14), legend.position = 'bottom'),
visreg2d(ht_gam, xvar='elevation', yvar='ksat', plot.type='gg') +
  scale_fill_viridis(name='height') +
  theme_minimal(base_size = 16) +
  theme(legend.title=element_text(size=14), legend.position = 'bottom'),
visreg2d(diam_gam, xvar='elevation', yvar='heat_load', plot.type='gg') +
  scale_fill_viridis(name='dbh') +
  theme_minimal(base_size = 16) +
  theme(legend.title=element_text(size=14), legend.position = 'bottom'),
visreg2d(ba_gam, xvar='elevation', yvar='om', plot.type='gg') +
  scale_fill_viridis(name='basal area') +
  theme_minimal(base_size = 16) +
  theme(legend.title=element_text(size=14), legend.position = 'bottom')
)
g
g + facet_grid()
library(gridExtra)
marrangeGrob(g, nrow=2, ncol=2)
?marrangeGrob()
mod_gam2$coefficients
par(mfcol=c(1,3), mar=c(2,2,4,2))
vis.gam(mod_gam2, view=c('elevation','swe'), type='response', plot.type='persp', phi=20, theta=48, border=NA, color='topo', zlab='90th pctl height', contour.col='black', main='Density v Elevation v TPI', xlab='Standardized elevation', ylab='standardized TPI', cex.lab=2, cex.main=2.5)

summary(mod_gam2)
summary(mod_lm)$r.sq
summary(mod_gam1)$r.sq

#############################
# Linear mixed effects model
#############################

mod_lme <- lmer(density ~ elevation+slope+folded_aspect_205+tpi_1km+twi_100m+(1|geology), data=vars)
summary(mod_lme)
confint(mod_lme)
ranef(mod_lme)$geol
coef(mod_lme)$geol
vars %>%
  ggplot(aes(x=elevation, y=density)) +
  geom_point(aes(color=geology), alpha=.50) +
  scale_color_brewer(type='div')

?scale_fill_brewer
