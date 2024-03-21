# Script for generating generalized additive models on forest structure and explainers

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

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
               'heat_load, cwd',
               'x, y')

make.modframe <- function(y, df, target.vars, itx=c('none')) {
  re <- df[y]
  x <- df[target.vars]
  fv <- target.vars[sapply(x, is.factor)]
  tv <- target.vars[!target.vars %in% fv &
                      !target.vars %in% c('x', 'y')]
  mm <- cbind(re,x)

  if(all(itx=='all')) {
    itx = apply(apply(combn(names(vars),2), 1, paste), 1, \(x) paste(x, collapse=', '))
  } else if(all(itx=='none')) {
    itx = NULL
  } else {
    itx = itx
  }

  ff <- paste(y, '~',
              paste(c(
                paste0('s(', tv,
                       ', bs="tp")'),
                fv),
                collapse='+')
  )

  if(!is.null(itx)) {
    ff <- paste(c(ff,
                  paste0('ti(', itx,
                         ', bs="tp")')),
                collapse='+')
  }

  ff <- as.formula(ff)
  return(list('formula'=ff, 'data'=mm))

}

##################################
# Height GAM
##################################
height.mf <- make.modframe('height', vars, target.vars, itx=target.itx)

# height.train <- createDataPartition(y=vars$height, p=0.75, list=F)
# train.height <- vars[height.train,]
# test.height <- vars[-height.train,]

gam.height.11 <- gam(height.mf$formula,
           data=height.mf$data,
           method='REML',
           select=T,
           family='gaussian',
           control=list(nthreads=18))

sum.height <- summary(gam.height)
concurv <- concurvity(gam.height)
de.height <- summary(gam.height)$dev.expl

concurv.height <- concurvity(gam.height, full=F)$estimate
corrplot(concurv.height)

library(gratia)
library(tidyr)

get.pdp <- function(v, mod) {
  if(!v=='geology') {
    data <- model.frame(mod)
    meds <- apply(data ,2, \(x) median(as.numeric(x), na.rm=T))
    r.max <- max(data[v])
    r.min <- min(data[v])
    geol.mode <- modal(height.mf$data$geology, na.rm=T)
    df <- data.frame(pseq=seq(-4.99,5,0.01))
    df <- cbind(df, t(data.frame(meds)))
    df$geology <- geol.mode
    df[(df$pseq<r.min | df$pseq>r.max),] <- NA
    df <- df[!names(df)==v]
    names(df)[1] <- v
    pred.v <- predict(mod, df, se.fit=T)
    slice.v <- data.frame(x=seq(-4.99,5,0.01),
                          fit=pred.v$fit,
                          u95=pred.v$fit+pred.v$se.fit,
                          l95=pred.v$fit-pred.v$se.fit)
    names(slice.v) <- c('x', paste0(v,'.fit'), paste0(v, '.u95'), paste0(v, '.l95'))
    slice.v
  }
}

slices <- lapply(target.vars, get.pdp, gam.height.11)
slices <- slices[!unlist(lapply(slices, is.null))]
slices.df <- reduce(slices, dplyr::left_join, by='x')

ggplot(slices.df) +
  geom_line(aes(x=x, y=elevation.fit)) +
  geom_line(aes(x=x, y=elevation.u95)) +
  geom_line(aes(x=x, y=elevation.l95))

fl <- slices.df %>%
  pivot_longer(cols=contains('fit'),
               names_to='var',
               values_to='fit') %>%
  mutate(var=str_replace_all(var, '.fit', '')) %>%
  dplyr::select(c(x,var,fit))

ul <- slices.df %>%
  pivot_longer(cols=contains('u95'),
               names_to='var',
               values_to='u95') %>%
  mutate(var=str_replace_all(var, '.u95', '')) %>%
  dplyr::select(c(x,var, u95))

ll <- slices.df %>%
  pivot_longer(cols=contains('l95'),
               names_to='var',
               values_to='l95') %>%
  mutate(var=str_replace_all(var, '.l95', '')) %>%
  dplyr::select(c(x,var, l95))

slices.df.l <- reduce(list(fl,ul,ll), left_join, by=c('x', 'var'))

ggplot(slices.df.l) +
  geom_ribbon(aes(x=x, ymax=u95, ymin=l95, group=var, fill=var), alpha=0.2) +
  geom_line(aes(x=x, y=fit, color=var)) +
  # geom_line(aes(x=x, y=u95, color=var)) +
  # geom_line(aes(x=x, y=l95, color=var)) +
  facet_grid(~var)

library(rlang)
slice.el <- data_slice(gam.height.11, j=evenly({{}}, n=1000))
r.max <- max(slice.el$elevation)
r.min <- min(slice.el$elevation)
slice.el$elevation <- seq(-4.99,5,0.01)
slice.el[(slice.el$elevation<r.min | slice.el$elevation>r.max),] <- NA
pred.el <- predict(gam.height.11, slice.el, se.fit=T)
slice.el.pred <- data.frame(x=seq(-4.99,5,0.01), pred.el)

slice.hl <- data_slice(gam.height.11, heat_load=evenly(heat_load, n=500))
pred.hl <- predict(gam.height.11, slice.hl, se.fit=T)
slice.hl.pred <- data.frame(x=round(slice.hl$heat_load,2), pred.hl=pred.hl)

slice.el.pred %>%
  ggplot(aes(x=x, y=fit)) +
  geom_ribbon(aes(ymin=fit-se.fit, ymax=fit+se.fit), alpha=0.3, fill='blue') +
  geom_line() +
  labs(title=expression("Partial effect of" ~ s(elevation)))

slice.el.hl <- data_slice(gam.height.10, elevation=evenly(elevation, n=100), heat_load=evenly(heat_load, n=100))
pred.el.hl <- predict(gam.height, slice.el.hl)
summary(pred.el.hl)
slice.el.hl.pred <- cbind(slice.el.hl, est=pred.el.hl)

slice.el.hl.pred %>%
  ggplot(aes(y = elevation, heat_load=heat_load, z=est)) +
  geom_contour_filled() +
  labs(title = expression("Partial effect of" ~ s(elevation) + s(heat_load) + ti(elevation, heat_load)))

ggplot() +
  geom_contour_filled(data = slice_xz_pred, aes(y = z, x=x, z=est)) +
  geom_point(data=dat$data, aes(x=x, y=z)) +
  labs(title = expression("Partial effect of" ~ s(x) + s(z) + ti(s,z)))

slices <- lapply(target.vars, \(v) {
  sl = data_slice(gam.height.10, x=evenly(heat_load, n=100))
  #sl.pred = predict(gam.height.10, sl)
  #sl.pred
})

?data_slice

gam.height.smooth <- predict(gam.height, type='terms')
beta <- coef(gam.height)[grepl('\\(heat_load\\)', names(coef(gam.height)))]
s <- gam.height.smooth[,grepl('\\(heat_load\\)', colnames(gam.height.smooth))] %*% beta
ggplot(data=cbind.data.frame(s, height.mf$data$heat_load), aes(x=height.mf$data$heat_load, y=s)) + geom_line()

gam.height.pred <- predict(object=gam.height,
                       newdata=test.height,
                       se.fit=T)

plot(gam.height.pred$fit, test.height$height)
trmse.gam.height <- RMSE(gam.height.pred$fit, test.height$height, na.rm=T)

test.height <- cbind(test.height, data.frame('yhat'=gam.height.pred$fit))

##################################
# BA GAM
##################################
lin <- sample.int(10000, 1)
set.seed(lin)
ba.train <- createDataPartition(y=vars$ba, p=0.75, list=F)
train.ba <- vars[ba.train,]
test.ba <- vars[-ba.train,]

gam.ba <- gam(ba ~
                s(elevation, bs='tp') +
                s(folded_aspect_205, bs='cc') +
                s(slope, bs='tp') +
                s(tpi, bs='tp') +
                s(twi, bs='cc') +
                s(heat_load, bs='tp') +
                s(curvature, bs='tp') +
                s(awc, bs='tp') +
                s(om, bs='tp') +
                #s(k, bs='tp') +
                s(ksat, bs='cc') +
                s(td, bs='cc') +
                s(swe, bs='cc') +
                s(delta_swe, bs='cc') +
                s(awc, bs='tp') +
                s(om, bs='tp') +
                s(cwd, bs='cc') +
                s(aet, bs='cc') +
                s(elevation, by = tpi, bs='tp') +
                s(elevation, by = swe, bs='tp') +
                s(elevation, by = delta_swe, bs='tp') +
                s(elevation, by = awc, bs='tp') +
                s(elevation, by = aet, bs='tp') +
                geology,
              data=train.ba,
              select=T,
              family='gaussian')

de.ba <- summary(gam.ba)$dev.expl
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
                  s(elevation, bs='tp') +
                  s(folded_aspect_205, bs='cc') +
                  s(slope, bs='tp') +
                  s(tpi, bs='tp') +
                  s(twi, bs='cc') +
                  s(heat_load, bs='tp') +
                  s(curvature, bs='tp') +
                  s(awc, bs='tp') +
                  s(om, bs='tp') +
                  #s(k, bs='tp') +
                  s(ksat, bs='cc') +
                  s(td, bs='cc') +
                  s(swe, bs='cc') +
                  s(delta_swe, bs='cc') +
                  s(awc, bs='tp') +
                  s(om, bs='tp') +
                  s(cwd, bs='cc') +
                  s(aet, bs='cc') +
                  s(elevation, by = tpi, bs='tp') +
                  s(elevation, by = swe, bs='tp') +
                  s(elevation, by = delta_swe, bs='tp') +
                  s(elevation, by = awc, bs='tp') +
                  s(elevation, by = aet, bs='tp') +
                  geology,
                data=train.diam,
                select=T,
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
                         s(elevation, bs='tp') +
                         s(folded_aspect_205, bs='cc') +
                         s(slope, bs='tp') +
                         s(tpi, bs='tp') +
                         s(twi, bs='cc') +
                         s(heat_load, bs='tp') +
                         s(curvature, bs='tp') +
                         s(awc, bs='tp') +
                         s(om, bs='tp') +
                         #s(k, bs='tp') +
                         s(ksat, bs='cc') +
                         s(td, bs='cc') +
                         s(swe, bs='cc') +
                         s(delta_swe, bs='cc') +
                         s(awc, bs='tp') +
                         s(om, bs='tp') +
                         s(cwd, bs='cc') +
                         s(aet, bs='cc') +
                         s(elevation, by = tpi, bs='tp') +
                         s(elevation, by = swe, bs='tp') +
                         s(elevation, by = delta_swe, bs='tp') +
                         s(elevation, by = awc, bs='tp') +
                         s(elevation, by = aet, bs='tp') +
                         geology,
                       data=train.height.skew,
                       select=T,
                       family='gaussian')

sum.height.skew <- summary(gam.height.skew)
de.height.skew <- summary(gam.height.skew)$dev.expl
gam.height.skew.pred <- predict(object=gam.height.skew,
                       newdata=test.height.skew,
                       se.fit=T)
plot(gam.height.skew.pred$fit, test.height.skew$height.skew)
trmse.gam.height.skew <- RMSE(gam.height.skew.pred$fit, test.height.skew$height.skew, na.rm=T)

##################################
# Density GAM
##################################
sample.int(10000, 5)
set.seed(6099)
density.train <- createDataPartition(y=vars$density, p=0.75, list=F)
train.density <- vars[density.train,]
test.density <- vars[-density.train,]

gam.density <- gam(density ~
                     s(elevation, bs='tp') +
                     s(folded_aspect_205, bs='cc') +
                     s(slope, bs='tp') +
                     s(tpi, bs='tp') +
                     s(twi, bs='cc') +
                     s(heat_load, bs='tp') +
                     s(curvature, bs='tp') +
                     s(awc, bs='tp') +
                     s(om, bs='tp') +
                     #s(k, bs='tp') +
                     s(ksat, bs='cc') +
                     s(td, bs='cc') +
                     s(swe, bs='cc') +
                     s(delta_swe, bs='cc') +
                     s(awc, bs='tp') +
                     s(om, bs='tp') +
                     s(cwd, bs='cc') +
                     s(aet, bs='cc') +
                     s(elevation, by = tpi, bs='tp') +
                     s(elevation, by = swe, bs='tp') +
                     s(elevation, by = delta_swe, bs='tp') +
                     s(elevation, by = awc, bs='tp') +
                     s(elevation, by = aet, bs='tp') +
                     geology,
                   data=train.density,
                   select=T,
                   family='gaussian')

sum.density <- summary(gam.density)
de.density <- summary(gam.density)$dev.expl
gam.density.pred <- predict(object=gam.density,
                       newdata=test.density,
                       se.fit=T)
plot(gam.density.pred$fit, test.density$density)
trmse.gam.density <- RMSE(gam.density.pred$fit, test.density$density, na.rm=T)
trmse.gam.density

##################################
# ABLA density GAM
##################################
sample.int(10000, 5)
set.seed(6099)
abla.vars <- vars[!is.na(vars$abla_density),]
abla.density.train <- createDataPartition(y=abla.vars$abla_density, p=0.75, list=F)
train.abla.density <- abla.vars[abla.density.train,]
test.abla.density <- abla.vars[-abla.density.train,]

gam.abla.density <- gam(abla_density ~
                     s(elevation, bs='tp') +
                     s(folded_aspect_205, bs='cc') +
                     s(slope, bs='tp') +
                     s(tpi, bs='tp') +
                     s(twi, bs='cc') +
                     s(heat_load, bs='tp') +
                     s(curvature, bs='tp') +
                     s(awc, bs='tp') +
                     s(om, bs='tp') +
                     #s(k, bs='tp') +
                     s(ksat, bs='cc') +
                     s(td, bs='cc') +
                     s(swe, bs='cc') +
                     s(delta_swe, bs='cc') +
                     s(awc, bs='tp') +
                     s(om, bs='tp') +
                     s(cwd, bs='cc') +
                     s(aet, bs='cc') +
                     s(elevation, by = tpi, bs='tp') +
                     s(elevation, by = swe, bs='tp') +
                     s(elevation, by = delta_swe, bs='tp') +
                     s(elevation, by = awc, bs='tp') +
                     s(elevation, by = aet, bs='tp') +
                     geology,
                   data=train.height,
                   select=T,
                   family='gaussian')

sum.abla.density <- summary(gam.abla.density)
de.abla.density <- summary(gam.abla.density)$dev.expl
gam.abla.density.pred <- predict(object=gam.abla.density,
                            newdata=test.abla.density,
                            se.fit=T)
plot(gam.abla.density.pred$fit, test.abla.density$abla.density)
trmse.gam.abla.density <- RMSE(gam.abla.density.pred$fit, test.abla.density$abla.density, na.rm=T)
trmse.gam.abla.density

##################################
# PIEN density GAM
##################################
sample.int(10000, 5)
set.seed(6099)
pien.vars <- vars[!is.na(vars$pien_density),]
pien.density.train <- createDataPartition(y=pien.vars$pien_density, p=0.75, list=F)
train.pien.density <- pien.vars[pien.density.train,]
test.pien.density <- pien.vars[-pien.density.train,]

gam.pien.density <- gam(pien_density ~
                          s(elevation, bs='tp') +
                          s(folded_aspect_205, bs='cc') +
                          s(slope, bs='tp') +
                          s(tpi, bs='tp') +
                          s(twi, bs='cc') +
                          s(heat_load, bs='tp') +
                          s(curvature, bs='tp') +
                          s(awc, bs='tp') +
                          s(om, bs='tp') +
                          #s(k, bs='tp') +
                          s(ksat, bs='cc') +
                          s(td, bs='cc') +
                          s(swe, bs='cc') +
                          s(delta_swe, bs='cc') +
                          s(awc, bs='tp') +
                          s(om, bs='tp') +
                          s(cwd, bs='cc') +
                          s(aet, bs='cc') +
                          s(elevation, by = tpi, bs='tp') +
                          s(elevation, by = swe, bs='tp') +
                          s(elevation, by = delta_swe, bs='tp') +
                          s(elevation, by = awc, bs='tp') +
                          s(elevation, by = aet, bs='tp') +
                          geology,
                        data=train.height,
                        select=T,
                        family='gaussian')

sum.pien.density <- summary(gam.pien.density)
de.pien.density <- summary(gam.pien.density)$dev.expl
gam.pien.density.pred <- predict(object=gam.pien.density,
                                 newdata=test.pien.density,
                                 se.fit=T)
plot(gam.pien.density.pred$fit, test.pien.density$pien.density)
trmse.gam.pien.density <- RMSE(gam.pien.density.pred$fit, test.pien.density$pien.density, na.rm=T)
trmse.gam.pien.density

##################################
# PICO density GAM
##################################
sample.int(10000, 5)
set.seed(6099)
pico.vars <- vars[!is.na(vars$pico_density),]
pico.density.train <- createDataPartition(y=pico.vars$pico_density, p=0.75, list=F)
train.pico.density <- pico.vars[pico.density.train,]
test.pico.density <- pico.vars[-pico.density.train,]

gam.pico.density <- gam(pico_density ~
                          s(elevation, bs='tp') +
                          s(folded_aspect_205, bs='cc') +
                          s(slope, bs='tp') +
                          s(tpi, bs='tp') +
                          s(twi, bs='cc') +
                          s(heat_load, bs='tp') +
                          s(curvature, bs='tp') +
                          s(awc, bs='tp') +
                          s(om, bs='tp') +
                          #s(k, bs='tp') +
                          s(ksat, bs='cc') +
                          s(td, bs='cc') +
                          s(swe, bs='cc') +
                          s(delta_swe, bs='cc') +
                          s(awc, bs='tp') +
                          s(om, bs='tp') +
                          s(cwd, bs='cc') +
                          s(aet, bs='cc') +
                          s(elevation, by = tpi, bs='tp') +
                          s(elevation, by = swe, bs='tp') +
                          s(elevation, by = delta_swe, bs='tp') +
                          s(elevation, by = awc, bs='tp') +
                          s(elevation, by = aet, bs='tp') +
                          geology,
                        data=train.height,
                        select=T,
                        family='gaussian')

sum.pico.density <- summary(gam.pico.density)
de.pico.density <- summary(gam.pico.density)$dev.expl
gam.pico.density.pred <- predict(object=gam.pico.density,
                                 newdata=test.pico.density,
                                 se.fit=T)
plot(gam.pico.density.pred$fit, test.pico.density$pico.density)
trmse.gam.pico.density <- RMSE(gam.pico.density.pred$fit, test.pico.density$pico.density, na.rm=T)
trmse.gam.pico.density

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

gam.trmse <- c(trmse.gam.height,
               trmse.gam.ba,
               trmse.gam.diam,
               trmse.gam.height.skew,
               trmse.gam.density,
               de.abla.density,
               de.pien.density,
               de.pico.density)


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
