# Script for generating generalized additive models on forest structure and explainers

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

##################################
# Source data
##################################

source(file.path('inst', 'notebooks',
                 'regressions', '01.00_stats_ingest_data.R'))

##################################
# Prep data
##################################

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
               'x, y'
               )

##################################
# Height GAM
##################################
height.mf <- make.modframe('height', vars, 'gam', target.vars, itx=target.itx)

gam.height <- gam(height.mf$formula,
                  data=height.mf$data,
                  method='REML',
                  select=T,
                  family='gaussian',
                  control=list(nthreads=nCores))

sum.height <- summary(gam.height)
concurv <- concurvity(gam.height)
de.height <- summary(gam.height)$dev.expl

concurv.height.full <- concurvity(gam.height, full=T)
concurv.height <- concurvity(gam.height, full=F)$estimate
corrplot(concurv.height)

saveRDS(gam.height, file.path('models', 'height_95p_gam.rda'))

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

saveRDS(gam.ba, file.path('models', 'ba_gam.rda'))

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

saveRDS(gam.diam, file.path('models', 'diam_gam.rda'))

##################################
# Height skew GAM
##################################
height.skew.mf <- make.modframe('height.skew', vars, 'gam', target.vars, itx=target.itx)

gam.height.skew <- gam(height.skew.mf$formula,
                  data=height.skew.mf$data,
                  method='REML',
                  select=T,
                  family='gaussian',
                  control=list(nthreads=nCores))

sum.height.skew <- summary(gam.height.skew)
concurv <- concurvity(gam.height.skew)
de.height.skew <- summary(gam.height.skew)$dev.expl

concurv.height.skew.full <- concurvity(gam.height.skew, full=T)
concurv.height.skew <- concurvity(gam.height.skew, full=F)$estimate
corrplot(concurv.height.skew)

saveRDS(gam.height.skew, file.path('models', 'height_skew_gam.rda'))

##################################
# Density GAM
##################################
density.mf <- make.modframe('density', vars, 'gam', target.vars, itx=target.itx)

gam.density <- gam(density.mf$formula,
                  data=density.mf$data,
                  method='REML',
                  select=T,
                  family='gaussian',
                  control=list(nthreads=nCores))

sum.density <- summary(gam.density)
concurv <- concurvity(gam.density)
de.density <- summary(gam.density)$dev.expl

concurv.density.full <- concurvity(gam.density, full=T)
concurv.density <- concurvity(gam.density, full=F)$estimate
corrplot(concurv.density)

saveRDS(gam.density, file.path('models', 'density_gam.rda'))

##################################
# ABLA density GAM
##################################
abla.density.mf <- make.modframe('abla_density', vars, 'gam', target.vars, itx=target.itx)

gam.abla.density <- gam(abla.density.mf$formula,
                  data=abla.density.mf$data,
                  method='REML',
                  select=T,
                  family='gaussian',
                  control=list(nthreads=nCores))

sum.abla.density <- summary(gam.abla.density)
concurv <- concurvity(gam.abla.density)
de.abla.density <- summary(gam.abla.density)$dev.expl

concurv.abla.density.full <- concurvity(gam.abla.density, full=T)
concurv.abla.density <- concurvity(gam.abla.density, full=F)$estimate
corrplot(concurv.abla.density)

saveRDS(gam.abla.density, file.path('models', 'abla_density_gam.rda'))

##################################
# PIEN density GAM
##################################
pien.density.mf <- make.modframe('pien_density', vars, 'gam', target.vars, itx=target.itx)

gam.pien.density <- gam(pien.density.mf$formula,
                  data=pien.density.mf$data,
                  method='REML',
                  select=T,
                  family='gaussian',
                  control=list(nthreads=nCores))

sum.pien.density <- summary(gam.pien.density)
concurv <- concurvity(gam.pien.density)
de.pien.density <- summary(gam.pien.density)$dev.expl

concurv.pien.density.full <- concurvity(gam.pien.density, full=T)
concurv.pien.density <- concurvity(gam.pien.density, full=F)$estimate
corrplot(concurv.pien.density)
saveRDS(gam.pien.density, file.path('models', 'pien_density_gam.rda'))

##################################
# PICO density GAM
##################################
pico.density.mf <- make.modframe('pico_density', vars, 'gam', target.vars, itx=target.itx)

gam.pico.density <- gam(pico.density.mf$formula,
                  data=pico.density.mf$data,
                  method='REML',
                  select=T,
                  family='gaussian',
                  control=list(nthreads=nCores))

sum.pico.density <- summary(gam.pico.density)
concurv <- concurvity(gam.pico.density)
de.pico.density <- summary(gam.pico.density)$dev.expl

concurv.pico.density.full <- concurvity(gam.pico.density, full=T)
concurv.pico.density <- concurvity(gam.pico.density, full=F)$estimate
corrplot(concurv.pico.density)

saveRDS(gam.pico.density, file.path('models', 'pico_density_gam.rda'))
gam.pico <- readRDS(file.path('models', 'height_95p_gam.rda'))

#####################
# Bulk write objects
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
