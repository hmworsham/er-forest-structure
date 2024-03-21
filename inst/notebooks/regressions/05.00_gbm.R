# Script for generating generalized boosted models on forest structure and explainers

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

source(file.path('inst', 'notebooks',
                 'regressions', '01.00_stats_ingest_data.R'))

# Specify cores
nCores <- as.integer(availableCores()-2)

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

##################################
# GBM controls
##################################

# Max shrinkage for gbm
nl = nrow(vars)
maxshrk <- max(0.01, 0.1*min(1, nl/10000))

# Max Value for interaction.depth
maxid <- floor(sqrt(NCOL(vars)))

# Make tuning grid
gbmGrid <-  expand.grid(interaction.depth = 1:maxid,
                        n.trees = seq(2000, 10000, 2000),
                        shrinkage = seq(.1, .01, -.02),
                        n.minobsinnode = 10)

fitControl <- trainControl(method="cv",
                           number=10,
                           #preProcOptions=list(thresh = 0.95),
                           classProbs = F)

##################################
# Height GBM
##################################
sample.int(10000, 5)
set.seed(9311)

height.mf <- make.modframe('height', vars, 'gbm', target.vars, itx='none')

cl <- makePSOCKcluster(nCores)
registerDoParallel(cl)

gbm.height <- train(height.mf$formula,
                    distribution='gaussian',
                    data=height.mf$data,
                    method = "gbm",
                    bag.fraction = 0.5,
                    nTrain = round(nrow(height.mf$data) *.75),
                    na.action=na.omit,
                    trControl = fitControl,
                    verbose = F,
                    tuneGrid = gbmGrid,
                    metric = "RMSE")

stopCluster(cl)

bi.height <- gbm.perf(gbm.height$finalModel, method = "OOB")
cve.height <- sqrt(min(gbm.height$finalModel$valid.error))

saveRDS(gbm.height, file.path('models', 'height_95p_gbm.rda'))

##################################
# BA GBM
##################################
sample.int(10000, 5)
set.seed(9311)

ba.mf <- make.modframe('ba', vars, 'gbm', target.vars, itx='none')

cl <- makePSOCKcluster(nCores)
registerDoParallel(cl)

gbm.ba <- train(ba.mf$formula,
                distribution='gaussian',
                data=ba.mf$data,
                method = "gbm",
                bag.fraction = 0.5,
                nTrain = round(nrow(ba.mf$data) *.75),
                na.action=na.omit,
                trControl = fitControl,
                verbose = F,
                tuneGrid = gbmGrid,
                metric = "RMSE")

stopCluster(cl)

bi.ba <- gbm.perf(gbm.ba$finalModel, method = "OOB")
cve.ba <- sqrt(min(gbm.ba$finalModel$valid.error))

saveRDS(gbm.ba, file.path('models', 'ba_gbm.rda'))

##################################
# QMD GBM
##################################
sample.int(10000, 5)
set.seed(9311)

diam.mf <- make.modframe('diam', vars, 'gbm', target.vars, itx='none')

cl <- makePSOCKcluster(nCores)
registerDoParallel(cl)

gbm.diam <- train(diam.mf$formula,
                  distribution='gaussian',
                  data=diam.mf$data,
                  method = "gbm",
                  bag.fraction = 0.5,
                  nTrain = round(nrow(diam.mf$data) *.75),
                  na.action=na.omit,
                  trControl = fitControl,
                  verbose = F,
                  tuneGrid = gbmGrid,
                  metric = "RMSE")

stopCluster(cl)

bi.diam <- gbm.perf(gbm.diam$finalModel, method = "OOB")
cve.diam <- sqrt(min(gbm.diam$finalModel$valid.error))

saveRDS(gbm.diam, file.path('models', 'diam_gbm.rda'))

##################################
# Height skew GBM
##################################
sample.int(10000, 5)
set.seed(9311)

height.skew.mf <- make.modframe('height.skew', vars, 'gbm', target.vars, itx='none')

cl <- makePSOCKcluster(nCores)
registerDoParallel(cl)

gbm.height.skew <- train(height.skew.mf$formula,
                         distribution='gaussian',
                         data=height.skew.mf$data,
                         method = "gbm",
                         bag.fraction = 0.5,
                         nTrain = round(nrow(height.skew.mf$data) *.75),
                         na.action=na.omit,
                         trControl = fitControl,
                         verbose = F,
                         tuneGrid = gbmGrid,
                         metric = "RMSE")

stopCluster(cl)

bi.height.skew <- gbm.perf(gbm.height.skew$finalModel, method = "OOB")
cve.height.skew <- sqrt(min(gbm.height.skew$finalModel$valid.error))

saveRDS(gbm.height.skew, file.path('models', 'height_skew_gbm.rda'))

##################################
# Density GBM
##################################
sample.int(10000, 5)
set.seed(9311)

density.mf <- make.modframe('density', vars, 'gbm', target.vars, itx='none')

cl <- makePSOCKcluster(nCores)
registerDoParallel(cl)

gbm.density <- train(density.mf$formula,
                     distribution='gaussian',
                     data=density.mf$data,
                     method = "gbm",
                     bag.fraction = 0.5,
                     nTrain = round(nrow(density.mf$data) *.75),
                     na.action=na.omit,
                     trControl = fitControl,
                     verbose = F,
                     tuneGrid = gbmGrid,
                     metric = "RMSE")

stopCluster(cl)

bi.density <- gbm.perf(gbm.density$finalModel, method = "OOB")
cve.density <- sqrt(min(gbm.density$finalModel$valid.error))

saveRDS(gbm.density, file.path('models', 'density_gbm.rda'))

##################################
# ABLA Density GBM
##################################
sample.int(10000, 5)
set.seed(9311)

abla.density.mf <- make.modframe('abla_density', vars, 'gbm', target.vars, itx='none')
abla.density.mf$data <- abla.density.mf$data[complete.cases(abla.density.mf$data),]
cl <- makePSOCKcluster(nCores)
registerDoParallel(cl)

gbm.abla.density <- train(abla.density.mf$formula,
                          distribution='gaussian',
                          data=abla.density.mf$data,
                          method = "gbm",
                          bag.fraction = 0.5,
                          nTrain = round(nrow(abla.density.mf$data) *.75),
                          na.action=na.omit,
                          trControl = fitControl,
                          verbose = F,
                          tuneGrid = gbmGrid,
                          metric = "RMSE")

stopCluster(cl)

bi.abla.density <- gbm.perf(gbm.abla.density$finalModel, method = "OOB")
cve.abla.density <- sqrt(min(gbm.abla.density$finalModel$valid.error))

saveRDS(gbm.abla.density, file.path('models', 'abla_density_gbm.rda'))

##################################
# PIEN Density GBM
##################################
sample.int(10000, 5)
set.seed(9311)

pien.density.mf <- make.modframe('pien_density', vars, 'gbm', target.vars, itx='none')
pien.density.mf$data <- pien.density.mf$data[complete.cases(pien.density.mf$data),]
cl <- makePSOCKcluster(nCores)
registerDoParallel(cl)

gbm.pien.density <- train(pien.density.mf$formula,
                          distribution='gaussian',
                          data=pien.density.mf$data,
                          method = "gbm",
                          bag.fraction = 0.5,
                          nTrain = round(nrow(pien.density.mf$data) *.75),
                          na.action=na.omit,
                          trControl = fitControl,
                          verbose = F,
                          tuneGrid = gbmGrid,
                          metric = "RMSE")

stopCluster(cl)

bi.pien.density <- gbm.perf(gbm.pien.density$finalModel, method = "OOB")
cve.pien.density <- sqrt(min(gbm.pien.density$finalModel$valid.error))

saveRDS(gbm.pien.density, file.path('models', 'pien_density_gbm.rda'))

##################################
# PICO Density GBM
##################################
sample.int(10000, 5)
set.seed(9311)

pico.density.mf <- make.modframe('pico_density', vars, 'gbm', target.vars, itx='none')

cl <- makePSOCKcluster(nCores)
registerDoParallel(cl)

gbm.pico.density <- train(pico.density.mf$formula,
                          distribution='gaussian',
                          data=pico.density.mf$data,
                          method = "gbm",
                          bag.fraction = 0.5,
                          nTrain = round(nrow(pico.density.mf$data) *.75),
                          na.action=na.omit,
                          trControl = fitControl,
                          verbose = F,
                          tuneGrid = gbmGrid,
                          metric = "RMSE")

stopCluster(cl)

bi.pico.density <- gbm.perf(gbm.pico.density$finalModel, method = "OOB")
cve.pico.density <- sqrt(min(gbm.pico.density$finalModel$valid.error))

saveRDS(gbm.pico.density, file.path('models', 'pico_density_gbm.rda'))


####################
# Read saved models
####################

gbms <- lapply(list.files(file.path('.', 'models'), pattern='rda', full.names=T), readRDS)
gbm.names <- str_replace_all(list.files(file.path('.', 'models'), pattern='_gbm.rda'),
                             '_gbm.rda', '')

#############
# Summaries
#############

# Relative influence data
gbm.sums <- lapply(gbms, \(x) {
  m.sum <- summary(x$finalModel,
  cBars=20,
  method=relative.influence,
  las=2)
  m.sum
})

gbm.sums <- bind_rows(gbm.sums, .id='Model')

gbm.geol <- gbm.sums %>%
  filter(grepl('geology', var)) %>%
  group_by(Model) %>%
  summarise(rel.inf=sum(rel.inf)) %>%
  mutate(var='geology')

gbm.sums <- gbm.sums %>%
  filter(!grepl('geology', var)) %>%
  rbind(gbm.geol) %>%
  arrange(Model, desc(rel.inf))

gbm.biplots <- lapply(gbms, \(x) {
  m.bi <- gbm.perf(x$finalModel, method='OOB')
  m.bi
})

# Train error
gbm.trainerr <- unlist(lapply(gbms, \(x) {
  trerr <- sqrt(min(x$finalModel$train.error, na.rm=T))
  trerr
}))

# Test error
gbm.cverr <- unlist(lapply(gbms, \(x) {
  cverr <- sqrt(min(x$finalModel$valid.error, na.rm=T))
  cverr
}))

##########
# Table
##########

gbm.perf.df <- as.data.frame(cbind('Response'=gbm.names,
                                   'Train error'=gbm.trainerr,
                                   'CV error'= gbm.cverr),
                             check.names=F)

write.csv(gbm.perf.df, file.path(config$data$pro, 'gbm_perf_df.csv'), row.names=F)

##########
# Plots
##########

varnames <- read.csv(file.path(config$data$int, 'explainer_names_table.csv'))

# gbm.var.labs <- c(elevation_10m='Elevation', slope='Slope',
#                   folded_aspect_205='Folded aspect',
#                   heat_load='Heat load', tpi_1km='TPI', twi_100m='TWI',
#                   td='Soil total depth', om='Soil % organic matter',
#                   k='Soil K', awc='Soil AWC',
#                   swe='SWE', delta_swe='∆ SWE', cwd='CWD', aet='AET',
#                   geology='Geology')
#
# gbm.var.labs <- rownames_to_column(data.frame(Variable=gbm.var.labs), 'var')
#
# gbm.summaries <- gbm.sums %>%
#   mutate(Model=factor(Model, levels=c('Density', 'Height 90p',
#                                       'QMD', 'Basal area', 'Height skew')),
#          Category = case_when(var %in% c('elevation_10m', 'slope',
#                                          'folded_aspect_205', 'heat_load',
#                                          'tpi_1km', 'twi_100m') ~ 'Topography',
#                               var %in% c('td', 'om', 'k', 'awc') ~ 'Soil',
#                               var %in% c('swe', 'delta_swe', 'cwd', 'aet') ~ 'Climate',
#                               T ~ 'Geology')) %>%
#   left_join(gbm.var.labs, by='var')

gbm.sums %>%

gbm.summaries.10 <- gbm.summaries %>%
  group_by(Model) %>%
  top_n(5, wt=rel.inf) %>%
  mutate(ranking=rank(rel.inf))

gbm.summaries %>%
  group_by(Model) %>%
  summarise(n=n())

gbm.summaries.10 %>%
  group_by(Model) %>%
  summarise(sum=sum(rel.inf))

gbm.summaries.bottom <- gbm.summaries %>%
  group_by(Model) %>%
  top_n(-8, wt=rel.inf) %>%
  summarise(sum=sum(rel.inf))

gbm.summaries.bottom

# Facet wrap top 10 variables
gbm.colors <- colorRampPalette(brewer.pal(8, "Greys"))(10)[6:10]

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

mypal <- colorRampPalette(brewer.pal(6, "PuBu")[3:6])
mypal2 <- colorRampPalette(brewer.pal(8, "BuGn")[3:8])
mypal3 <- colorRampPalette(brewer.pal(6, "YlOrRd")[3:6])
mypal4 <- colorRampPalette(brewer.pal(6, "Greys")[5])

my_colors <- c("SWE" = mypal(4)[3],
               "∆SWE" = mypal(4)[4],
               "Soil AWC" = mypal3(4)[2],
               "Elevation" = mypal2(6)[1],
               "Soil total depth" = mypal3(4)[4],
               "Slope" = mypal2(6)[4],
               'Geology' = mypal4(1))

p1 <- ggplot(gbm.summaries, aes(x=reorder_within(Variable, rel.inf, Model), y=rel.inf), fill='grey20') +
  geom_col() +
  scale_x_reordered() +
  coord_flip() +
  #scale_fill_manual(values=gbm.colors, guide='none') +
  #scale_fill_manual(values = c(mypal(4), mypal2(6), mypal3(4), mypal4(1))) +
  labs(x='Predictor variable', y='Relative influence') +
  facet_wrap(~Model, scales='free_y') +
  jtools::theme_apa()
# +
# theme(axis.text = element_text(size = 12),
#       plot.title = element_text(size=12),
#       strip.text = element_text(size=14),
#       strip.text.x = element_text(size=14),
#       strip.text.y = element_text(size=14))

p1

# Group by plot
gbm.summaries.cat <- gbm.summaries %>%
  mutate(Category = factor(Category, levels=c('Climate', 'Topography',
                                              'Soil', 'Geology')),
         Variable = factor(Variable, levels=c('AET', 'CWD', 'SWE', '∆ SWE',
                                              'Elevation', 'Folded aspect',
                                              'Heat load', 'Slope',
                                              'TPI', 'TWI', 'Soil % organic matter',
                                              'Soil AWC','Soil K',
                                              'Soil total depth', 'Geology')),
         var = factor(var)) %>%
  #group_by(Category) %>%
  arrange(Variable, Category)

# Plot importance for each structure metric

p2 <- ggplot(gbm.summaries.cat, aes(x=Model, y=rel.inf, fill=interaction(Category, Variable, sep=': '))) +
  geom_col(position='stack') +
  #scale_fill_manual(values=unname(nifty::icolors('pantone')[c(1,4,5,7)])) +
  scale_fill_manual(values = c(mypal(4), mypal2(4), mypal3(4), mypal4(1))) +
  labs(x='Relative influence', y='Predictor variable') +
  jtools::theme_apa()

p2
gridExtra::grid.arrange(p2, p1, ncol=2, widths=c(6,5))

# ice <- gbm.height %>%
#   partial(
#     pred.var = 'elevation',
#     n.trees = 5000,
#     grid.resolution = 100,
#     ice = TRUE
#   ) %>%
#   autoplot(rug = TRUE, train = ames_train, alpha = .1, center = TRUE) +
#   ggtitle("Centered") +
#   scale_y_continuous(labels = scales::dollar)
#
# par(mfrow = c(1, 2))
# summary(gbm1, n.trees = 1)          # using first tree
# summary(gbm1, n.trees = best.iter)  # using estimated best number of trees

# Construct univariate partial dependence plots
plot(gbm.height, i.var = 1, n.trees = best.iter)
plot(gbm1, i.var = 2, n.trees = best.iter)
plot(gbm1, i.var = 3, n.trees = best.iter)  # can use index or name

