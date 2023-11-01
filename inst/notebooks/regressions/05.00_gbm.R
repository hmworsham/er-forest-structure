# Script for generating generalized additive models on forest structure and explainers

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

source(file.path('~', 'Repos', 'er', 'er-forest-structure', 'inst', 'notebooks', 'regressions', '01.00_stats_ingest_data.R'))

##################################
# GBMs
##################################

# getModelInfo()$gbm$parameters
# library(parallel)
#
# # Max shrinkage for gbm
# nl = nrow(vars)
# max(0.01, 0.1*min(1, nl/10000))
# # Max Value for interaction.depth
# floor(sqrt(NCOL(vars)))
# gbmGrid <-  expand.grid(interaction.depth = c(1, 3, 5, 9, 10),
#                         n.trees = seq(2000, 10000, 2000),
#                         shrinkage = seq(.1, .01, -.01),
#                         n.minobsinnode = 10)
#
# fitControl <- trainControl(method = "repeatedcv",
#                            repeats = 5,
#                            preProcOptions = list(thresh = 0.95),
#                            ## Estimate class probabilities
#                            classProbs = F)
#
#
# set.seed(4673)
# system.time(gbm.height <- train(height ~
#                                 aet
#                                 +elevation_10m
#                                 +folded_aspect_205
#                                 +slope
#                                 +tpi_1km
#                                 +twi_100m
#                                 +heat_load
#                                 +awc
#                                 +om
#                                 +k
#                                 +td
#                                 +swe
#                                 +delta_swe
#                                 +geology
#                                 +cwd,
#                                 distribution='gaussian',
#                                 data=vars,
#                                 method = "gbm", bag.fraction = 0.5,
#                                 nTrain = round(nrow(vars) *.75),
#                                 na.action=na.pass,
#                                 trControl = fitControl,
#                                 verbose = F,
#                                 tuneGrid = gbmGrid,
#                                 ## Specify which metric to optimize
#                                 metric = "RMSE"))

##################################
# Height GBM
##################################
sample.int(10000, 5)
set.seed(9311)
height.train <- createDataPartition(y=vars$height, p=0.75, list=F)
train.height <- vars[height.train,]
test.height <- vars[-height.train,]

gbm.height <- gbm(formula=height ~
            aet
            +elevation_10m
            #+folded_aspect_205
            +slope
            +tpi_1km
            #+twi_100m
            +heat_load
            +awc
            +om
            +k
            +td
            +swe
            +delta_swe
            +geology
            +cwd,
            distribution='gaussian',
            data=train.height,
            n.trees=10000,
            cv.folds=10,
            shrinkage=0.01)

bi.height <- gbm.perf(gbm.height, method = "OOB")
cve.height <- sqrt(min(gbm.height$cv.error))

height.pred <- predict(object= gbm.height,
                       newdata = test.height,
                       n.trees= bi.height,
                       type='link')

plot(height.pred, test.height$height)
trmse.height <- RMSE(height.pred, test.height$height)

##################################
# BA GBM
##################################
sample.int(10000, 5)
set.seed(9311)
ba.train <- createDataPartition(y=vars$ba, p=0.75, list=F)
train.ba <- vars[ba.train,]
test.ba <- vars[-ba.train,]

gbm.ba <- gbm(formula=ba ~
                    aet
                  +elevation_10m
                  #+folded_aspect_205
                  +slope
                  +tpi_1km
                  #+twi_100m
                  +heat_load
                  +awc
                  +om
                  +k
                  +td
                  +swe
                  +delta_swe
                  +geology
                  +cwd,
                  distribution='gaussian',
                  data=train.ba,
                  n.trees=10000,
                  cv.folds=10,
                  shrinkage=0.01)

bi.ba <- gbm.perf(gbm.ba, method = "OOB")
cve.ba <- sqrt(min(gbm.ba$cv.error))

ba.pred <- predict(object= gbm.ba,
                       newdata = test.ba,
                       n.trees= bi.ba,
                       type='link')

plot(ba.pred, test.ba$ba)
trmse.ba <- RMSE(ba.pred, test.ba$ba)

##################################
# QMD GBM
##################################
sample.int(10000, 5)
set.seed(9311)
diam.train <- createDataPartition(y=vars$diam, p=0.75, list=F)
train.diam <- vars[diam.train,]
test.diam <- vars[-diam.train,]

gbm.diam <- gbm(formula=diam ~
                aet
              +elevation_10m
              #+folded_aspect_205
              +slope
              +tpi_1km
              #+twi_100m
              +heat_load
              +awc
              +om
              +k
              +td
              +swe
              +delta_swe
              +geology
              +cwd,
              distribution='gaussian',
              data=train.diam,
              n.trees=10000,
              cv.folds=10,
              shrinkage=0.01)

bi.diam <- gbm.perf(gbm.diam, method = "OOB")
cve.diam <- sqrt(min(gbm.diam$cv.error))

diam.pred <- predict(object= gbm.diam,
                   newdata = test.diam,
                   n.trees= bi.diam,
                   type='link')

plot(diam.pred, test.diam$diam)
trmse.diam <- RMSE(diam.pred, test.diam$diam)

##################################
# Height skew GBM
##################################
sample.int(10000, 5)
set.seed(9311)
height.skew.train <- createDataPartition(y=vars$height.skew, p=0.75, list=F)
train.height.skew <- vars[height.skew.train,]
test.height.skew <- vars[-height.skew.train,]

gbm.height.skew <- gbm(formula=height.skew ~
                aet
              +elevation_10m
              #+folded_aspect_205
              +slope
              +tpi_1km
              #+twi_100m
              +heat_load
              +awc
              +om
              +k
              +td
              +swe
              +delta_swe
              +geology
              +cwd,
              distribution='gaussian',
              data=train.height.skew,
              n.trees=10000,
              cv.folds=10,
              shrinkage=0.01)

bi.height.skew <- gbm.perf(gbm.height.skew, method = "OOB")
cve.height.skew <- sqrt(min(gbm.height.skew$cv.error))

height.skew.pred <- predict(object= gbm.height.skew,
                   newdata = test.height.skew,
                   n.trees= bi.height.skew,
                   type='link')

plot(height.skew.pred, test.height.skew$height.skew)
trmse.height.skew <- RMSE(height.skew.pred, test.height.skew$height.skew)

##################################
# Density GBM
##################################
sample.int(10000, 5)
set.seed(9311)
density.train <- createDataPartition(y=vars$density, p=0.75, list=F)
train.density <- vars[density.train,]
test.density <- vars[-density.train,]

gbm.density <- gbm(formula=density ~
                aet
              +elevation_10m
              #+folded_aspect_205
              +slope
              +tpi_1km
              #+twi_100m
              +heat_load
              +awc
              +om
              +k
              +td
              +swe
              +delta_swe
              +geology
              +cwd,
              distribution='gaussian',
              data=train.density,
              n.trees=10000,
              cv.folds=10,
              shrinkage=0.01)

bi.density <- gbm.perf(gbm.density, method = "OOB")
cve.density <- sqrt(min(gbm.density$cv.error))

density.pred <- predict(object= gbm.density,
                   newdata = test.density,
                   n.trees= bi.density,
                   type='link')

plot(density.pred, test.density$density)
trmse.density <- RMSE(density.pred, test.density$density)

#############
# Summaries
#############

gbm.height.sum <- summary(
  gbm.height,
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)

gbm.ba.sum <- summary(
  gbm.ba,
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)

gbm.diam.sum <- summary(
  gbm.diam,
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)

gbm.height.skew.sum <- summary(
  gbm.height.skew,
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)

gbm.density.sum <- summary(
  gbm.density,
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)

gbm.objs <- list('Height 90p'=gbm.height,
                 'Basal area'=gbm.ba,
                 'QMD'=gbm.diam,
                 'Height skew'=gbm.height.skew,
                 'Density'=gbm.density)

lapply(seq_along(gbm.objs), function(x) saveRDS(x, file.path('models', paste(names(gbm.objs)[x], '_gbm.rda'))))

gbm.summaries <- list('Height 90p'=gbm.height.sum,
                      'Basal area'=gbm.ba.sum,
                      'QMD'=gbm.diam.sum,
                      'Height skew'=gbm.height.skew.sum,
                      'Density'=gbm.density.sum)

gbm.summaries <- bind_rows(gbm.summaries, .id='Model')

gbm.cve <- c(cve.height,
                      cve.ba,
                      cve.diam,
                      cve.height.skew,
                      cve.density)

gbm.trmse <- c(trmse.height,
                  trmse.ba,
                  trmse.diam,
                  trmse.height.skew,
                  trmse.density)

##########
# Table
##########

gbm.perf.df <- as.data.frame(cbind('Response'=names(gbm.objs),
                      'CV error'= gbm.cve,
                      'Test error'=gbm.trmse),
                      check.names=F)

write.csv(gbm.perf.df, file.path(config$data$pro, 'gbm_perf_df.csv'), row.names=F)

##########
# Plots
##########

gbm.var.labs <- c(elevation_10m='Elevation', slope='Slope',
                  folded_aspect_205='Folded aspect',
                  heat_load='Heat load', tpi_1km='TPI', twi_100m='TWI',
                  td='Soil total depth', om='Soil % organic matter',
                  k='Soil K', awc='Soil AWC',
                  swe='SWE', delta_swe='∆ SWE', cwd='CWD', aet='AET',
                  geology='Geology')
gbm.var.labs <- rownames_to_column(data.frame(Variable=gbm.var.labs), 'var')

gbm.summaries <- gbm.summaries %>%
  mutate(Model=factor(Model, levels=c('Density', 'Height 90p',
                                      'QMD', 'Basal area', 'Height skew')),
         Category = case_when(var %in% c('elevation_10m', 'slope',
                                         'folded_aspect_205', 'heat_load',
                                         'tpi_1km', 'twi_100m') ~ 'Topography',
                              var %in% c('td', 'om', 'k', 'awc') ~ 'Soil',
                              var %in% c('swe', 'delta_swe', 'cwd', 'aet') ~ 'Climate',
                              T ~ 'Geology')) %>%
  left_join(gbm.var.labs, by='var')

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

