# Script for generating generalized additive models on forest structure and explainers

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

source(file.path('~', 'Repos', 'er', 'er-forest-structure', 'inst', 'notebooks', 'regressions', '01.00_stats_ingest_data.R'))

##################################
# # GAM 1 - couple of explainers
##################################
library(gbm)
gbm1 <- gbm(formula=ba ~
            elevation
            +slope
            +folded_aspect_205
            +tpi_1km
            +twi_100m
            +awc
            +om
            +ph
            #+k
            +geology
            +aet
            +cwd,
            distribution='gaussian',
            data=vars,
            n.trees=5000,
            cv.folds=10,
            shrinkage=0.05)

summary(gbm1)

best.iter <- gbm.perf(gbm1, method = "OOB")
print(best.iter)

sqrt(min(gbm1$cv.error))

par(mar = c(5, 8, 1, 1))
summary(
  gbm1,
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)

print(best.iter)

ice <- gbm1 %>%
  partial(
    pred.var = 'elevation',
    n.trees = 5000,
    grid.resolution = 100,
    ice = TRUE
  ) %>%
  autoplot(rug = TRUE, train = ames_train, alpha = .1, center = TRUE) +
  ggtitle("Centered") +
  scale_y_continuous(labels = scales::dollar)

par(mfrow = c(1, 2))
summary(gbm1, n.trees = 1)          # using first tree
summary(gbm1, n.trees = best.iter)  # using estimated best number of trees

# Construct univariate partial dependence plots
plot(gbm1, i.var = 1, n.trees = best.iter)
plot(gbm1, i.var = 2, n.trees = best.iter)
plot(gbm1, i.var = 3, n.trees = best.iter)  # can use index or name

