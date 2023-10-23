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

gbm.height <- gbm(formula=height ~
            aet
            +elevation_10m
            +folded_aspect_205
            +slope
            +tpi_1km
            +twi_100m
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
            data=vars,
            n.trees=5000,
            cv.folds=10,
            shrinkage=0.05)

gbm.ba <- gbm(formula=ba ~
                    aet
                  +elevation_10m
                  +folded_aspect_205
                  +slope
                  +tpi_1km
                  +twi_100m
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
                  data=vars,
                  n.trees=5000,
                  cv.folds=10,
                  shrinkage=0.05)

gbm.diam <- gbm(formula=diam ~
                    aet
                  +elevation_10m
                  +folded_aspect_205
                  +slope
                  +tpi_1km
                  +twi_100m
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
                  data=vars,
                  n.trees=5000,
                  cv.folds=10,
                  shrinkage=0.05)

gbm.height.skew <- gbm(formula=height.skew ~
                    aet
                  +elevation_10m
                  +folded_aspect_205
                  +slope
                  +tpi_1km
                  +twi_100m
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
                  data=vars,
                  n.trees=5000,
                  cv.folds=10,
                  shrinkage=0.05)

gbm.density <- gbm(formula=density ~
                    aet
                  +elevation_10m
                  +folded_aspect_205
                  +slope
                  +tpi_1km
                  +twi_100m
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
                  data=vars,
                  n.trees=5000,
                  cv.folds=10,
                  shrinkage=0.05)

best.iter <- gbm.perf(gbm1, method = "OOB")
print(best.iter)

sqrt(min(gbm1$cv.error))

par(mar = c(5, 8, 1, 1))

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

gbm.summaries <- list('Height'=gbm.height.sum,
                      'Basal area'=gbm.ba.sum,
                      'QMD'=gbm.diam.sum,
                      'Height skew'=gbm.height.skew.sum,
                      'Density'=gbm.density.sum)

gbm.summaries <- bind_rows(gbm.summaries, .id='Model')

gbm.summaries <- gbm.summaries %>%
  mutate(Model=factor(Model),
         Category = case_when(var %in% c('elevation_10m', 'slope', 'folded_aspect_205',
                                         'heat_load', 'tpi_1km', 'twi_100m') ~ 'Topography',
                              var %in% c('td', 'om', 'k', 'awc') ~ 'Soil',
                              var %in% c('swe', 'delta_swe', 'cwd', 'aet') ~ 'Climate',
                              T ~ 'Lithology'))
# gbm.summaries.10 <- gbm.summaries %>%
#   group_by(Model) %>%
#   arrange(rel.inf)
#   top_n(n=10)
#
# View(gbm.summaries.10)

# Facet wrap top 10 variables
gbm.colors <- colorRampPalette(brewer.pal(8, "Blues"))(100)[24:100]

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

ggplot(gbm.summaries, aes(x=reorder_within(var, rel.inf, Model), y=rel.inf, fill=factor(rel.inf))) +
  geom_col() +
  scale_x_reordered() +
  coord_flip() +
  scale_fill_manual(values=gbm.colors, guide='none') +
  labs(x='Relative influence', y='Variable') +
  facet_wrap(~Model, scales='free_y') +
  theme_apa()

# Group by plot
gbm.summaries %>%
  group_by(Category) %>%
  arrange(var)

ggplot(gbm.summaries, aes(x=Model, y=rel.inf, fill=Category)) +
  geom_col(position='stack') +
  scale_fill_manual(values=unname(nifty::icolors('gothic'))) +
  #ggthemes::theme_calc() +
  labs(x='Relative influence', y='Variable') +
  theme_apa()

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

