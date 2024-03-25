# Script for generating generalized boosted models on forest structure and explainers

##################################
# Set up workspace
##################################

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

###################################
# Read saved models and prep data
###################################
gbms.rda <- list.files('models', pattern='gbm', full.names=T)
gbms <- lapply(gbms.rda, readRDS)
gbm.names <- names(gbms) <- tools::file_path_sans_ext(basename(gbms.rda))
gbm.names.new <- data.frame('o'=gbm.names,
                        'n'=c('Fir density',
                              'Basal area',
                              'Total density',
                              'QMD',
                              'Height 95P',
                              'Height skew',
                              'Pine density',
                              'Spruce density'))
gbm.names.new
x1 <- match(names(gbms), gbm.names.new$o)
names(gbms) <- gbm.names.new$n[x1]

# Read variable names reference table
varnames <- read.csv(file.path(config$data$int, 'explainer_names_table.csv'),
                     row.names=1)

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
gbm.testerr <- unlist(lapply(gbms, \(x) {
  testerr <- sqrt(min(x$finalModel$valid.error, na.rm=T))
  testerr
}))

# Cross-validation error
gbm.cverr <- unlist(lapply(gbms, \(x) {
  gbmres <- x$results
  gbmres <- gbmres[order(as.numeric(row.names(gbmres))),]
  gbmres <- gbmres[which.min(gbmres$RMSE),]
  gbmres$RMSE
}))

##########
# Table
##########

gbm.perf.df <- data.frame(cbind('Response'=gbm.names,
                                'Train error'=gbm.trainerr,
                                'CV error'= gbm.cverr),
                          check.names=F)

# write.csv(gbm.perf.df, file.path(config$extdata$scratch, 'gbm_performance', 'gbm_perf_df.csv'), row.names=F)

######################
# Relative influence
######################

gbm.sums <- left_join(gbm.sums, varnames, by=c('var'='varnames'))

gbm.sums <- gbm.sums %>%
  mutate(Model=factor(Model, levels=c('Basal area',
                                      'Height 95P',
                                      'Height skew',
                                      'QMD',
                                      'Total density',
                                      'Fir density',
                                      'Spruce density',
                                      'Pine density')))

gbm.summaries.5 <- gbm.sums %>%
  group_by(Model) %>%
  top_n(5, wt=rel.inf) %>%
  mutate(ranking=rank(rel.inf))

gbm.sums %>%
  group_by(Model) %>%
  summarise(n=n())

gbm.summaries.5 %>%
  group_by(Model) %>%
  summarise(sum=sum(rel.inf))

gbm.sums %>%
  group_by(Model) %>%
  top_n(-8, wt=rel.inf) %>%
  summarise(sum=sum(rel.inf))

######################
# Plots
######################

# Facet wrap top 10 variables

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

p1 <- ggplot(gbm.summaries.5, aes(x=reorder_within(label, rel.inf, Model), y=rel.inf,
                                  fill=label)) +
  geom_col() +
  scale_x_reordered() +
  coord_flip() +
  scale_fill_manual(limits=gbm.summaries.5$label, values=gbm.summaries.5$pdcolors,
                    guide='none') +
  labs(x='Predictor variable', y='Relative influence') +
  facet_wrap(~Model, scales='free_y') +
  ggthemes::theme_calc(base_size=18)
# +
# theme(axis.text = element_text(size = 12),
#       plot.title = element_text(size=12),
#       strip.text = element_text(size=14),
#       strip.text.x = element_text(size=14),
#       strip.text.y = element_text(size=14))

p1

# Group by plot
gbm.summaries.cat <- gbm.sums %>%
  mutate(category = factor(category, levels=c('Climate', 'Topography',
                                              'Soil', 'Geology')),
         label = factor(label, levels=c('AET', 'CWD', 'SWE', '∆SWE',
                                         'Curvature', 'Elevation', 'Heat load',
                                            'TPI', 'TWI',
                                         'AWC', 'CEC', 'Organic matter',
                                            'Silt content','ksat', 'pH',
                                         'Geology')),
         var = factor(var)) %>%
  arrange(category, label)

# Plot importance for each structure metric

p2 <- ggplot(gbm.summaries.cat, aes(x=Model, y=rel.inf, fill=interaction(category, label, sep=': '))) +
  geom_col(position='stack') +
  scale_fill_manual(limits=interaction(gbm.summaries.cat$category, gbm.summaries.cat$label, sep=': '),
                    values=gbm.summaries.cat$pdcolors, name=NULL) +
  #scale_fill_manual(values=unname(nifty::icolors('pantone')[c(1,4,5,7)])) +
  #scale_fill_manual(values = c(mypal(4), mypal2(4), mypal3(4), mypal4(1))) +
  labs(x='Relative influence', y='Predictor variable') +
  ggthemes::theme_calc(base_size=18) +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        legend.position='left')

p2

gridExtra::grid.arrange(p2, p1, ncol=2, widths=c(6,5))

#############
# SCRATCH
#############

# Univariate partial dependence plots
plot(gbms[[7]]$finalModel, i.var = 1:3, n.trees = 2000)
plot(gbm1, i.var = 2, n.trees = 2000)
plot(gbm1, i.var = 3, n.trees = best.iter)  # can use index or name

# Color handling

cat.lens <- varnames %>%
  group_by(category) %>%
  count()

clim.pal <- colorRampPalette(brewer.pal(6, "PuBu")[3:6])
geol.pal <- colorRampPalette(brewer.pal(6, "BrBG")[1])
soil.pal <- colorRampPalette(brewer.pal(6, "YlOrRd")[3:6])
topo.pal <- colorRampPalette(brewer.pal(8, "BuGn")[3:8])

my.pal <- c(clim.pal(cat.lens[cat.lens$category=='Climate',]$n),
            geol.pal(cat.lens[cat.lens$category=='Geology',]$n),
            soil.pal(cat.lens[cat.lens$category=='Soil',]$n),
            topo.pal(cat.lens[cat.lens$category=='Topography',]$n))

varnames <- varnames %>%
  arrange(category, label) %>%
  mutate(pdcolors=my.pal)
