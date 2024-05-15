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

varnames[varnames$label=='\xc6SWE', 'label'] <- '\u0394SWE'

#############
# Summaries
#############

# Best models in tuning
gbm.best <- bind_rows(sapply(gbms, '[', 'bestTune', simplify=T), .id='Response') %>%
  mutate(Response=str_replace_all(Response, '.bestTune', ''))
names(gbm.best) <- c('Response', 'N trees', 'Interaction depth',
                     'Shrinkage', 'Min obs. in node')

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

# Cumu RI of least important explainers
gbm.sums %>%
  group_by(Model) %>%
  top_n(-11, wt=rel.inf) %>%
  summarise(sum=sum(rel.inf))

# What were the 3 least important explainers?
print(gbm.sums %>%
  group_by(Model) %>%
  top_n(-3, wt=rel.inf),
  n=30)

# Cumu RI of soil explainers
gbm.sums %>%
  filter(category=='Soil') %>%
  group_by(Model) %>%
  summarise(sum=sum(rel.inf))

# Max RI of geology
gbm.sums %>%
  filter(category=='Geology') %>%
  group_by(Model) %>%
  summarise(sum=sum(rel.inf))

# Max RI of 11 least important explainers
gbm.sums %>%
  group_by(Model) %>%
  top_n(-11, wt=rel.inf) %>%
  summarise(maxri=max(rel.inf))

######################
# Plots
######################

# Group by plot
gbm.summaries.cat <- gbm.sums %>%
  mutate(category = factor(category, levels=c('Climate', 'Topography',
                                              'Soil', 'Geology')),
         label = factor(label,
                        levels=c('AET', 'CWD', 'SWE', '\u0394SWE',
                                        'Curvature', 'Elevation', 'Heat load',
                                        'TPI', 'TWI',
                                        'AWC', 'CEC', 'ksat',
                                        'Organic matter', 'pH', 'Silt content',
                                        'Geology'),
                        labels=c('AET', 'CWD', 'SWE', '\u0394SWE',
                                 'Curvature', 'Elevation', 'Heat load',
                                 'TPI', 'TWI',
                                 'AWC', 'CEC', expression(k['sat']),
                                 'Organic matter', 'pH', 'Silt content',
                                 'Geology')),
         var = factor(var)) %>%
  arrange(category, label)

soil.labs <- c('AWC', 'CEC', expression(k['sat']),
'Organic matter', 'pH', 'Silt content')

# Plot importance for each structure metric

p1 <- ggplot(gbm.summaries.cat, aes(x=Model, y=rel.inf, fill=interaction(category, label, sep=': '))) +
  geom_col(position='stack') +
  scale_fill_manual(limits=interaction(gbm.summaries.cat$category, gbm.summaries.cat$label, sep=': '),
                    values=gbm.summaries.cat$pdcolors, name=NULL) +
  #scale_fill_manual(values=unname(nifty::icolors('pantone')[c(1,4,5,7)])) +
  #scale_fill_manual(values = c(mypal(4), mypal2(4), mypal3(4), mypal4(1))) +
  labs(x='Response', y='Relative influence (%)') +
  ggthemes::theme_calc(base_size=8,
                       base_family='Arial') +
  guides(fill = guide_legend(byrow = TRUE)) +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        legend.position='none',
        # legend.direction = 'horizontal',
        legend.spacing.y = unit(0.1, 'mm'),
        # legend.text = element_text(size=rel(0.7)),
        legend.key.size = unit(0.8, 'lines'),
        legend.margin = margin(r=1, l=0, t=10, b=10),
        plot.background = element_rect(color = NA)
        )

p1

gbm.sum.tbl <- split(gbm.summaries.cat, gbm.summaries.cat$category)
# gbm.sum.tbl <- lapply(gbm.sum.tbl, \(x) {
#   #x$label=as.character(x$label)
#   x %>%
#     arrange(x, label, Model)
#   x})

gbm.col.tbl <- lapply(gbm.sum.tbl, \(x) {
  x %>%
    select(label, pdcolors) %>%
    deframe()
})

p1.leg <- ggplot(mapping=aes(x=Model, y=rel.inf)) +
  geom_col(data=gbm.sum.tbl$Climate, position='stack', aes(fill=label)) +
  scale_fill_manual(values=gbm.col.tbl$Climate,
                    name='Climate',
                    guide=guide_legend(order=1)) +
  ggnewscale::new_scale_fill() +
  geom_col(data=gbm.sum.tbl$Topography, position='stack', aes(fill=label)) +
  scale_fill_manual(values=gbm.col.tbl$Topography,
                    name='Topography',
                    guide=guide_legend(order=2)) +
  ggnewscale::new_scale_fill() +
  geom_col(data=gbm.sum.tbl$Soil, position='stack', aes(fill=label)) +
  scale_fill_manual(values=gbm.col.tbl$Soil,
                    name='Soil',
                    guide=guide_legend(order=3),
                    labels=soil.labs) +
  ggnewscale::new_scale_fill() +
  geom_col(data=gbm.sum.tbl$Geology, position='stack', aes(fill=label)) +
  scale_fill_manual(values=gbm.col.tbl$Geology,
                    name='Geology',
                    guide=guide_legend(order=4)) +
  theme(legend.position = "left",
        legend.box = "vertical",
        legend.direction='vertical',
        legend.box.just = "left",
        legend.title = element_text(size=rel(1))) +
  labs(x='Model', y='Relative influence (%)') +
  ggthemes::theme_calc(base_size=7,
                       base_family='Arial') +
  guides(fill = guide_legend(byrow = TRUE)) +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        legend.position='left',
        # legend.direction = 'horizontal',
        legend.spacing.y = unit(0.1, 'mm'),
        legend.text = element_text(hjust=0),
        # legend.text = element_text(size=rel(0.7)),
        legend.key.size = unit(0.8, 'lines'),
        legend.margin = margin(r=1, l=1, t=2, b=5),
        legend.title = element_text(size=rel(1),
                                    face='bold'),
        #panel.background = element_rect(color = NA)
        plot.background = element_rect(color = NA)
  )

p1.leg <- g_legend(p1.leg)

# Facet wrap top 10 variables

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

gbm.summaries.5.all <- gbm.summaries.5[!gbm.summaries.5$Model %in%
                                         c('Fir density', 'Spruce density',
                                           'Pine density'),]

p2 <- ggplot(gbm.summaries.5.all, aes(x=reorder_within(label, rel.inf, Model), y=rel.inf,
                                  fill=label)) +
  geom_col() +
  scale_x_reordered() +
  coord_flip() +
  scale_fill_manual(limits=gbm.summaries.5$label, values=gbm.summaries.5$pdcolors,
                    guide='none') +
  labs(x='Abiotic explanatory variable', y='Relative influence (%)') +
  facet_wrap(~Model, scales='free_y') +
  ggthemes::theme_calc(base_size=8,
                       base_family='Arial')

p2

gbm.summaries.5.spp <- gbm.summaries.5[gbm.summaries.5$Model %in%
                                         c('Fir density', 'Spruce density',
                                           'Pine density'),]

p3 <- ggplot(gbm.summaries.5.spp, aes(x=reorder_within(label, rel.inf, Model),
                                      y=rel.inf, fill=label)) +
  geom_col() +
  scale_x_reordered() +
  coord_flip() +
  scale_fill_manual(limits=gbm.summaries.5$label, values=gbm.summaries.5$pdcolors,
                    guide='none') +
  labs(x='Abiotic explanatory variable', y='Relative influence (%)') +
  facet_wrap(~Model, scales='free_y') +
  ggthemes::theme_calc(base_size=8,
                       base_family='Arial')

#gridExtra::grid.arrange(p2, p1, ncol=2, widths=c(6,5))

# loadfonts(device='pdf')
# setEPS()
# postscript('~/Desktop/Fig6.eps', width=190/25.4, height=160/25.4,
#             onefile=T, family='Arial Unicode MS', paper='special', bg='white', horizontal=T)
#
# pdf('~/Desktop/Fig6.pdf', width=190/25.4, height=100/25.4, onefile=T,
#     family='Arial', bg='white', paper='special', encoding='ISOLatin2.enc')
#

cairo_pdf('~/Desktop/Fig6.pdf', width=190/25.4, height=120/25.4, onefile=T,
          family='Arial', bg='white')

grid.newpage()

vp1 <- viewport(x = 0, y = 0,
                height = 1, width = 0.12,
                just = c("left", "bottom"),
                name = "leg1")
pushViewport(vp1)
#grid.rect()
grid.draw(p1.leg)
grid.text(expression(bold('(A)')), x=0.04, y=0.92, just=c('left', 'bottom'))

upViewport(1)
vp2 <- viewport(x=0.12, y=0,
                height=1, width=0.38,
                just = c('left', 'bottom'),
                name='p1')
pushViewport(vp2)
print(p1, newpage=F)

upViewport(1)
vp3 <- viewport(x=1, y=0.4,
                height=0.6, width=0.5,
                just=c('right', 'bottom'),
                name='p2')
pushViewport(vp3)
#grid.rect()
print(p2, newpage=F)
grid.text(expression(bold('(B)')), x=0.02, y=0.93, just=c('left', 'bottom'))

upViewport(1)
vp4 <- viewport(x=1, y=0,
                height=0.4, width=0.5,
                just=c('right', 'bottom'),
                name='p3')
pushViewport(vp4)
#grid.rect()
print(p3, newpage=F)
grid.text(expression(bold('(C)')), x=0.02, y=0.94, just=c('left', 'bottom'))


upViewport(1)
vp5 <- viewport(x=0, y=0,
                height=1, width=0.5,
                just=c('left', 'bottom'),
                name='vp4')
pushViewport(vp5)
grid.rect(gp=gpar(fill=NA))

dev.off()

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
