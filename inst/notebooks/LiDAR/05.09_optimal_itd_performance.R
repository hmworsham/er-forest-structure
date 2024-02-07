### ITC optimization using LayerStacking

## Workspace setup
## ---------------------------------------------------------------------------------------------------

# Load config
config <- config::get(file=file.path('~',
                                     'Repos',
                                     'er-forest-structure',
                                     'config',
                                     'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

# Configure Drive auth
drive_auth(path=config$drivesa)

## Load data: optimized segmented trees and matched trees
## ---------------------------------------------------------------------------------------------------

opt.trees <- list.files(file.path(config$extdata$itc, 'opt_trees'),
                        pattern='shp', full.names=T)
ls.trees <- lapply(opt.trees, st_read)

ls.match <- read.csv(file.path(config$extdata$itc, 'opt_matches.csv'))


## Estimate diameter and compute some summary stats on matched trees
## ---------------------------------------------------------------------------------------------------

a <- 0.8849
b <- 0.9102
dbh_est <- nthroot(ls.match$Zpred/a, b)

med.ht <- median(ls.match$Zpred, na.rm=T)
p90.ht <- quantile(ls.match$Zpred, .9, na.rm=T)
qmd <- sqrt(mean(dbh_est^2, na.rm=T))

## Compare detected and reference trees by plot
## ---------------------------------------------------------------------------------------------------

# Compare median height by site
ls.detect.med.l <- ls.match %>%
  filter(src==1) %>%
  group_by(site) %>%
  summarise(`All detected` = median(Zobs, na.rm=T))

ls.match.comp.l <- ls.match %>%
  filter(src==0) %>%
  group_by(site) %>%
  summarise(`Matched detected` = median(Zpred[!is.na(pred)], na.rm=T),
            `All reference` = median(Zobs, na.rm=T),
            `Matched reference` = median(Zobs[!is.na(pred)],na.rm=T)) %>%
  left_join(ls.detect.med.l, by='site') %>%
  pivot_longer(cols=c(`All detected`, `Matched detected`,
                      `All reference`,`Matched reference`)) %>%
  mutate(name=factor(name, levels=c('All detected', 'All reference',
                                    'Matched detected', 'Matched reference')))

# Compare 90th pctl height by site
ls.detect.90.l <- ls.match %>%
  filter(src==1) %>%
  group_by(site) %>%
  summarise(`All detected` = quantile(Zobs, .9, na.rm=T))

ls.match.comp.l.90 <- ls.match %>%
  filter(src==0) %>%
  group_by(site) %>%
  summarise(`Matched detected` = quantile(Zpred[!is.na(pred)], .9, na.rm=T),
            `All reference` = quantile(Zobs, .9, na.rm=T),
            `Matched reference` = quantile(Zobs[!is.na(pred)], .9, na.rm=T)) %>%
  left_join(ls.detect.med.l, by='site') %>%
  pivot_longer(cols=c(`All detected`, `Matched detected`,
                      `All reference`,`Matched reference`)) %>%
  mutate(name=factor(name, levels=c('All detected', 'All reference',
                                    'Matched detected', 'Matched reference')))

# Compare QMD by site
# TODO: 02-05-2024 - To make this happen, need to pull in field DBH for reference trees...
# ls.detect.qmd.l <- ls.match %>%
#   filter(src==1) %>%
#   mutate(DBH_est=nthroot(Zobs/a, b)) %>%
#   group_by(site) %>%
#   summarise(`All detected` = sqrt(mean(DBH_est^2, na.rm=T)))
#
# ls.match.qmd.l <- ls.match %>%
#   filter(src==0) %>%
#   mutate(DBH_est=nthroot(Zobs/a, b)) %>%
#   group_by(site) %>%
#   summarise(`Matched detected` = sqrt(mean(DBH_est[!is.na(pred)]^2, na.rm=T)), # Need to pull in field DBH
#             `All reference` = sqrt(mean(DBH^2, na.rm=T)),
#             `Matched reference` = sqrt(mean(DBH[!is.na(pred)]^2, na.rm=T))) %>% # Need to pull in field DBH
#   left_join(ls.detect.qmd.l, by='site') %>%
#   pivot_longer(cols=c(`All detected`, `Matched detected`,
#                       `All reference`,`Matched reference`)) %>%
#   mutate(name=factor(name, levels=c('All detected', 'All reference',
#                                     'Matched detected', 'Matched reference')))

# Plot: median height comparison barplot
hcomp.colors <- c(brewer.pal(9, name='Blues')[c(8,4)],
                  brewer.pal(9, name='Greens')[c(8,4)])

ggplot(ls.match.comp.l, aes(x=site, y=value, fill=factor(name))) +
  geom_col(position='dodge') +
  scale_fill_manual(values=hcomp.colors, name='Median height') +
  labs(x='Site', y='Median height (m)') +
  ggthemes::theme_calc(base_size=18)

# Plot: 90th pctl height comparison barplot
ggplot(ls.match.comp.l.90, aes(x=site, y=value, fill=name)) +
  geom_col(position='dodge') +
  scale_fill_manual(values=hcomp.colors, name='90th percentile height') +
  labs(x='Site', y='90th percentile of height (m)') +
  ggthemes::theme_calc(base_size=18)

# Plot: QMD comparison barplot
ggplot(ls.match.qmd.l, aes(x=site, y=value, fill=name)) +
  geom_col(position='dodge') +
  scale_fill_manual(values=hcomp.colors, name='Quadratic mean diameter') +
  labs(x='Site', y='90th percentile of height (m)') +
  ggthemes::theme_calc(base_size=18)

# Plot: kernel density

## Reformat matches
ls.match.l <- ls.match %>%
  filter(src==0 & !is.na(pair_id)) %>%
  mutate(pairid.site = paste(site, as.character(pair_id), sep='.')) %>%
  pivot_longer(cols=c(treeID, pred),
             names_to='source',
             values_to='treeID') %>%
  arrange(pair_id) %>%
  mutate(src = case_when(source=='pred' ~ 'Matched detected',
                         T ~ 'Matched reference')) %>%
  mutate(across(Zobs:Yobs, ~ ifelse(src=='Matched detected', NA, .)),
         across(Zpred:Ypred, ~ ifelse(src=='Matched reference', NA, .)),
         Z = coalesce(Zobs, Zpred),
         X = coalesce(Xobs, Xpred),
         Y = coalesce(Yobs, Ypred))

df.matched.plt.l <- ls.match.l %>%
  pivot_longer(cols=c(X,Y,Z),
                 names_to='dim')

# Reformat ALL detected
ls.detect.l <- ls.match %>%
  filter(src==1)  %>%
  mutate(src='All detected',
         Z=Zobs,
         X=Xobs,
         Y=Yobs) %>%
  pivot_longer(cols=c(X,Y,Z),
               names_to='dim')

# Reformat ALL observed
ls.ref.l <- ls.match %>%
  filter(src==0) %>%
  mutate(src='All reference',
         Z=Zobs,
         X=Xobs,
         Y=Yobs) %>%
  pivot_longer(cols=c(X,Y,Z),
               names_to='dim')

kdens.colors <- hcomp.colors
skill.density <- ggplot(df.matched.plt.l, aes(x=value, group=src, color=factor(src))) +
  stat_density(linewidth=1, geom='line', position='identity',
               aes(x=value, group=src, color=factor(src)), data=ls.detect.l) +
  stat_density(linewidth=1, geom='line', position='identity',
               aes(x=value, group=src, color=factor(src)), data=ls.ref.l) +
  stat_density(linewidth=1, geom='line', position='identity') +
  scale_color_manual(values=kdens.colors, name='Data source') +
  #scale_fill_manual(values=kdens.colors, name='Data source') +
  labs(x='Dimensional value', y='Kernel density') +
  facet_wrap(~dim, nrow=3, scales='free') +
  ggthemes::theme_calc(base_size=18)

skill.density
