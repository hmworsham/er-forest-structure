# Figure 3

#############################
# Set up working environment
#############################

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

#############################
# Ingest data
#############################

# Optimal ITD results
download.file('https://drive.google.com/uc?export=download&id=1Vnv4UGjPsVZSW5deBQE_1qjXo2Vq3K5P&usp=drive_fs',
              destfile=file.path(tempdir(), 'optimal_itd.tar.gz'),
              method='wget')
untar(file.path(tempdir(), 'optimal_itd.tar.gz'), exdir=file.path(tempdir(), 'optimal_itd'))
opt.itd <- untar(file.path(tempdir(), 'optimal_itd.tar.gz'), list=T)

## Tree objects (spatial)
ls.trees <- lapply(file.path(tempdir(), 'optimal_itd', opt.itd[grepl('shp', opt.itd)]), st_read)

## Match data (tabular)
ls.match <- read.csv(file.path(tempdir(), 'optimal_itd', 'opt_matches.csv'))

##########################################################
# Compare detected and reference trees by plot
##########################################################

# Compare median height by site
ls.detect.med.l <- ls.match %>%
  filter(src==1) %>%
  group_by(site) %>%
  summarise(`Median` = median(Zpred, na.rm=T),
            `U75` = quantile(Zpred, .75, na.rm=T),
            `L25` = quantile(Zpred, .25, na.rm=T)) %>%
  pivot_longer(cols=-site) %>%
  mutate(src='All LiDAR-detected trees')

ls.detect.match.med.l <- ls.match %>%
  filter(src==0) %>%
  group_by(site) %>%
  summarise(`Median` = median(Zpred[!is.na(pred)], na.rm=T),
            `U75` = quantile(Zpred[!is.na(pred)], .75, na.rm=T),
            `L25` = quantile(Zpred[!is.na(pred)], .25, na.rm=T)) %>%
  pivot_longer(cols=-site) %>%
  mutate(src='Matched LiDAR-detected trees')

ls.ref.med.l <- ls.match %>%
  filter(src==0) %>%
  group_by(site) %>%
  summarise(`Median` = median(Zobs, na.rm=T),
            `U75` = quantile(Zobs, .75, na.rm=T),
            `L25` = quantile(Zobs, .25, na.rm=T)) %>%
  pivot_longer(cols=-site) %>%
  mutate(src='All field-observed trees')

ls.refmatch.med.l <- ls.match %>%
  filter(src==0) %>%
  group_by(site) %>%
  summarise(`Median` = median(Zobs[!is.na(pred)], na.rm=T),
            `U75` = quantile(Zobs[!is.na(pred)], .75, na.rm=T),
            `L25` = quantile(Zobs[!is.na(pred)], .25, na.rm=T)) %>%
  pivot_longer(cols=-site) %>%
  mutate(src='Matched field-observed trees')

ls.match.comp.medh <- bind_rows(ls.detect.med.l, ls.detect.match.med.l,
                                ls.ref.med.l, ls.refmatch.med.l) %>%
  pivot_wider(names_from=c(name))

# Plot: median height comparison barplot
hcomp.colors <- c(brewer.pal(9, name='Blues')[c(6,8)],
                  brewer.pal(9, name='Greens')[c(4,8)])

hcomp.plt <- ggplot(ls.match.comp.medh, aes(x=site, y=Median, fill=factor(src))) +
  geom_col(position=position_dodge(width=0.75), width=0.75) +
  geom_errorbar(aes(x=site, ymin=L25, ymax=U75),
                position=position_dodge(width=0.75),
                width=0.5,
                linewidth=0.25,
                color='grey60') +
  scale_fill_manual(values=hcomp.colors,
                    name=NULL,
                    guide = guide_legend()) +
  labs(x='Site', y='Median height (m)') +
  ggthemes::theme_calc(base_size=8) +
  theme(legend.position = c(0, 1),
        legend.justification = c('left', 'top'),
        legend.key.size=unit(0.02, 'npc'),
        legend.box.background = element_rect(fill = "white", color = "black"),
        axis.text.x = element_text(angle=60, hjust=1))

##################################################################
# Compare detected and reference trees across the full domain
##################################################################

## Reformat matches
ls.match.l <- ls.match %>%
  filter(src==0 & !is.na(pair_id)) %>%
  mutate(pairid.site = paste(site, as.character(pair_id), sep='.')) %>%
  pivot_longer(cols=c(treeID, pred),
               names_to='source',
               values_to='treeID') %>%
  arrange(pair_id) %>%
  mutate(src = case_when(source=='pred' ~ 'Matched LiDAR-detected trees',
                         T ~ 'Matched field-observed trees')) %>%
  mutate(across(Zobs:Yobs, ~ ifelse(src=='Matched LiDAR-detected trees', NA, .)),
         across(Zpred:Ypred, ~ ifelse(src=='Matched field-observed trees', NA, .)),
         Z = coalesce(Zobs, Zpred),
         X = coalesce(Xobs, Xpred),
         Y = coalesce(Yobs, Ypred))

df.matched.plt.l <- ls.match.l %>%
  pivot_longer(cols=c(X,Y,Z),
               names_to='dim')

# Reformat ALL detected
ls.detect.l <- ls.match %>%
  filter(src==1)  %>%
  mutate(src='All LiDAR-detected trees',
         Z=Zpred,
         X=Xpred,
         Y=Ypred) %>%
  pivot_longer(cols=c(X,Y,Z),
               names_to='dim')

# Reformat ALL observed
ls.ref.l <- ls.match %>%
  filter(src==0) %>%
  mutate(src='All field-observed trees',
         Z=Zobs,
         X=Xobs,
         Y=Yobs) %>%
  pivot_longer(cols=c(X,Y,Z),
               names_to='dim')

# Filter to height values
df.matched.plt.l.z <- df.matched.plt.l %>%
  filter(dim=='Z')
ls.detect.l.z <- ls.detect.l %>%
  filter(dim=='Z')
ls.ref.l.z <- ls.ref.l %>%
  filter(dim=='Z')

# Plot kernel density across height support
skill.density <- ggplot(df.matched.plt.l.z, aes(x=value, group=src, color=factor(src))) +
  stat_density(linewidth=1, geom='line', position='identity',
               aes(x=value, group=src, color=factor(src)), data=ls.detect.l.z) +
  stat_density(linewidth=1, geom='line', position='identity',
               aes(x=value, group=src, color=factor(src)), data=ls.ref.l.z) +
  stat_density(linewidth=1, geom='line', position='identity') +
  scale_color_manual(values=hcomp.colors, name=NULL) +
  labs(x='Height (m)', y='Frequency of occurrence (kernel density)') +
  ggthemes::theme_calc(base_size=8) +
  theme(legend.position='none',
  )

#############################
# Write
#############################

cairo_pdf(file.path('inst', 'ms', 'figures', 'Fig3.pdf'),
          width=90/25.4, height=180/25.4, onefile=T,
          family='Arial', bg='white')

hcomp.plt /
  skill.density +
  plot_annotation(tag_levels = list(paste0('(', LETTERS[1:2], ')'))) &
  theme(plot.tag = element_text(face = 'bold'))

dev.off()
