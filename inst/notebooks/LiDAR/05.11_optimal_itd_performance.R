# Check optimal ITD performance
# Author: Marshall Worsham | worsham@berkeley.edu
# Created: 04-08-24
# Revised: 07-23-24

#############################
# Set up working environment
#############################

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

# Configure Drive auth
drive_auth(path=config$drivesa)

#############################
# Data ingest
#############################

# Ingest optimized segmented trees
opt.trees <- drive_ls(as_id('1aPYSoWdXJxX8L81ABSvCZuwi_CRzgd9U'))
ls.trees <- unlist(lapply(1:nrow(opt.trees), \(x) {
  tf <- drive_download(
    as_id(opt.trees$id[x]),
    path=file.path(tempdir(), opt.trees$name[x]),
    overwrite=T)
  tf$local_path
}))

ls.trees <- lapply(ls.trees[grepl('shp', ls.trees)], st_read)

# Ingest match data
ls.match <- drive_download(
  as_id('1LCafch6gd2bYqYTT0eeW1o5QSHXv5O50'),
  path=file.path(tempdir(), 'opt_matches.csv'),
  overwrite = T)$local_path

ls.match <- read.csv(ls.match)

#############################################
# Compare detected and reference trees
#############################################

pred.med.ht <- median(ls.match$Zpred, na.rm=T)
pred.sd.ht <- sd(ls.match$Zpred, na.rm=T)
pred.p90.ht <- quantile(ls.match$Zpred, .9, na.rm=T)

pred.med.ht
pred.sd.ht

ht.delta <- ls.match %>%
  filter(src==1) %>%
  summarise(mean(Zpred-Zobs, na.rm=T))

################################################
# Compare detected and reference trees by plot
################################################

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

# # Compare 90th pctl height by site
# ls.detect.90.l <- ls.match %>%
#   filter(src==1) %>%
#   group_by(site) %>%
#   summarise(`All detected` = quantile(Zpred, .9, na.rm=T))
#
# ls.match.comp.l.90 <- ls.match %>%
#   filter(src==0) %>%
#   group_by(site) %>%
#   summarise(`Matched detected` = quantile(Zpred[!is.na(pred)], .9, na.rm=T),
#             `All reference` = quantile(Zobs, .9, na.rm=T),
#             `Matched reference` = quantile(Zobs[!is.na(pred)], .9, na.rm=T))
#   left_join(ls.detect.med.l, by='site') %>%
#   pivot_longer(cols=c(`All detected`, `Matched detected`,
#                       `All reference`,`Matched reference`)) %>%
#   mutate(name=factor(name, levels=c('All detected', 'All reference',
#                                     'Matched detected', 'Matched reference')))

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

#############################################
# Plots
#############################################

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

# Plot: 90th pctl height comparison barplot
# ggplot(ls.match.comp.l.90, aes(x=site, y=value, fill=name)) +
#   geom_col(position=position_dodge(width=0.75), width=0.75) +
#   scale_fill_manual(values=hcomp.colors, name='90th percentile height') +
#   labs(x='Site', y='90th percentile of height (m)') +
#   ggthemes::theme_calc(base_size=18)

# Plot: QMD comparison barplot
# ggplot(ls.match.qmd.l, aes(x=site, y=value, fill=name)) +
#   geom_col(position=position_dodge(width=0.75), width=0.75) +
#   scale_fill_manual(values=hcomp.colors, name='Quadratic mean diameter') +
#   labs(x='Site', y='90th percentile of height (m)') +
#   ggthemes::theme_calc(base_size=18)

###################################################
# Compare detected and reference trees full domain
###################################################

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

##########################
# Plot v1: XYZ density
#########################

# Kernel density estimate
kdens.colors <- hcomp.colors
skill.density <- ggplot(df.matched.plt.l, aes(x=value, group=src, color=factor(src))) +
  stat_density(linewidth=1, geom='line', position='identity',
               aes(x=value, group=src, color=factor(src)), data=ls.detect.l) +
  stat_density(linewidth=1, geom='line', position='identity',
               aes(x=value, group=src, color=factor(src)), data=ls.ref.l) +
  stat_density(linewidth=1, geom='line', position='identity') +
  scale_color_manual(values=kdens.colors, name='Data source') +
  scale_y_continuous(labels = \(x) format(x, digits=1, scientific = TRUE)) +
  #scale_fill_manual(values=kdens.colors, name='Data source') +
  labs(x='Dimension value', y='Kernel density') +
  facet_wrap(~dim, nrow=3, scales='free') +
  ggthemes::theme_calc(base_size=8) +
  theme(legend.position = 'bottom')

skill.density

##########################
# Plot v2: XYZ density
#########################

df.matched.plt.l.z <- df.matched.plt.l %>%
  filter(dim=='Z')
ls.detect.l.z <- ls.detect.l %>%
  filter(dim=='Z')
ls.ref.l.z <- ls.ref.l %>%
  filter(dim=='Z')

skill.density.2 <- ggplot(df.matched.plt.l.z, aes(x=value, group=src, color=factor(src))) +
  stat_density(linewidth=1, geom='line', position='identity',
               aes(x=value, group=src, color=factor(src)), data=ls.detect.l.z) +
  stat_density(linewidth=1, geom='line', position='identity',
               aes(x=value, group=src, color=factor(src)), data=ls.ref.l.z) +
  stat_density(linewidth=1, geom='line', position='identity') +
  scale_color_manual(values=kdens.colors, name=NULL) +
  #scale_y_continuous(labels = \(x) format(x, digits=1, scientific = TRUE)) +
  #scale_fill_manual(values=kdens.colors, name='Data source') +
  labs(x='Height (m)', y='Frequency of occurrence (kernel density)') +
  #facet_wrap(~dim, nrow=3, scales='free') +
  ggthemes::theme_calc(base_size=8) +
  theme(legend.position='none',
        # legend.position = c(0.742,0.895),
        # legend.box.background = element_rect(fill = "white", color = "black")
  )

skill.density.2

# Format for publication
cairo_pdf('~/Desktop/Fig2.pdf', width=90/25.4, height=180/25.4, onefile=T,
          family='Arial', bg='white')

hcomp.plt /
  skill.density.2 +
  plot_annotation(tag_levels = list(paste0('(', LETTERS[1:2], ')'))) &
  theme(plot.tag = element_text(face = 'bold'))

dev.off()

#############################################
# Map example tree detections
#############################################

# Ingest plot boundaries
plotsf <- load.plot.sf(path=as_id(config$extdata$plotid),
                       pattern=config$extdata$plotpattern)

# Ingest LAS
# infiles <- list.files(config$extdata$las_dec, full.names=T)
# lascat <- readLAScatalog(infiles)

# Ingest CHM
# er.chm <- rast(file.path(config$extdata$scratch, 'chm_full_extent', 'chm_smooth_masked.tif'))

# Ingest NAIP base image
# tmpfile <- drive_download(
#   as_id(config$extdata$naipid),
#   path=file.path(tempdir(), config$extdata$naipid),
#   overwrite=T)$local_path
#
# naip <- rast(tmpfile)

# View n for each site-src
View(ls.match %>%
  group_by(site,src) %>%
  summarise(n=n()))

# Define target site and subset matches to all predicted
uc2.match <- ls.match[ls.match$site=='CC-UC2',]
uc2.pred <- uc2.match[uc2.match$src==1 |
                        (uc2.match$src==0 & is.na(uc2.match$pair_id)),]

# Reformat matches
uc2.l <- uc2.pred %>%
  pivot_longer(cols=c(treeID, obs),
               names_to='Source',
               values_to='treeID') %>%
  arrange(pair_id) %>%
  dplyr::filter(!(src==0 & Source=='treeID')) %>% # may need to filter the opposite Source
  mutate(Source=ifelse(Source=='treeID', 'LiDAR-detected trees', 'Field-observed trees')) %>%
  mutate(across(Zobs:Yobs, ~ ifelse(Source=='LiDAR-detected trees', NA, .)),
         across(Zpred:Ypred, ~ ifelse(Source=='Field-observed trees', NA, .)),
         Z = coalesce(Zobs, Zpred),
         X = coalesce(Xobs, Xpred),
         Y = coalesce(Yobs, Ypred),
         Zscale = Z*.001,
         matched = ifelse(is.na(pair_id), F, T))

# Subset plot boundaries to target site
uc2.bnd <- plotsf[plotsf$PLOT_ID=='CC-UC2',]

# Clip las to plot boundaries
uc2.las <- clip_roi(lascat, st_buffer(uc2.bnd, 50, nQuadSegs = 2, endCapStyle ='SQUARE', joinStyle='MITRE', mitreLimit = 10))
st_crs(uc2.las) <- st_crs(uc2.bnd)
plot(uc2.las)
rglwidget()

# Rasterize canopy
uc2.chm.pitfree.05 <- rasterize_canopy(uc2.las, 0.5, p2r(subcircle=0.25, na.fill=knnidw()), pkg = "terra")
plot(ext(uc2.chm.pitfree.05)+20)
plot(uc2.chm.pitfree.05, col=rev(brewer.pal(11,'Spectral')), add=T)

# Smooth canopy
kernel <- matrix(1,1,1)
uc2.chm.smooth <- focal(uc2.chm.pitfree.05, w = kernel, fun = median, na.rm = TRUE)
uc2.chm.smooth <- mask(uc2.chm.smooth, st_buffer(uc2.bnd, 5, endCapStyle ='SQUARE', joinStyle='MITRE', mitreLimit = 5))
plot(ext(uc2.chm.pitfree.05)+20)
plot(uc2.chm.smooth, col=rev(brewer.pal(11,'Spectral')),add=T)

# Write out
writeRaster(uc2.chm.smooth, file.path(config$extdata$scratch, 'uc2_chm.tif'))

# Coerce to df
uc2.chm.df <- as.data.frame(uc2.chm.smooth, xy=T)

# Clip naip to plot boundaries
uc2.naip <- crop(naip, st_buffer(uc2.bnd, 5, endCapStyle ='SQUARE', joinStyle='MITRE', mitreLimit = 5))
uc2.naip.df <- as.data.frame(uc2.naip, xy=T)
names(uc2.naip.df)[3:5] <- c('green', 'blue', 'red')

# Plot
uc2.base.map <- ggplot() +
  geom_raster(data=uc2.chm.df, aes(x=x, y=y, fill=focal_median)) +
  scale_fill_gradient(low='#1A1A1A', high='#FFFFFF',
                      name='Canopy height (m)',
                      breaks=c(1, 10, 20))

uc2.match.map <- uc2.base.map +
  geom_sf(data=uc2.bnd, color='black', linewidth=1, fill=NA) +
  geom_point(data=uc2.l, aes(x=X, y=Y, size=Zscale,
                             shape=factor(Source),
                             color=factor(matched, levels=c(T,F))),
             inherit.aes = F) +
  geom_path(data=uc2.l[!is.na(uc2.l$pair_id),], aes(x=X, y=Y, group=factor(pair_id)),
            color='#4AC63F', linewidth=0.4) +
  # geom_text(data=uc2.l, aes(x=X, y=Y,
  #                           color=factor(matched, levels=c(T,F)),
  #                           label=factor(pair_id)),
  #                           size=1,
  #           position=position_jitter()
  #           ) +
  scale_color_manual(values=c('#4AC63F', 'grey40'), name='Matched') +
  scale_shape_manual(values=c(0,1), name='Data source') +
  ggspatial::annotation_scale(location='br', style='ticks') +
  guides(size='none') +
  labs(x='Longitude', y='Latitude') +
  ggthemes::theme_calc(base_size=8) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        # plot.margin = margin(c(t=0, r=0, b=0, l=0.05), unit='npc'),
        legend.key.size=unit(0.0075, 'npc'),
        legend.spacing=unit(0.00008, 'npc'),
        #legend.title = element_text(size=14),
        #legend.text = element_text(size=12),
        #legend.direction='horizontal',
        legend.position=c(0, 0),
        legend.justification = c('left', 'bottom'),
        #legend.box.margin = margin(rep(0,4), unit='npc'),
        legend.box.background = element_rect(fill = "white", color = "black"),
        legend.background=element_blank(),
        #legend.spacing.y = unit(0.1,"cm")
        )

# saveRDS(uc2.match.map, 'map')
# uc2.match.map <- readRDS(file.path(config$data$int, 'matchmap.Rda'))

cairo_pdf('~/Desktop/Fig2.pdf', width=140/25.4, height=140/25.4, onefile=T,
          family='Arial', bg='white')

print(uc2.match.map)

dev.off()
