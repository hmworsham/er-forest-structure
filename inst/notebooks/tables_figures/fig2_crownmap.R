# Figure 2

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

# Plot boundaries
download.file('https://drive.google.com/uc?export=download&id=1AxK7Wla0s41k2QWLNvFjzkljr3MlN8uy&usp=drive_fs',
              destfile=file.path(tempdir(), 'plots.tar.gz'),
              method='wget')
untar(file.path(tempdir(), 'plots.tar.gz'), exdir=file.path(tempdir(), 'plots'))
plotsf <- untar(file.path(tempdir(), 'plots.tar.gz'), list=T)
plotsf <- st_read(file.path(tempdir(), 'plots', plotsf[file_ext(plotsf)=='shp']))

# CHM
download.file('https://drive.usercontent.google.com/download?id=1FXs0Wjnu1feV5oNH8YISUDV_av95azKb',
              destfile=file.path(tempdir(), 'chm.tif'),
              method='wget')
chm <- rast(file.path(tempdir(), 'chm.tif'))

#######################################
# Map tree detection at example site
#######################################

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

# Plot basemap
uc2.base.map <- ggplot() +
  geom_raster(data=chm, aes(x=x, y=y, fill=focal_median)) +
  scale_fill_gradient(low='#1A1A1A', high='#FFFFFF',
                      name='Canopy height (m)',
                      limits=c(0,20),
                      breaks=c(0, 20),
                      na.value=NA)

# Plot field-identified and LiDAR-detected tree objects, shaded by match status
uc2.match.map <- uc2.base.map +
  geom_sf(data=uc2.bnd, color='black', linewidth=1, fill=NA) +
  geom_point(data=uc2.l, aes(x=X, y=Y, size=Zscale,
                             shape=factor(Source),
                             color=factor(matched, levels=c(T,F))),
             inherit.aes = F) +
  geom_path(data=uc2.l[!is.na(uc2.l$pair_id),], aes(x=X, y=Y, group=factor(pair_id)),
            color='#4AC63F', linewidth=0.4) +
  scale_color_manual(values=c('#4AC63F', 'grey40'), name='Matched') +
  scale_shape_manual(values=c(0,1), name='Data source') +
  ggspatial::annotation_scale(location='br', style='ticks') +
  guides(size='none') +
  labs(x='Longitude', y='Latitude') +
  ggthemes::theme_calc(base_size=8) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.key.size=unit(0.0075, 'npc'),
        legend.spacing=unit(0.00008, 'npc'),
        legend.position=c(0, 0),
        legend.justification = c('left', 'bottom'),
        legend.box.background = element_rect(fill = "white", color = "black"),
        legend.background=element_blank(),
  )

#############################
# Write
#############################
cairo_pdf(file.path('inst', 'ms', 'figures', 'Fig2.pdf'),
          width=140/25.4, height=140/25.4, onefile=T,
          family='Arial', bg='white')

print(uc2.match.map)

dev.off()
