# Water-Energy Limitation

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

# Specify number of cores
nCores <- as.integer(availableCores()-6)

# source(file.path('inst', 'notebooks',
#                  'regressions', '01.00_stats_ingest_data.R'))


we.slice <- pe.slice.itx(c('elevation', 'swe'), gams[[3]])
we.slice.df <- we.slice %>%
  select(-c(v1,v2, `elevation-swe.u95`, `elevation-swe.l95`)) %>%
  cbind(expand.grid(v1=seq(-4.9,5,0.1),
                    v2=seq(-4.9,5,0.1))) %>%
  rename(fit=`elevation-swe.fit`)

we.slice.max <- we.slice.df %>%
  group_by(v1) %>%
  summarise(max.d=max(`fit`, na.rm=T)) %>%
  mutate(max.d=ifelse(is.infinite(max.d), NA, max.d)) %>%
  drop_na()

we.slice.merge <- we.slice.max %>%
  left_join(we.slice.df, by=c('v1', 'max.d'='fit')) %>%
  right_join(we.slice.df, by='v1') %>%
  mutate(fit=ifelse(fit<0, NA, fit)) %>%
  drop_na()

ggplot(we.slice.merge, aes(x=v1, y=v2.y, z=fit)) +
  geom_raster(aes(fill=fit)) +
  geom_line(aes(x=v1, y=v2.x)) +
  scale_y_continuous(name='SWE (m)',
                     sec.axis=sec_axis(trans=~.*1, name=bquote('SWE'[Dmax]))) +
  labs(x='Elevation (m)') +
  #geom_contour(color='white', bins=20, alpha=0.25) +
  #geom_contour(aes(x=v1, y=v2, z=u95), color='white', bins=20, alpha=0.25) +
  scale_fill_viridis(option='G', na.value=NA, direction=-1) +
  ggthemes::theme_calc(base_size=18)


# Ingest unscaled variables
vars <- read.csv(file.path(config$data$pro, 'all_variables_unscaled.csv'))

we <- vars[c('density',
             'elevation',
             'heat_load',
             'swe')]

we <- we %>%
  select(c(density, elevation, heat_load, swe)) %>%
  mutate(elevation.bin=cut_width(elevation, width=10, labels=F),
         heat_load.bin=cut_width(heat_load, width=0.005, labels=F),
         swe.bin=cut_width(swe, width=0.01, labels=F))

# we.itp <- interp.surface.grid(list(x=we$elevation,
#                                              y=we$heat_load,
#                                              z=matrix(we$density,
#                                                       nrow=length(we$density),
#                                                       ncol=length(we$density))),
#                               grid.list=list(x=1:200,
#                                              y=1:200))

ggplot(we.itp) +
  geom_raster(aes(x=elevation, y=heat_load, z=density),
                      bins=20)

ggplot(we) +
  geom_raster(aes(x=elevation.bin, y=heat_load.bin, fill=density), interpolate=T)

we.grd <- ppp(we$elevation.bin, we$heat_load.bin, marks=we$density,
              owin(c(0,1200), c(0,650)))
we.itp <- idw(we.grd, power=2, at='pixels')

df <- data.frame(x=we$elevation.bin,
                 y=we$heat_load.bin,
                 z=we$density)

weras <- rast(df, type='xyz')
weras <- interpIDW(dras, as.points(dras), 'z', radius=9, power=2)
weras <- focal(dras, 3)
we.df <- as.data.frame(dras, xy=T) %>%
  rename('Elevation'=x,
         'Heat load'=y,
         'Density'=focal_sum)

we.df.max <- we.df %>%
  group_by(Elevation) %>%
  summarise(max.d=max(Density, na.rm=T)) %>%
  mutate(max.d=ifelse(is.infinite(max.d), NA, max.d)) %>%
  drop_na()

we.df.merge <- we.df.max %>%
  left_join(we.df, by=c('Elevation', 'max.d'='Density')) %>%
  right_join(we.df, by='Elevation') %>%
  mutate(Density=ifelse(Density<0, NA, Density)) %>%
  drop_na() %>%
  rename('Heat load'=`Heat load.y`,
         'Heat load Dmax'=`Heat load.x`)

ggplot(we.df.merge) +
  geom_raster(aes(x=Elevation, y=`Heat load`, fill=Density)) +
  geom_line(aes(x=Elevation, y=`Heat load Dmax`), linewidth=6, color='white') +
  scale_y_continuous(name='Heat load',
                     sec.axis=sec_axis(trans=~.*1, name=bquote('Heat load'[Dmax])),
                     limits=c(0, max(we.df.merge$`Heat load`))) +
  labs(x='Elevation (m)') +
  #geom_contour(color='white', bins=20, alpha=0.25) +
  #geom_contour(aes(x=v1, y=v2, z=u95), color='white', bins=20, alpha=0.25) +
  scale_fill_viridis(option='G', na.value=NA) +
  ggthemes::theme_calc(base_size=64) +
  theme(legend.position=c(0.1,0.84),
        legend.key.size = unit(4, 'cm')) +
  coord_fixed()

# Rasters

el.rc <- we.df.merge %>%
  select(Elevation, `Heat load Dmax`) %>%
  rename('From'=Elevation, 'To'=`Heat load Dmax`)

el.hl <- c(explainers[[2]], explainers[[1]])
el.hl <- el.hl %>%
  mutate(el.bins=cut_width(elevation, width=10, label=F),
         hl.bins=cut_width(heat_load, width=0.005, label=F)
  )

el.hl$el.hl.d.max <- terra::classify(el.hl$el.bins, el.rc)

fullmask <- rast('/Volumes/GoogleDrive/.shortcut-targets-by-id/1HZhH3KecyMfW0wQ8V8iJr7rmCNYOzF7t/RMBL-East River Watershed Forest Data/RMBL 2019/Data/LiDAR/tifs/fullmask_5m.tif')
ext(el.hl) <- ext(fullmask)
fullmask <- alignfun(fullmask, el.hl)
el.hl.masked <- mask(el.hl, fullmask)


limit.ras <- el.hl.masked$hl.bins-el.hl.masked$el.hl.d.max
limit.ras <- (limit.ras, 0,1)
plot(limit.ras, col=viridis(10, option='C'))

exx <- ext(el.hl)
par(mar=rep(0,4))
exx <- c(344220.25, 344720.25, 4298076.75, 4322676.75)
plot(ext(el.hl))
plot(el.hl['elevation'], col=grey.colors(100), legend=F)
plot(limit.ras, col=viridis(40, option='C', direction=-1), alpha=0.8, add=T,
     plg=list(loc='right',
              ext=exx,
              title='E-W limit'))

plot(el.hl$el.bins, limit.ras)
abline(1,1)
