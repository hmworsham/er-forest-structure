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

we.slice <- pe.slice.itx(c('heat_load', 'swe'), gams[[3]])

we.slice.df <- we.slice %>%
  select(-c(v1,v2, `heat_load_swe.u95`, `swe-heat_load.l95`)) %>%
  cbind(expand.grid(v1=seq(-4.9,5,0.1),
                    v2=seq(-4.9,5,0.1))) %>%
  rename(fit=`swe-heat_load.fit`)

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

we <- vars %>%
  select(c(density, diam, elevation, heat_load, swe)) %>%
  mutate(sdi=density*(diam/25.4)^1.605,
         elevation.bin=cut_width(elevation, width=10, labels=F),
         heat_load.bin=cut_width(heat_load, width=0.005, labels=F),
         swe.bin=cut_width(swe, width=0.02, labels=F))

df <- data.frame(x=we$swe.bin,
                 y=we$heat_load.bin,
                 z=we$sdi)

df.max <- df %>%
  group_by(x) %>%
  summarise(max.sdi=max(z, na.rm=T)) %>%
  mutate(max.sdi=ifelse(is.infinite(max.sdi), NA, max.sdi))

df.merge <- df.max %>%
  left_join(df, by=c('x', 'max.sdi'='z')) %>%
  right_join(df, by='x') %>%
  mutate(SDI=ifelse(z<0, NA, z)) %>%
  drop_na() %>%
  rename('Heat load'=`y.y`,
         'Heat load Dmax'=`y.x`)

ggplot(df.merge) +
  geom_line(aes(x=x, y=`Heat load`, group=x, color=`Heat load`)) +
  facet_wrap(~x)

ggplot(df.merge) +
  geom_tile(aes(x=x,y=`Heat load`, fill=SDI)) +
  geom_line(aes(x=x, y=`Heat load Dmax`), color='white') +
  # geom_smooth(aes(x=x, y=`Heat load Dmax`), color='white',
  #             method='loess', se=F) +
  scale_fill_viridis(option='G', direction=-1)

weras <- rast(df, type='xyz')
weras <- interpIDW(weras, as.points(weras), 'z', radius=9, power=2)
weras <- focal(weras, 3)
we.df <- as.data.frame(weras, xy=T) %>%
  rename('SWE'=x,
         'Heat load'=y,
         'SDI'=focal_sum)

we.df.max <- we.df %>%
  group_by(SWE) %>%
  summarise(max.d=max(SDI, na.rm=T)) %>%
  mutate(max.d=ifelse(is.infinite(max.d), NA, max.d)) %>%
  drop_na()

we.df.merge <- we.df.max %>%
  left_join(we.df, by=c('SWE', 'max.d'='SDI')) %>%
  right_join(we.df, by='SWE') %>%
  mutate(SDI=ifelse(SDI<0, NA, SDI)) %>%
  drop_na() %>%
  rename('Heat load'=`Heat load.y`,
         'Heat load Dmax'=`Heat load.x`)

ggplot(we.df.merge) +
  geom_raster(aes(x=SWE, y=`Heat load`, fill=SDI)) +
  geom_line(aes(x=SWE, y=`Heat load Dmax`), linewidth=1, color='white') +
  scale_y_continuous(name='Heat load',
                     sec.axis=sec_axis(trans=~.*1, name=bquote('Heat load'[Dmax])),
                     limits=c(0, max(we.df.merge$`Heat load`))) +
  labs(x='SWE (m)') +
  #geom_contour(color='white', bins=20, alpha=0.25) +
  #geom_contour(aes(x=v1, y=v2, z=u95), color='white', bins=20, alpha=0.25) +
  scale_fill_viridis(option='G', na.value=NA) +
  ggthemes::theme_calc(base_size=8) +
  # theme(legend.position=c(0.1,0.84),
  #       legend.key.size = unit(4, 'cm')) +
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


# Principal Components
library(factoextra)
vars <- vars %>%
  mutate(sdi=density*(diam/25.4)^1.605,
         #elevation.bin=cut_width(elevation, width=10, labels=F),
         #heat_load.bin=cut_width(heat_load, width=0.005, labels=F),
         #swe.bin=cut_width(swe, width=0.02, labels=F)
         )

pca.vars <- vars %>%
  select(heat_load, elevation, twi, tpi, curvature,
         awc, swe, delta_swe, cwd, aet, sdi) %>%
  drop_na()

pca.vars.e <- pca.vars %>%
  select(-sdi)

we.pca <- prcomp(pca.vars, scale=T)
fviz_eig(we.pca)
fviz_pca_var(we.pca,
             col.var = 'contrib',
             repel=T)

pca.df <- data.frame(we.pca$x) %>%
  cbind(pca.vars$sdi) %>%
  rename(sdi=`pca.vars$sdi`) %>%
  #sample_frac(0.1, replace=F) %>%
  select(PC1, PC2, sdi) #%>%
  # rename(x=PC1,
  #        y=PC2,
  #        z=SDI) %>%
  # mutate(PC1.bins=cut_width(PC1, width=0.01, label=F),
  #        PC2.bins=cut_width(PC2, width=0.01, label=F)) %>%
  # select(PC1.bins, PC2.bins, sdi)

pca.gam <- gam(sdi ~ s(PC1) + s(PC2) + ti(PC1, PC2), data=pca.df)

pc1.seq <- seq(min(pca.df$PC1), max(pca.df$PC1), length=100)
pc2.seq <- seq(min(pca.df$PC2), max(pca.df$PC2), length=100)

gam.predfun <- function(x,y) {
  df <- data.frame(PC1=x, PC2=y)
  predict(pca.gam, newdata=df)
}

pca.fit <- outer(pc1.seq, pc2.seq, Vectorize(gam.predfun))

min(pca.fit)
min(pca.df$sdi)

pca.fit.df <- expand_grid(pc1.seq, pc2.seq)
pca.fit.vec <- as.vector(pca.fit)

pca.fit.df <- cbind(pca.fit.vec, pca.fit.df) %>%
  rename(SDI_hat=pca.fit.vec,
         PC1=pc1.seq,
         PC2=pc2.seq)

ggplot(pca.fit.df, aes(x=PC1, y=PC2, z=SDI_hat)) +
  # geom_contour_filled(binwidth=50)

library(plotly)
plot_ly() %>%
  #add_markers(x=~pca.df$PC1, y=~pca.df$PC2, z=pca.df$sdi) %>%
  add_surface(x=~pc1.seq, y=~pc2.seq, z=t(pca.fit))

pca.fit.df2 <- rast(pca.fit)
pca.fit.df2 <- as.data.frame(pca.fit.df2, xy=T)

pca.df.max <- pca.fit.df2 %>%
  group_by(x) %>%
  summarise(max.sdi=max(lyr.1, na.rm=T)) %>%
  mutate(max.sdi=ifelse(is.infinite(max.sdi), NA, max.sdi)) %>%
  drop_na()

pca.df.merge <- pca.df.max %>%
  left_join(pca.fit.df2, by=c('x', 'max.sdi'='lyr.1')) %>%
  right_join(pca.fit.df2, by='x') %>%
  mutate(SDI=lyr.1,
         #SDI=ifelse(lyr.1<0, NA, lyr.1)
         ) %>%
  drop_na() %>%
  rename('PCA1'=x,
         'PCA2'=y.y,
         #'SDI'=lyr.1,
         'SDImax'=max.sdi,
         'PCA2max'=y.x)

ggplot(pca.df.merge) +
  geom_raster(aes(x=PCA1, y=PCA2, fill=SDI)) +
  geom_smooth(aes(x=PCA1, y=PCA2max), linewidth=1, color='white') +
  scale_y_continuous(name='SDImax',
                     sec.axis=sec_axis(trans=~.*.10, name='SDImax'),
                     limits=c(0, max(pca.df.merge$PCA2max))) +
  labs(x='PCA1') +
  #geom_contour(color='white', bins=20, alpha=0.25) +
  #geom_contour(aes(x=v1, y=v2, z=u95), color='white', bins=20, alpha=0.25) +
  scale_fill_viridis(option='G', na.value=NA) +
  ggthemes::theme_calc(base_size=8) +
  # theme(legend.position=c(0.1,0.84),
  #       legend.key.size = unit(4, 'cm')) +
  coord_fixed()

weras <- rast(pca.df)
weras <- interpIDW(weras, as.points(weras), 'sdi', radius=9, power=2)
#weras <- focal(weras, 3)
we.df <- as.data.frame(weras, xy=T)

we.df <- as.data.frame(weras, xy=T) %>%
  rename('PC1'=x,
         'PC2'=y,
         'SDI'=sdi)

ggplot(we.df, aes(x=PC1, y=PC2, fill=SDI)) +
  geom_raster()

library(gg3D)
ggplot(pca.df, aes(x=x, y=y, z=z, color=z)) +
  geom_point()
