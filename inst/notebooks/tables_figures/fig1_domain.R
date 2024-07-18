# Figure 1

#############################
# Set up working environment
#############################

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

#############################
# Ingest location data
#############################

# AOP boundary
download.file('https://drive.google.com/uc?export=download&id=1Ax0F5Z4Bo3Ka0j2BKD7YSdEo_eOfQfpr&usp=drive_fs',
              destfile=file.path(tempdir(), 'aop.tar.gz'),
              method='wget')
untar(file.path(tempdir(), 'aop.tar.gz'), exdir=file.path(tempdir(), 'aop'))
aop <- untar(file.path(tempdir(), 'aop.tar.gz'), list=T)
aop <- st_read(file.path(tempdir(), 'aop', aop[file_ext(aop)=='shp']))

# Plots
download.file('https://drive.google.com/uc?export=download&id=1Ax68dArhdEJ3KgGbfwWoAL20VcmDVOG3&usp=drive_fs',
              destfile=file.path(tempdir(), 'plots.tar.gz'),
              method='wget')
untar(file.path(tempdir(), 'plots.tar.gz'), exdir=file.path(tempdir(), 'plots'))
plotsf <- untar(file.path(tempdir(), 'plots.tar.gz'), list=T)
plotsf <- st_read(file.path(tempdir(), 'plots', plotsf[file_ext(plotsf)=='shp']))

aois <- plotsf$PLOT_ID
aois <- c(aois[grep('XX', aois)], 'ER-BME3', 'SG-NWS1')
plotsf <- plotsf[!plotsf$PLOT_ID %in% aois,]

# DEM
download.file('https://drive.usercontent.google.com/download?id=1mWHn_kGv2tNNy744yypNy8QO1ODYKkc5&usp=drive_fs&confirm=t',
              destfile=file.path(tempdir(), 'usgs_dem.tif'),
              method='wget')
elv <- rast(file.path(tempdir(), 'usgs_dem.tif'))
elv <- crop(elv, aop)
elv <- mask(elv, aop)

#############################
# Build region map
#############################

sg.coords <- data.frame('lon'=-106.9656486,
                     'lat'=38.9131767)
sg.coords <- usmap_transform(sg.coords, input_names=c('lon', 'lat'))

w.map <- plot_usmap(regions='states',
                    include=c('WA', 'OR', 'CA', 'ID', 'NV', 'UT', 'NM',
                              'AZ', 'MT', 'WY', 'CO', 'NM'),
                    linewidth=0.5) +
  geom_sf(data=sg.coords, size=1.1, shape=13)


#############################
# Plot
#############################

d.map <- ggplot() +
  geom_spatraster(data=elv) +
  geom_sf(data=aop, fill=NA, linewidth=0.5, col='black') +
  geom_point(data=plotsf, aes(geometry=geometry), fill='white', col='black',
             shape=21, size=1.25, stat='sf_coordinates') +
  scale_fill_distiller(type = "seq",direction = -1, palette = "Greys",
                       na.value=NA, name='Elevation\n(m.a.s.l.)') +
  annotation_scale(location='br', style='ticks',
                   pad_x=unit(0.005, 'npc'),
                   pad_y=unit(0.005, 'npc'),
                   aes(width_hint=0.25)) +
  annotation_north_arrow(location='tr', which_north='T',
                         style=north_arrow_minimal(),
                         height=unit(0.1, 'npc'),
                         width=unit(0.05, 'npc')) +
  theme_void(base_size=8,
             base_family='Arial') +
  theme(plot.margin=margin(t=0, r=0, b=0, l=0.2, unit='npc'))

#############################
# Write
#############################
cairo_pdf(file.path('inst', 'ms', 'figures', 'Fig1.pdf'),
          width=90/25.4, height=75/25.4, onefile=T,
          family='Arial', bg='white')

(d.map + inset_element(w.map,
                      left=0, bottom=0, right=0.28, top=0.48,
                      align_to='full'))
dev.off()
