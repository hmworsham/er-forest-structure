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

#### AOP boundary ####
download.file('https://data.ess-dive.lbl.gov/catalog/d1/mn/v2/object/ess-dive-13f264ad5d1bbfd-20230228T000240452', destfile=file.path('~/Desktop', 'aop.zip'), method='wget')

aop <- unzip(file.path(tempdir(), 'aop.zip'), file.path(tempdir(), 'aop.zip'))
aop <- load.plot.sf(path=as_id(config$extdata$bndid),
                    pattern=config$extdata$bndpattern)

#### Plots ####
plotsf <- load.plot.sf(path=as_id('1xqG7Mig73txKO4SMjxbAIyE5C_GY0BiU'),
                       pattern='Kueppers_EastRiver_AllPlots_Centroid_2023_WGS84UTM13N')
aois <- plotsf$PLOT_ID
aois <- c(aois[grep('XX', aois)], 'ER-BME3', 'SG-NWS1')
plotsf <- plotsf[!plotsf$PLOT_ID %in% aois,]

elvfile <- drive_download(
  as_id('1sDKyVFypWFk0BbTe43PTuDv007XycZ23'),
  path=file.path(tempdir(), 'usgs_dem.tif'),
  overwrite = T)$local_path

elv <- rast(elvfile)
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
  geom_point(data=sg.coords, aes(x=x, y=y), size=1.1, shape=13)


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

cairo_pdf(file.path('..', '..', 'ms', 'figures', 'Fig1.pdf'), width=90/25.4, height=75/25.4, onefile=T,
          family='Arial', bg='white')

(d.map + inset_element(w.map,
                      left=0, bottom=0, right=0.28, top=0.48,
                      align_to='full'))
dev.off()
