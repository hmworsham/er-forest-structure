# Figure 5

#############################
# Set up working environment
#############################

# Load config
config <- config::get(file=file.path('ft_repro_config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

#############################
# Ingest data
#############################

# List and download forest structure rasters
download.file(config$extdata$forest_rast,
              destfile=file.path(tempdir(), 'forest_structure_rasters.tar.gz'),
              method='wget')
untar(file.path(tempdir(), 'forest_structure_rasters.tar.gz'), exdir=file.path(tempdir(), 'forest_structure_rasters'))
strfiles <- list.files(file.path(tempdir(), 'forest_structure_rasters'), full.names=T)

# Ingest forest structure rasters
dnsty <- rast(strfiles[grep('density_100m_masked.tif', strfiles)])
height <- rast(strfiles[grep('height_95pctl_100m_masked.tif', strfiles)])
height.skew <- rast(strfiles[grep('height_skew_100m_masked.tif', strfiles)])
diam <- rast(strfiles[grep('diam_qmd_100m_masked.tif', strfiles)])
ba <- rast(strfiles[grep('ba_100m_masked.tif', strfiles)])

# Ingest forest species composition rasters
abla <- rast(strfiles[grep('density_abla_100m_masked.tif', strfiles)])
pien <- rast(strfiles[grep('density_pien_100m_masked.tif', strfiles)])
pico <- rast(strfiles[grep('density_pico_100m_masked.tif', strfiles)])

# Download NAIP true color
download.file(config$extdata$naip,
              destfile=file.path(tempdir(), 'aop_naip_ortho_3m.tif'),
              method='wget')

# Ingest NAIP
naip <- rast(file.path(tempdir(), 'aop_naip_ortho_3m.tif'))

#############################
# Clean rasters for plotting
#############################

response <- list(
  'density'=dnsty,
  'height'=height,
  'height.skew'=height.skew,
  'diam'=diam,
  'ba'=ba,
  'abla_density'=abla,
  'pien_density'=pien,
  'pico_density'=pico
)

# Assign names as variable names
for(i in seq_along(response)){
  names(response[[i]]) <- names(response)[i]
}

# Coerce to WGS84
res.rasters <- lapply(response, project, y='EPSG:4326')

# Clean NAIP
naip.raster <- project(naip, y='EPSG:4326')
values(naip.raster)[values(naip.raster) == 0] <- NA
ext(naip.raster) <- ext(res.rasters[[1]])

#####################################
# Plot forest structure from rasters
#####################################

# Order rasters
res.rasters <- res.rasters[c(5,2:4,1,6:8)]

# Assign titles
res.labs <- c(ba='Basal area', height='Height 95P',
              height.skew='Height skew', diam='QMD', density='Total density',
              abla_density='Fir density', pien_density='Spruce density',
              pico_density='Pine density')

# Assign labels
lblr <- c(expression(paste('Basal area (m'^'2',' m'^'-2', ')')),
          'Height 95P (m)',
          'Height skew',
          'QMD (cm)',
          expression(paste('Total density (stems ha'^'-1', ')')),
          expression(paste('Fir density (stems ha'^'-1', ')')),
          expression(paste('Spruce density (stems ha'^'-1', ')')),
          expression(paste('Pine density (stems ha'^'-1', ')')))

# Assign panel tags
virlet <- c(LETTERS[c(1,3:6)], rep('G',3))

# Plot rasters
rast.plots <- lapply(1:8, \(i) {
  gp <- ggplot() +
    geom_spatraster(data=res.rasters[[i]]) +
    scale_fill_viridis(direction=-1, option=virlet[i],
                       na.value=NA, name=lblr[i], discrete=F,
                       guide=guide_colorbar(title.position='right',
                                            title.vjust=0.5)) +
    labs(title=res.labs[[i]], x = NULL, y = NULL) +
    theme_void(base_size=8,
               base_family='Arial') +
    theme(legend.title = element_text(angle = -90,
                                      hjust = 0.5),
          legend.key.width = unit(0.01, 'npc'),
          plot.margin=margin(unit(c(0,0,0,0), 'null')),
          plot.title=element_text(face='bold',
                                  hjust=0.5))
  print(gp)
})

np <- print(ggplot() +
              geom_spatraster_rgb(data=naip.raster) +
              theme_void(base_size=8,
                         base_family='Arial')) +
  labs(title='True color') +
  theme(plot.title=element_text(face='bold',
                                hjust=0.5))

#############################
# Write
#############################

cairo_pdf(file.path('inst', 'ms', 'figures', 'Figure_5.pdf'),
          width=190/25.4, height=190/25.4, onefile=T,
          family='Arial', bg='white')

design='
ABC
DEF
GHI
'

wrap_plots(rast.plots) + np +
  patchwork::plot_layout(heights=c(20,20,20), design=design) +
  plot_annotation(tag_levels = list(paste0('(', LETTERS[1:9], ')'))) &
  theme(plot.tag = element_text(face = 'bold'))

dev.off()
