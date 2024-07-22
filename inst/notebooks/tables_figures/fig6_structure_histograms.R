# Figure 6

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
res.x.labs <- c(ba=expression(paste('BA (m'^'2', ' m'^'-2', ')')),
                height='Height (m)', height.skew='Height skew', diam='QMD (cm)',
                density=expression(paste('Total density (stems ha'^'-1',')')),
                abla_density=expression(paste('Fir density (stems ha'^'-1',')')),
                pien_density=expression(paste('Spruce density (stems ha'^'-1',')')),
                pico_density=expression(paste('Pine density (stems ha'^'-1',')')))

# Plot histograms
str.hist <- lapply(1:8, \(i) {
  res.df <- as.data.frame(values(res.rasters[[i]]))
  names(res.df) <- c('v')
  ggplot(res.df) +
    geom_histogram(aes(x=v), bins=30, color='white', fill='grey40') +
    labs(title=res.labs[[i]], x=res.x.labs[[i]], y='Count of pixels') +
    ggthemes::theme_calc(base_size=8,
                         base_family='Arial') +
    theme(aspect.ratio = 1,
          plot.background=element_rect(fill=NA, color=NA, linewidth=0),
          plot.title=element_text(face='bold',
                                  hjust=0.5))
})

#############################
# Write
#############################

cairo_pdf(file.path('inst', 'ms', 'figures', 'Figure_6.pdf'),
          width=190/25.4, height=190/25.4, onefile=T,
          family='Arial', bg='white')

design='
ABC
DEF
GHI
'

wrap_plots(str.hist) +
  patchwork::plot_layout(heights=c(20,20,20), design=design) +
  plot_annotation(tag_levels = list(paste0('(', LETTERS[1:8], ')'))) &
  theme(plot.tag = element_text(face = 'bold'))

dev.off()
