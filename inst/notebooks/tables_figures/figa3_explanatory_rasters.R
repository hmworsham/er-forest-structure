# Figure A2

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

# List and download explanatory variable rasters
download.file(config$extdata$exp_rast,
              destfile=file.path(tempdir(), 'explanatory_rasters.tar.gz'),
              method='wget')
untar(file.path(tempdir(), 'explanatory_rasters.tar.gz'), exdir=tempdir())
exp.rasters <- lapply(list.files(file.path(tempdir(), 'explanatory_rasters'), full.names=T), rast)

#############################
# Clean rasters for plotting
#############################
# Force variable order
target.vars <- c('heat_load', 'elevation', 'twi', 'tpi', #4
                 'curvature', 'awc', 'cec', 'silt', #8
                 'ksat', 'ph', 'om', 'swe', #12
                 'delta_swe', 'cwd', 'aet', 'geology') #16
exp.rasters <- exp.rasters[order(match(names(exp.rasters), target.vars))]

# Coerce NaN to NA
exp.rasters <- lapply(exp.rasters, \(x) {x[is.nan(x)] <- NA;x})

# Coerce to WGS84
exp.rasters <- lapply(exp.rasters, project, y='EPSG:4326')

geol <- tail(exp.rasters,1)[[1]]

exp.labs <- c(heat_load='Heat load (unitless)',
              elevation='Elevation (m. a. s. l.)',
              twi='TWI (unitless)',
              tpi='TPI (unitless)',
              curvature='Curvature (unitless)',
              awc='Soil AWC (mm)',
              cec=expression(paste('Soil CEC (meq', ~hg^-1, ')')),
              silt='Soil silt content (%)',
              ksat=expression(paste('Soil k'[sat], ~'(', µm~sec^-1, ')')),
              ph=expression(paste('Soil pH (', -log[10]~H^'+', ')')),
              om='Soil organic matter (%)',
              swe='SWE (mm)',
              delta_swe=expression(paste('∆ SWE (mm', ~d^-1, ')')),
              aet='AET (mm)',
              cwd='CWD (mm)',
              geology='Geology')

exp.colors <- list('Heat load'=colorRampPalette(viridis(10, option=2))(100),
                   'Elevation'=colorRampPalette(terrain.colors(10))(100),
                   'TWI'=colorRampPalette(viridis(10, option=8))(100),
                   'TPI'=colorRampPalette(viridis(10, option=3))(100),
                   'Curvature'=colorRampPalette(viridis(10, option=9))(100),
                   'Soil AWC'=colorRampPalette(brewer.pal(9, name='BuGn'))(100),
                   'Soil CEC'=colorRampPalette(brewer.pal(9, name='Reds'))(100),
                   'Soil silt content'=colorRampPalette(brewer.pal(9, name='YlOrRd'))(100),
                   'Soil ksat'=colorRampPalette(brewer.pal(9, name='Blues'))(100),
                   'Soil pH'=colorRampPalette(brewer.pal(9, name='PuBu'))(100),
                   'Soil % organic matter'=colorRampPalette(viridis(10, option=7))(100),
                   'SWE'=colorRampPalette(rev(brewer.pal(9, name='GnBu')))(100),
                   '∆SWE'=colorRampPalette(rev(brewer.pal(9, name='Purples')))(100),
                   'AET'=colorRampPalette(viridis(9, option=5))(100),
                   'CWD'=colorRampPalette(viridis(9, option=4))(100),
                   'Geology'=brewer.pal(9, name='BrBG'))

exp.plots <- lapply(1:15, \(i) {
  gp <- ggplot() +
    geom_spatraster(data=exp.rasters[[i]]) +
    scale_fill_gradientn(colors=exp.colors[[i]],
                         na.value=NA, name=exp.labs[i],
                         guide=guide_colorbar(title.position='right',
                                              title.vjust=0.5)) +
    labs(x = NULL, y = NULL) +
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

# Process geology raster separately as factor
geol.rast <- as.factor(geol)

geol.plt <- ggplot() +
  geom_spatraster(data=geol.rast) +
  scale_fill_manual(values=exp.colors$Geology,
                    na.value=NA, name=exp.labs[16],
                    guide=guide_legend(title.position='right',
                                       title.vjust=0.5),
                    na.translate=F) +
  labs(x = NULL, y = NULL) +
  theme_void(base_size=8,
             base_family='Arial') +
  theme(legend.title = element_text(angle = -90,
                                    hjust = 0.5),
        legend.key.width = unit(0.01, 'npc'),
        legend.key.height = unit(0.0175, 'npc'),
        plot.margin=margin(unit(c(0,0,0,0), 'null')),
        plot.title=element_text(face='bold',
                                hjust=0.5))

cairo_pdf(file.path('inst', 'ms', 'figures', 'Figure_A2.pdf'),
          width=190/25.4, height=190/25.4, onefile=T,
          family='Arial', bg='white')

design='
ABCD
EFGH
IJKL
MNOP
'

wrap_plots(exp.plots) + geol.plt +
  patchwork::plot_layout(heights=c(20,20,20,20), design=design) +
  plot_annotation(tag_levels = list(paste0('(', LETTERS[1:16], ')'))) &
  theme(plot.tag = element_text(face = 'bold'))

dev.off()
