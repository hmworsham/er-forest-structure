# Script for writing out images of forest structure and abiotic explanatory variables

#############################
# Set up working environment
#############################

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

source(file.path('inst', 'notebooks', 'regressions', '01.00_stats_ingest_data.R'))

#############################
# Modify rasters for plotting
#############################

res.rasters <- lapply(response, project, y='EPSG:4326')
exp.rasters <- lapply(explainers, project, y='EPSG:4326')
naip.raster <- project(naip, y='EPSG:4326')

geol <- tail(exp.rasters,1)[[1]]
# exp.rasters[16] <- geol
values(naip.raster)[values(naip.raster) == 0] <- NA
ext(naip.raster) <- ext(res.rasters[[1]])

#####################################
# Plot forest structure from rasters
#####################################

res.rasters <- res.rasters[c(5,2:4,1,6:8)]
res.labs <- c(ba='Basal area', height='Height 95P',
              height.skew='Height skew', diam='QMD', density='Total density',
              abla_density='Fir density', pien_density='Spruce density',
              pico_density='Pine density')

lblr <- c(expression(paste('Basal area (m'^'2',' m'^'-2', ')')),
          'Height 95P (m)',
          'Height skew',
          'QMD (cm)',
          expression(paste('Total density (stems ha'^'-1', ')')),
          expression(paste('Fir density (stems ha'^'-1', ')')),
          expression(paste('Spruce density (stems ha'^'-1', ')')),
          expression(paste('Pine density (stems ha'^'-1', ')')))

virlet <- c(LETTERS[c(1,3:6)], rep('G',3))

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

cairo_pdf('~/Desktop/Fig5.pdf', width=190/25.4, height=190/25.4, onefile=T,
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

# for(i in 1:8) {
#   terra::plot(res.rasters[[i]], col=viridis(n=20, end=0.8, direction=-1, option=virlet[i]),
#   asp=NA, axes=F, mar=c(2.1, 2.1, 3.5, 5.1), ext=ext(res.rasters[[i]]),
#   cex.lab=1.5, cex.axis=1.5, cex.main=1.8, cex.sub=1.5,
#   plg=list(cex = 2))
#   title(res.labs[[i]], line = 1, cex.main=1.8)
#   mtext(paste0('(', LETTERS[i], ')'), side=1, line=1)
# }
#
# nr <- plotRGB(naip.raster,
#         main='True color',
#         asp=NA, colNA='transparent', bgalpha=0,
#         axes=F, mar=c(2.1,2.1,2.5,5.1),
#         cex.lab=1.5, cex.axis=1.5, cex.main=1.8, cex.sub=1.5)
# mtext('(I)', side=1, line=1)
#
# dev.off()

######################################
# Plot histogram of forest structure
######################################

res.x.labs <- c(ba=expression(paste('BA (m'^'2', ' m'^'-2', ')')),
                height='Height (m)', height.skew='Height skew', diam='QMD (cm)',
                density=expression(paste('Total density (stems ha'^'-1',')')),
                abla_density=expression(paste('Fir density (stems ha'^'-1',')')),
                pien_density=expression(paste('Spruce density (stems ha'^'-1',')')),
                pico_density=expression(paste('Pine density (stems ha'^'-1',')')))

str.hist <- lapply(1:8, \(i) {
  hist(res.rasters[[i]],
       c='grey50',
       breaks=20,
       border='white',
       main=(res.labs[i]),
       #sub=paste0('(', LETTERS[i], ')'),
       xlab=res.x.labs[i],
       ylab='count',
       cex.lab=1.8, cex.axis=1.8, cex.main=2, cex.sub=1.8)
})

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
                                hjust=0.5),
        #legend.key.width = unit(0.01, 'npc'),
        #plot.margin=margin(unit(c(0,0,0,0), 'null')),
  )
})

cairo_pdf('~/Desktop/Fig6.pdf', width=190/25.4, height=190/25.4, onefile=T,
          family='Arial', bg='white')

wrap_plots(str.hist) +
  patchwork::plot_layout(heights=c(20,20,20), design=design) +
  plot_annotation(tag_levels = list(paste0('(', LETTERS[1:8], ')'))) &
  theme(plot.tag = element_text(face = 'bold'))

dev.off()

################################
# Plot explainers from rasters
###############################

target.vars <- c('heat_load',
                 'elevation',
                 'twi',
                 #'folded_aspect_205',
                 #'slope', #5
                 'tpi',
                 'curvature',
                 'awc',
                 'cec',
                 #'sand', #10
                 #'total_depth',
                 'silt',
                 'ksat',
                 'ph',
                 #'clay', #15
                 'om',
                 'swe',
                 'delta_swe',
                 'cwd',
                 'aet', #20
                 'geology')

exp.rasters <- exp.rasters[unlist(lapply(exp.rasters, names)) %in% target.vars]
exp.rasters <- lapply(exp.rasters, \(x) {x[is.nan(x)] <- NA;x})

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
              delta_swe=expression(paste('∆ SWE (mm', ~sec^-1, ')')),
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

cairo_pdf('~/Desktop/FigS2.pdf', width=190/25.4, height=190/25.4, onefile=T,
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

####################################
# Scratch
####################################

# plot(exp.rasters[[12]], col=exp.colors[[12]], main=exp.labs[[12]], asp=1,
#      legend=F)
# par(xpd=T)
# legend('right',
#        legend=levels(factor(c('Dakota Sandstone',
#          'Mancos Shale',
#          'Mesa Verde Formation (Sand/Silt/Coal)',
#          'Gothic Formation (Sand/Shale)',
#          'Maroon Formation (Red Sand/Mud/Conglomerate)',
#          'Glacial Drift',
#          'Landslide Deposits',
#          'Middle-Tertiary Granodioritic Laccoliths',
#          'Wasatch Formation (Claystone-Shale)'))),
#        inset=c(-6,0))

# par <- opar
#
# ex.stack <- raster::stack(explainers.sub)
# ex.coords <- xyFromCell(ex.stack, seq_len(ncell(ex.stack)))
# ex.stack <- stack(as.data.frame(getValues(ex.stack)))
# names(ex.stack) <- c('value', 'variable')
# ex.stack <- cbind(ex.coords, ex.stack)
#
# plot_func <- function(df, name) {
#   ggplot(data = df, aes(x = x, y = y, fill = value)) +
#     geom_tile() +
#     scale_fill_continuous(name = name)
# }
#
# nested_tmp <- ex.stack %>%
#   group_by(variable) %>%
#   nest() %>%
#   mutate(plots=map2(data, variable, plot_func))
#
# gridExtra::grid.arrange(grobs = nested_tmp$plots)



# APPROACH 2

# # ex.coords <- xyFromCell(ex.stack, seq_len(ncell(ex.stack)))
# # ex.stack <- stack(as.data.frame(getValues(ex.stack)))
# # names(ex.stack) <- c('value', 'variable')
# # ex.stack <- cbind(ex.coords, ex.stack)
# res.stack <- stack(res.rasters)
# res.stack <- projectRaster(res.stack, crs=4326)
#
# res.df <- as.data.frame(res.stack, xy=T) %>%
#   rename(all_of(res.labs)) %>%
#   pivot_longer(cols=Density:BA,
#                names_to = 'variable',
#                values_to = 'value')
#
# naip.raster <- aggregate(naip.raster, fact=20)
# naip.raster <- projectRaster(naip.raster, crs=4326)
# naip.df <- as.data.frame(naip.raster, xy=T) %>%
#   rename(Red='aop_naip_ortho_1',
#          Green = 'aop_naip_ortho_2',
#          Blue = 'aop_naip_ortho_3') %>%
#   filter(Red != 0)
#
# plot_func <- function(df, name) {
#   ggplot(data = df, aes(x = x, y = y, fill = value)) +
#     geom_raster() +
#     scale_fill_viridis(name = name, option='D', na.value='transparent') +
#     labs(title=name) +
#     coord_fixed() +
#     theme(title = element_text(hjust = 0.5)) +
#     jtools::theme_apa()
# }
#
# naip.plot <- ggplot(naip.df, aes(x=x, y=y)) +
#   geom_raster(fill=rgb(r=naip.df$Red,
#                        g=naip.df$Green,
#                        b=naip.df$Blue,
#                        maxColorValue = 255),
#               show.legend=F) +
#   scale_fill_identity() +
#   scale_x_continuous(scales::pretty_breaks(n = 4)) +
#   labs(title='True Color') +
#   coord_fixed() +
#   theme(title = element_text(hjust = 0.5)) +
#   jtools::theme_apa()
#
# nested_tmp <- res.df %>%
#   group_by(variable) %>%
#   nest() %>%
#   mutate(plots=map2(data, variable, plot_func))
#   # ungroup() %>%
#   # add_row(variable='True Color RGB (NAIP)',
#   #                            data=naip.df,
#   #                            plots=as.vector(naip.plot))
#
# gridExtra::grid.arrange(grobs = nested_tmp$plots)
