# Script for writing out images of forest structure and abiotic explanatory variables

#############################
# Set up working environment
#############################

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

source(file.path('~', 'Repos', 'er', 'er-forest-structure', 'inst', 'notebooks', 'regressions', '01.00_stats_ingest_data.R'))

#############################
# Modify rasters for plotting
#############################

res.rasters <- projectRaster(res.rasters, crs=4326)
exp.rasters <- projectRaster(explainers.sub, 4326)
naip.raster <- projectRaster(naip, crs=4326)

values(naip.raster)[values(naip.raster) == 0] <- NA

#####################################
# Plot forest structure from rasters
#####################################

res.labs <- c(density='Density', height='Height',
              'height.skew'='Height skew', diam='QMD', ba='BA')

par(mfrow=c(3,2),
    mar=rep(1.5, 4))
for(i in 1:5) {
  plot(res.rasters[[i]], col=viridis(n=20, option=LETTERS[i]),
  asp=1,
  cex.lab=1.5, cex.axis=1.5, cex.main=1.8, cex.sub=1.5)
  title(res.labs[[i]], line = 1, cex.main=1.8)
}

ytick <- c(38.80, 39.05)
xtick <- c()
plotRGB(naip.raster,
        main='True color',
        asp=1, colNA='transparent', bgalpha=0,
        axes=T,
        cex.lab=1.5, cex.axis=1.5, cex.main=1.8, cex.sub=1.5)
axis(side=2, at=ytick, labels=T)
dev.off()
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

################################
# Plot explainers from rasters
###############################

exp.labs <- c(elevation_10m='Elevation', slope='Slope', tpi_1km='TPI',
              awc='Soil AWC', k='Soil K', td='Soil total depth',
              om='Soil % organic matter', swe='SWE', delta_swe='∆ SWE',
              aet='AET', cwd='CWD', geology='Geology')

exp.colors <- list('Elevation'=terrain.colors(10),
                   'Slope'=viridis(10, option=2),
                   'TPI'=viridis(10, option=7),
                   'Soil AWC'=brewer.pal(9, name='BuGn'),
                   'Soil K'=brewer.pal(9, name='Blues'),
                   'Soil total depth'=brewer.pal(9, name='YlOrRd'),
                   'Soil % organic matter'=viridis(10, option=7),
                   'SWE'=rev(brewer.pal(9, name='GnBu')),
                   '∆SWE'=rev(brewer.pal(9, name='Purples')),
                   'AET'=viridis(9, option=5),
                   'CWD'=viridis(9, option=4),
                   'Geology'=brewer.pal(9, name='BrBG'))

opar <- par()
par(mfcol=c(4,3), mar=rep(1.5,4))
for(i in seq_along(exp.rasters)) {
  plot(exp.rasters[[i]], col=exp.colors[[i]], main=exp.labs[i], asp=1,
       cex.lab=1.5, cex.main=1.8, cex.sub=1.5)
}

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

par <- opar
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
#######################
#
######################
