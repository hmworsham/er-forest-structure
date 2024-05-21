## Functions to assist with tree species mapping

#' reclass
#' @description Reclassify raster with factor labels
#' @export make.modframe
#'

reclass <- function(rs, target) {
  v <- rast(rs)
  v[] <- NA
  f <- data.frame(a=as.numeric(rs))
  names(f) <- 'Pixel_Code'
  vec <- c(1:nrow(f))
  f[,2] <- vec
  m <- left_join(f, target, by='Pixel_Code')
  colnames(m)[2] <- 'ord'
  m <- m[order(m$ord),]
  v[] <- m$Sp_Code
  names(v) <- 'Sp_Code'

  return(v)
}

#' mapit
#' @description Create a tree crown map figure to visualize ITD detection at a target plot
#' @export mapit
#'

mapit <- function(spras, stems) {
  ggmap(gt1.bmap) +
    tidyterra::geom_spatraster(data=spras, aes(fill=Sp_Code), alpha=0.8) +
    scale_fill_manual(values=sp.pal, name='Classified Species', na.value = NA) +
    geom_sf(data=stems[stems$Site_Name=='ER-GT1', ],
            fill=NA, linewidth=0.5, color='gold', inherit.aes=F) +
    scale_color_manual(values=sp.pal, name='Classified Species', na.value = NA) +
    ggtitle(paste('Global N =', nrow(stems), '\nSite N =', nrow(stems[stems$Site_Name=='ER-GT1',]))) +
    xlab('Longitude') +
    ylab('Latitude') +
    coord_sf(crs = st_crs(4326)) +
    ggthemes::theme_calc(base_size=18) +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))
}

#' get.spp
#' @description Pull species from a classification map
#' @export get.spp
#'

get.spp <- function(spras, stems, spcodes) {

  class.sp.plots <- exactextractr::exact_extract(spras, stems, 'mode', progress=T)
  class.sp.plots <- data.frame('N'=1:length(class.sp.plots),
                               'Pixel_Code'=as.numeric(class.sp.plots))

  # Join classifications to field data
  stems$N <- 1:nrow(stems)
  stems.comp <- left_join(stems, class.sp.plots, by='N')
  stems.comp <- left_join(stems.comp, spcodes, by='Pixel_Code')
  stems.comp$Sp_Code.y <- as.factor(stems.comp$Sp_Code.y)
  levels(stems.comp$Sp_Code.y) <- levels(stems.comp$Sp_Code.x) <- union(levels(stems.comp$Sp_Code.x),
                                                                        levels(stems.comp$Sp_Code.y))
  stems.comp <- stems.comp %>%
    rename(Reference=Sp_Code.x,
           Classified=Sp_Code.y)

  assertthat::are_equal(nrow(stems.comp), nrow(class.sp.plots))

  # Add size bins
  stems.comp$DBH_bins <- as.numeric(cut(stems.comp$DBH, c(0, 10, 20, 30, 40, 50)))

  return(stems.comp)
}

#' plt.density
#' @description Generate a kernel density plot for tree detection evaluation
#' @export plt.density
#'

plt.density <- function(x) {
  x %>%
    pivot_longer(cols=c('Reference', 'Classified'),
                 names_to='Source',
                 values_to='Species') %>%
    ggplot(aes(x=Height, color=Source, fill=Source)) +
    geom_density(alpha=0.6,
                 bounds=c(0,40)) +
    scale_fill_manual(values=c('green4', 'dodgerblue')) +
    scale_color_manual(values=c('green4', 'dodgerblue')) +
    ggtitle(paste('N =', nrow(x))) +
    xlab('Height (m)') +
    ylab('Kernel density') +
    ggthemes::theme_calc(base_size=18) +
    # theme(legend.position = 'bottom',
    #       legend.direction = 'horizontal') +
    facet_grid(~Species)
}

#' plt.bar
#' @description Generate a bar plot for tree detection evaluation
#' @export plt.bar
#'

plt.bar <- function(x){
  x %>%
    pivot_longer(cols=c('Reference', 'Classified'),
                 names_to='Source',
                 values_to='Species') %>%
    ggplot(aes(x=DBH_bins, fill=Species, alpha=Source), color='black') +
    geom_bar(position=position_dodge2(width=1, preserve='single'),
             aes(y=(..count..) / sum(..count..), fill=Species)) +
    scale_fill_manual(values=sp.pal) +
    scale_alpha_manual(values=c(1, 0.6)) +
    ggtitle(paste('N =', nrow(x))) +
    xlab('Size bins') +
    ylab('Frequency of observations') +
    ggthemes::theme_calc(base_size=18) # +
  # theme(legend.position = 'bottom',
  #       legend.direction = 'horizontal')
  # theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
}

# Maps at every site
# siteplots <- lapply(unique(stem.sf$Site_Name), \(i) {
#   site.sp <- crop(sp.class, ext(plotsf[plotsf$PLOT_ID==i,]))
#   names(site.sp) <- 'Sp_Code'
#   site.sp <- reclass(site.sp, sp.codes)
#   ggplot() +
#     tidyterra::geom_spatraster(data=site.sp, aes(fill=Sp_Code), alpha=0.8) +
#     scale_fill_manual(values=sp.pal, name='Classified Species', na.value = NA) +
#     geom_sf(data=stem.buff[stem.buff$Site_Name==i,],
#             fill=NA, linewidth=1, color='gold', inherit.aes=F) +
#     #ggtitle(paste('Global N =', nrow(stem.sf), '\nSite N =', nrow(stem.sf[stem.sf$Site_Name==i,]))) +
#     xlab('Longitude') +
#     ylab('Latitude') +
#     ggthemes::theme_calc(base_size=14)
# })
