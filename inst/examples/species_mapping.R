## Tree species mapping at Snodgrass

# Load config
config <- config::get(file=file.path('config',
                                     'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

drive_auth(path=config$drivesa)

# Ingest plot boundaries
plotsf <- load.plot.sf(path=as_id(config$extdata$plotid),
                       pattern=config$extdata$plotpattern)

# Ingest field data
tmpfile <- drive_download(
  as_id(config$extdata$invid),
  type='csv',
  path=file.path(tempdir(), config$extdata$invid),
  overwrite=T)$local_path

inv <- read.csv(tmpfile)

# Ingest Nicola's classification map
tmpfile <- drive_download(
  as_id(config$extdata$falcoid),
  path=file.path(tempdir(), config$extdata$falcoid),
  overwrite = T)$local_path

sp.class <- rast(tmpfile)
sp.class <- as.factor(sp.class)

tmpfile <- drive_download(
  as_id(config$extdata$classid),
  path=file.path(tempdir(), config$extdata$classid),
  overwrite=T)$local_path

sp.codes <- read.csv(tmpfile)

# Subset field data to Snodgrass sites
inv <- inv %>%
  filter(!Site_Name %in% c('XX-CAR1', 'XX-CAR2', 'XX-CAR3',
                           'XX-PLN1', 'XX-PLN2', 'SG-NWS1',
                           'XX-FAR1', 'ER-BME3'))

# Filter out trees not meeting criteria
inv <- inv[grep('outside plot', inv$Comments, invert=T),] # Outside plots
inv <- inv[inv$Status == 'Live',] # Living stems
inv <- inv[!is.na(inv$Latitude | !is.na(inv$Longitude)),]
# inv <- inv[inv$DBH_Avg_CM >= 5,]

# Keep stem x,y,z data
stem.xyz = data.frame('Tag_Number'=as.numeric(inv$Tag_Number),
                      'Height'=as.numeric(inv$Height_Avg_M),
                      'DBH'=as.numeric(inv$DBH_Avg_CM),
                      'CR'=0.069*inv$Height_Avg_M + 0.458,
                      'Canopy_Position'=inv$Canopy_Position,
                      'X'=as.numeric(inv$Longitude),
                      'Y'=as.numeric(inv$Latitude),
                      'Sp_Code'=as.factor(inv$Sp_Code),
                      'Site_Name'=inv$Site_Name)
# stem.xyz = na.omit(stem.xyz)

# Turn stem.xyz into sf object
stem.sf <- st_as_sf(stem.xyz, coords=c('X', 'Y'), crs='EPSG:4326')
stem.sf <- st_transform(stem.sf, crs=st_crs(plotsf))
stem.sf <- vect(stem.sf)

# FILTER OUT SUBORDINATE TREES
# Buffer around all trees
# Exclude if:
  # A. a tree is < 80th pctl in height
  # B. buffer intersects the buffer of another tree(s)
  # C. any of those trees are larger

stem.buff <- buffer(stem.sf, width=0.069*Height +	0.458)

# Pull species from classification map
class.sp.plots <- extract(sp.class, stem.sf, factors=T, sp=T)
names(class.sp.plots) <- c('N', 'pixel.code')
class.sp.plots$N <- as.character(class.sp.plots$N)
class.sp.plots$pixel.code <- as.character(class.sp.plots$pixel.code)

# Join classifications to field data
stems.comp <- left_join(rownames_to_column(stem.xyz, 'N'), class.sp.plots)
stems.comp <- left_join(stems.comp, sp.codes, by='pixel.code')

levels(stems.comp$Sp_Code.x) <- unique(stems.comp$Sp_Code.y)
stems.comp$Sp_Code.y <- factor(stems.comp$Sp_Code.y, levels=levels(stems.comp$Sp_Code.x))
stems.comp <- stems.comp %>%
  rename(Reference=Sp_Code.x,
         Prediction=Sp_Code.y)

assertthat::are_equal(nrow(stems.comp), nrow(stem.xyz), nrow(class.sp.plots))

# Add size bins
stems.comp$DBH_bins <- as.numeric(cut(stems.comp$DBH, c(0, 10, 20, 30, 40, 50)))

# Confusion matrix
confusionMatrix(stems.comp$Reference, stems.comp$Prediction)

stems.cp <- stems.comp %>%
  filter(Canopy_Position %in% c('C', 'D'))

confusionMatrix(stems.cp$Reference, stems.cp$Prediction)

# Plot
stems.cp.l <- stems.comp %>%
  pivot_longer(cols=c('Reference', 'Prediction'),
               names_to='Source',
               values_to='Species')

density.dbh.facsp <- ggplot(stems.cp.l, aes(x=DBH, color=Source, fill=Source)) +
  geom_density(alpha=0.75) +
  scale_fill_brewer(palette='Spectral') +
  scale_color_brewer(palette='Spectral') +
  ggthemes::theme_calc() +
  facet_grid(~Species)

density.dbh.facsp

bar.dbh.sp <- ggplot(stems.cp.l, aes(x=DBH_bins, fill=Species, alpha=Source), color='black') +
  geom_bar(position=position_dodge2(width=0.9, preserve='single'),
           aes(y=(..count..) / sum(..count..), fill=Species)) +
  # geom_bar_pattern(aes(y=(..count..) / sum(..count..)),
  #                  position = position_dodge2(width = 0.9, preserve = "single"),
  #                  pattern_fill='white',
  #                  pattern_color='white',
  #                  pattern_density=0.05,
  #                  pattern_spacing=0.001) +
  scale_fill_brewer(palette='Spectral') +
  scale_alpha_manual(values=c(1, 0.5)) +
  #scale_pattern_manual(values=c('none', 'circle')) +
  ggthemes::theme_calc() # +
  # theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

bar.dbh.sp
# Plot
sg.sites <- unique(inv$Site_Name)

lapply(sg.sites, function(x) {
  #plot(plotsf[plotsf$PLOT_ID==x,'PLOT_ID'])
  plot(stem.sf)
})

sp.class.nes2 <- crop(sp.class, st_bbox(stem.sf[stem.sf$Site_Name=='SG-NES2',]))

sp.class.nes2.df <- data.frame(sp.class.nes2)
sp.class.nes2.df$Marshall_forest_species <- as.character(sp.class.nes2.df$Marshall_forest_species)
sp.class.nes2.df <- left_join(sp.class.nes2.df, sp.codes, by=c('Marshall_forest_species'='pixel.code'))

sp.class.nes2 <- setValues(sp.class.nes2, sp.class.nes2.df$Sp_Code)

plot(sp.class.nes2)
plot(stem.sf[stem.sf$Site_Name=='SG-NES2',], add=T)

sp.class.nes2.df <- as.data.frame(sp.class.nes2, xy=T)

newplot <- ggplot(sp.class.nes2.df) +
  geom_raster(aes(x=x, y=y, fill=Marshall_forest_species)) +
  scale_fill_brewer(palette='RdYlBu') +
  geom_sf(data=stem.sf[stem.sf$Site_Name=='SG-NES2',])

