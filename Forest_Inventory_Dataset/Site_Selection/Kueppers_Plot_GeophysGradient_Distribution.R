# Kueppers_Plot_GeophysGradient_Distribution
# Generates figures to depict the distribution of Kueppers et al. East River forest inventory plots along several geophysical gradients

# Author: Marshall Worsham
# Created: 10-06-20
# Revised: 06-29-21

#############################
# Set up workspace
#############################

## Install and load libraries
library(moose)
pkgs <- c(
  'data.table',
  'dplyr',
  'ggplot2',
  'moose',
  'tidyverse')
load.pkgs(pkgs) # Runs the function on the list of packages defined in pkgs

# Set working directory.
erdir <- file.path('/Volumes', 'GoogleDrive', 'My Drive', 'Research', 'RMBL', fsep='/')
fidir <- file.path(erdir, 'Working_Files', 'Forest_Inventory_Dataset', fsep = '/')
wsdir <- file.path(erdir, 'Working_Files', 'Watershed_Spatial_Dataset', fsep = '/')
rasdir <- file.path(erdir, 'RMBL-East River Watershed Forest Data', 'Data', 'Geospatial', 'Worsham_2021_SiteSelection', '2021_Analysis_Layers', 'USGS_1-9_arcsec_DEM')
sfdir <- file.path(erdir, 'RMBL-East River Watershed Forest Data', 'Data')

#############################
# Ingest source data
#############################

# Ingest 2020 Kueppers plot characteristics CSVs
tmpfile <- tempfile()
tmpfile <- drive_download('Kueppers_EastRiver_Site_Index', tmpfile)$local_path
siteinfo21 <- read_excel(tmpfile)

# Ingest 2020-2021 Kueppers plot info
siteinfo22 <- read.csv(file.path(fidir, 'EastRiver_ProposedSites_2021_25.csv'), header = T)

###############################################
# Batch create plots for individual variables
###############################################

# Row bind 2020 and 2021 site info
zonnew$Site_ID <- rownames(zonnew)
zonnew$Established <- 'Proposed'
zonnew <- zonnew[c(8,9,1:7)]
zonnew
names(zonnew) <- c('Site_ID',
                   'Established',
                   'Aspect',
                   'Curvature',
                   'Elevation_m',
                   'Slope',
                   'TPI_1000',
                   'TPI_2000',
                   'TWI')

fullset <- bind_rows(siteinfo21, siteinfo20, zonnew)

fullset[fullset$Site_ID %in% c('Snodgrass NE Slope 5', 
                               'Snodgrass NE Slope 4', 
                               'Carbon 21B',
                               'sr-pvg1',
                               'cc-cvs1'), 'Established'] <- 'Established'

fullset <- siteinfo21
# Select variables of interest from 2021 site info
topos <- fullset[c('Location_ID',
                   'Established',
                   'Elevation_m',
                   'Slope',
                   'Aspect',
                   'Heat_Load',
                   '205-Folded_Aspect',
                   '205-Southness',
                   'TWI',
                   'TPI_1000',
                   'TPI_2000')]
topos <- as.data.frame(topos)

# Specify which plots to exclude
outs <- NA
outs <- c('Carbon 15',
          'Carbon 21',
          'Carbon 6',
          'Carbon 6B',
          'Cement Creek 8',
          'Cement Creek 9',
          'Cement Creek 16',
          'Coal Creek Valley North 1', 
          #'Coal Creek Valley North 1B',
          'Coal Creek 1B',
          'Coal Valley South 1',
          'Coal Valley South 2',
          'Coal Valley South 4',
          'Coal Valley South 5',
          'dummy',
          'emerald',
          'emerald2',
          #'Point Lookout North 3',
          'Schuylkill North 2B',
          'Schuylkill North 5B', 
          'Schuylkill North 2',
          'Schuylkill North 5',
          'SG-NES1B',
          'Snodgrass Convergent 4',
          'Snodgrass East Slope 2',
          'Snodgrass NE Slope 1',
          'Ute Gulch 1',
          'Ute Gulch 2')
          #'Walrod 1',
          #'Walrod 2'

# Specify which plots are in 
ins <- c(topos[topos$Established == 'Established', 'Site_ID'],
         'Coal Creek Valley North 1B',
         'cement 28',
         '25 Granite Basin',
         'emerald',
         'Carbon 20',
         #'axtell1',
         'coal north 2', 
         'Point Lookout North 3',
         #'wildcat1', 
         #'scarp1', 
         'scarp2',
         #'scarp3',
         'coal north 3', 
         'sr-pvg1',
         'cc-cvs1'
         )

# Remove the cut sites from the dataframe
#topos_cut <- topos[!topos$Site_ID %in% outs, ]
#topos_cut <- topos[topos$Established == 'Established', ]
topos_cut <- topos[topos$Site_ID %in% ins, ]
topos_cut$Sensor <- 'Sensor = F'
topos_cut[topos_cut$Site_ID %in% c('Carbon 21B',
                                   'sr-pvg1',
                                   'Snodgrass NE Slope 5',
                                   'ER-APL1',
                                   'ER-APU1',
                                   'ER-BME1',
                                   'ER-GT1',
                                   'SG-SWR1',
                                   'XX-PLN2',
                                   'cc-cvs1'), 'Sensor'] <-  'Sensor = T'
topos_cut

# topos_cut$sta <- sin(topos_cut$Aspect*pi/180)
# topos_cut$sta
# plot(sort(topos_cut$sta))

# Define a function to print figures
printfigs <- function(df){
  #'''
  # Function
  # Input: a dataframe of sites and topographic variables
  # Returns: a set of png files 
  #'''
  
  # Define colors
  set.seed=29
  colors = sample(hmwcolors('crayons'), 10)
  varnames = c('Elevation [m]', 
               'Slope angle [º]', 
               'Aspect [º]', 
               'Heat Load (MJ cm-2 y-1)',
               'Aspect Folded about 205º SW-NE (º)',
               'Southness Adjusted to 205ºSW-NE (º)',
               'Topographic Wetness Index',
               'TPI (45m window)',
               'TPI (1000m window)', 
               'TPI (2000m window)')
  
  # Loop through variables
  for (t in seq(length(df))){
    clr = colors[t]
    varname = varnames[t]
    print(clr)
    print(df[,t+2])
    
    # Open the png quartz image
    png(file.path(fidir, 
                  'Production_Images', 
                  paste0(names(topos[t+2]),'.png')), 
        width = 12, height = 9, units = 'in', res = 180)
    
    # Print the plot to png
      print(
        ggplot(df, aes(x = reorder(Location_ID,  df[, t+2]), y = df[, t+2])) +
          geom_point(aes(color = Established, size = 12)) +
          scale_color_manual(values = c(clr, 'grey 70')) +
          scale_y_continuous(name = varname) +
          #                    limits = c(2600, 3600),
          #                    breaks = seq(2600, 3600, 100)
          # ) +
          labs(x = 'Plot ID', y = names(topos)[t+2]) +
          theme_light(base_size = 24) +
          theme(axis.text.x = element_text(angle = 90, hjust = 1),
                legend.position="bottom", legend.title = element_blank()) +
          guides(scale = 'none')
      )
    dev.off()  
  }
}

printfigs(topos)
nrow(topos_cut)
topos_cut

############################
# Facet grid all variables
############################

# Select variables of interest
kplots_long <- siteinfo20[,c('Site_ID',
                           'TPI_1000',
                           'TPI_2000',
                           'Elevation_m',
                           'Radiation',
                           'Aspect',
                           'Slope',
                           'TWI')]

# Gather to format long
kplots_long <- gather(kplots_long, variable, value, -Site_ID)
kplots_long <- kplots_long %>%
  arrange(variable, value) %>%
  mutate(order = row_number())

# Set up facet grid with all variables
varsgrid <- ggplot(kplots_long, aes(x = order, y = value)) +
  geom_point(aes(color = variable, size=4)) +
  scale_color_manual(values = c('grey10', 'grey50', 'firebrick', 'darkblue', 'steelblue', 'forest green', 'chocolate1')) +
  facet_wrap(~variable, scales = 'free') +
  scale_x_continuous(breaks = kplots_long$order, labels = kplots_long$SFA_ID) +
  theme(legend.position = 'element_blank',
        axis.text.x = element_text(angle = 90))

# Print facet grid
varsgrid


########################################
# Create plots for individual variables
########################################

# define vars
ele = siteinfo20$Elevation_m
rad = siteinfo20$Radiation
tpi45 = siteinfo20$TPI_45
tpi1000 = siteinfo20$TPI_1000
tpi2000 = siteinfo20$TPI_2000
twi = siteinfo20$TWI
slo = siteinfo20$Slope
asp = siteinfo20$Aspect
topos = list(ele, tpi1000, tpi2000, tpi45, twi, slo, asp)

# plot elevation
png("ele.png",width=15,height=10,units="in",res=180)

feature = ele
ggplot(siteinfo20, aes((reorder(SFA_ID, Elevation_m)), Elevation_m)) +
#geom_point(aes(color = Established, size = 4)) +
#scale_color_manual(values = c('grey10', 'grey70')) +
geom_point(color = 'grey10', size = 18) +
scale_y_continuous(name = 'Elevation (m)', 
                   limits = c(2600, 3600), 
                   breaks = seq(2600, 3600, 100)
                   ) +
  labs(x = 'Plot ID', y = 'Elevation (m)') + 
  theme_light(base_size = 36) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="bottom", legend.title = element_blank()) +
  guides(size = F)

dev.off()


# plot radiation
png("rad.png",width=15,height=10,units="in",res=180)

feature = rad
ggplot(siteinfo20, aes((reorder(SFA_ID, feature)), feature)) +
  #geom_point(aes(color = Established, size = 4)) +
  #scale_color_manual(values = c('firebrick', 'grey70')) +
  geom_point(color = 'firebrick', size = 18) +
  scale_y_continuous(name = expression(paste('Radiation (WH', m^-2, ')')), 
                     limits = c(min(feature), max(feature)), 
                     breaks = seq(1.2e6, 1.9e6, 0.1e6),
                     labels = scales::scientific
  ) +
  labs(x = 'Plot ID', y = 'Radiation') + 
  theme_light(base_size = 36) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="bottom", legend.title = element_blank()) +
  guides(size = F)

dev.off()


# plot tpi-1k
png("tpi1k.png",width=15,height=10,units="in",res=180)

feature = tpi1
ggplot(siteinfo20, aes((reorder(SFA_ID, feature)), feature)) +
  #geom_point(aes(color = Established, size = 4)) +
  #scale_color_manual(values = c('darkblue', 'grey70')) +
  geom_point(color = 'darkblue', size = 18) +
  scale_y_continuous(name = 'TPI (1000m px window)', 
                     limits = c(min(feature), max(feature)), 
                     #breaks = seq(1.2e6, 1.9e6, 0.1e6),
                     #labels = scales::scientific
  ) +
  labs(x = 'Plot ID', y = 'TPI') + 
  theme_light(base_size = 36) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="bottom", legend.title = element_blank()
        ) +
  guides(size = F)

dev.off()

# plot tpi2k
png("tpi2k.png",width=15,height=10,units="in",res=180)

feature = tpi2
ggplot(siteinfo20, aes((reorder(SFA_ID, feature)), feature)) +
  #geom_point(aes(color = Established, size = 4)) +
  #scale_color_manual(values = c('darkblue', 'grey70')) +
  geom_point(color = 'grey50', size = 18) +
  scale_y_continuous(name = 'TPI (2000m px window)', 
                     limits = c(min(feature), max(feature)), 
                     #breaks = seq(1.2e6, 1.9e6, 0.1e6),
                     #labels = scales::scientific
  ) +
  labs(x = 'Plot ID', y = 'TPI') + 
  theme_light(base_size = 36) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="bottom", legend.title = element_blank()) +
  guides(size = F)

dev.off()

# plot tpi45
png("tpi45.png",width=15,height=10,units="in",res=180)

feature = tpi45
ggplot(siteinfo20, aes((reorder(SFA_ID, feature)), feature)) +
  #geom_point(aes(color = Established, size = 4)) +
  #scale_color_manual(values = c('darkblue', 'grey70')) +
  geom_point(color = 'darkblue', size = 18) +
  scale_y_continuous(name = 'TPI (45m px window)', 
                     limits = c(min(feature), max(feature)), 
                     #breaks = seq(1.2e6, 1.9e6, 0.1e6),
                     #labels = scales::scientific
  ) +
  labs(x = 'Plot ID', y = 'TPI') + 
  theme_light(base_size = 36) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="bottom", legend.title = element_blank()) +
  guides(size = F)

dev.off()


#plot twi
png("twi.png",width=15,height=10,units="in",res=180)

feature = twi
ggplot(siteinfo20, aes((reorder(SFA_ID, feature)), feature)) +
  #geom_point(aes(color = Established, size = 4)) +
  #scale_color_manual(values = c('steelblue', 'grey70')) +
  geom_point(color = 'steelblue', size = 18) +
  scale_y_continuous(name = 'Topographic Wetness Index', 
                     limits = c(min(feature), max(feature)), 
                     #breaks = seq(1.2e6, 1.9e6, 0.1e6),
                     #labels = scales::scientific
  ) +
  labs(x = 'Plot ID', y = 'TWI') + 
  theme_light(base_size = 36) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="bottom", legend.title = element_blank()) +
  guides(size = F)

dev.off()

#plot slope
png("slo.png",width=15,height=10,units="in",res=180)

feature = slo
ggplot(siteinfo20, aes((reorder(SFA_ID, feature)), feature)) +
  geom_point(color = 'forest green', size = 18) +
  #geom_point(aes(color = Established, size = 4)) +
  #scale_color_manual(values = c('forest green', 'grey70')) +
  scale_y_continuous(name = 'Slope (degrees)', 
                     limits = c(min(feature), max(feature)), 
                     #breaks = seq(1.2e6, 1.9e6, 0.1e6),
                     #labels = scales::scientific
  ) +
  labs(x = 'Plot ID', y = 'Slope') + 
  theme_light(base_size = 36) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = 'bottom',
        legend.title = element_blank()) +
  guides(size = F)

dev.off()

#plot aspect
png("asp.png",width=15,height=10,units="in",res=180)

feature = asp
ggplot(siteinfo20, aes((reorder(SFA_ID, feature)), feature)) +
  #geom_point(aes(color = Established, size = 4)) +
  #scale_color_manual(values = c('chocolate1', 'grey70')) +
  geom_point(color = 'chocolate1', size = 18) +
  scale_y_continuous(name = 'Aspect (degrees)', 
                     limits = c(min(feature), max(feature)), 
                     #breaks = seq(1.2e6, 1.9e6, 0.1e6),
                     #labels = scales::scientific
  ) +
  labs(x = 'Plot ID', y = 'Aspect') + 
  theme_light(base_size = 36) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="bottom", legend.title = element_blank()) +
  guides(size = F)

dev.off()

