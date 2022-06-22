# Kueppers_Plot_GeophysGradient_Distribution
# Generates figures to depict the distribution of Kueppers et al. East River forest inventory plots along several geophysical gradients

# Author: Marshall Worsham
# Created: 10-06-20
# Revised: 06-29-21

#############################
# Set up workspace
#############################

# Function to install new packages if they're not already installed
load.pkgs <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Name packages to install
pkgs <- c(
  'data.table',
  'dplyr',
  'ggplot2',
  'ggpubr',
  'googledrive',
  'readxl',
  'tidyverse', 
  'mrmoose')

# Run the function on the list of packages defined in pkgs
load.pkgs(pkgs)

# Source helper functions
source(file.path(dirname(rstudioapi::getSourceEditorContext()$path), 'ss.helpers.R'))

# Define directories
erdir <- file.path('/Volumes', 'GoogleDrive', 'My Drive', 'Research', 'RMBL')
fidir <- file.path(erdir, 'Working_Files', 'Forest_Inventory_Dataset')
wsdir <- file.path(erdir, 'Working_Files', 'Watershed_Spatial_Dataset')
rasdir <- file.path(erdir, 'RMBL-East River Watershed Forest Data', 'Data', 'Geospatial', 'Worsham_2021_SiteSelection', '2021_Analysis_Layers', 'USGS_1-9_arcsec_DEM')
sfdir <- file.path(erdir, 'RMBL-East River Watershed Forest Data', 'Data', 'Geospatial')

#############################
# Ingest source data
#############################

# Ingest 2020 Kueppers plot characteristics CSVs
tmpfile <- tempfile()
tmpfile <- drive_download('Kueppers_EastRiver_Site_Index', tmpfile)$local_path
siteinfo.21 <- read_excel(tmpfile)
siteinfo.21$Established <- 'Established'
siteinfo.21$RMBL_Approved <- as.character(siteinfo.21$RMBL_Approved)

# Ingest 2020-2021 Kueppers plot info
siteinfo.22 <- read.csv(file.path(fidir, 'Output', 'EastRiver_Proposed_Sites_2022_10.csv'), header = T)
siteinfo.22$Location_ID <- siteinfo.22$Location_Name

###############################################
# Prep data for plotting
###############################################

# Row bind 2021 and 2022 site info
si.new <- bind_rows(siteinfo.21, siteinfo.22)

# Select variables of interest from 2021 site info
topos <- si.new[c('Location_ID',
                   'Established',
                   'Elevation_m',
                   'Slope',
                   'Aspect',
                   'Heat_Load',
                   'Folded_Aspect_205',
                   'Southness_205',
                   'TWI_100',
                   'TWI_1000',
                   'TPI_1000',
                   'TPI_2000')]

# Make df of topo variables
topos <- as.data.frame(topos)

# Specify which plots to exclude
outs <- NA
#outs <- c()

# Specify which plots are in 
ins <- c(topos[topos$Established == 'Established', 'Location_ID'],
         'SG-NWS1',
         'ER-BME3',
         'XX-FER1'#,
         #'Cement Creek 28',
         #'Emerald 1', 
         #'Baldy Mountain east 3',
         #'Brush Creek S 1',
         #'Brush Creek S 2',
         #'Baldy Mountain east 5',
         #'Snodgrass NW slope 1'#,
         #'Kebler 1',
         #'Brush Creek N 1',
         #'Brush Creek E 1',
         #'Red Mountain 1',
         #'Spring Creek E 1'
         )

# Remove the cut sites from the dataframe
topos_cut <- topos[topos$Location_ID %in% ins, ]
#topos_cut <- topos[!topos$Site_ID %in% outs, ]
#topos_cut <- topos[topos$Established == 'Established', ]

# Add column for sensor presence
topos_cut$Sensor <- F

# Assign T for sites with sensors installed
topos_cut[topos_cut$Location_ID %in% c('CC-CVS1',
                                       'ER-APL1',
                                       'ER-APU1',
                                       'ER-BME1',
                                       'ER-GT1',
                                       'SG-NES1', 
                                       'SG-SWR1',
                                       'SR-PVG1',
                                       'XX-CAR3',
                                       'XX-PLN1'), 'Sensor'] <- T

##############################################
# Batch create plots for individual variables
##############################################

# Print figures to png
print.figs(topos_cut)

############################
# Facet grid all variables
############################

# Specify vars to include
topos.no <- c('Aspect',
              'Southness_205',
              'TWI_1000', 
              'TPI_2000')
kplots <- topos_cut[,!names(topos_cut) %in% topos.no]

# Gather to long format
kplots_long <- gather(kplots, variable, value, -c(Location_ID, Established))
kplots_long$Elevation <- kplots_long[kplots_long$variable=='Elevation_m','value']
kplots_long <- kplots_long %>%
  arrange(variable, value) %>%
  mutate(order = row_number())
#kplots_long$Established <- factor(kplots_long$Established, levels = c('Established', 'Approved', 'Proposed'))

# Set up facet grid with all variables
varsgrid <- ggplot(kplots_long, aes(x = order, y = value)) +
  geom_point(aes(fill = Elevation, size=1), shape=21) +
  scale_fill_viridis_c(name='Elevation') +
  #scale_fill_manual(values=unname(icolors('mario'))) +
  #scale_alpha_discrete(range = c(1, 0.2)) +
  scale_x_continuous(
    breaks = kplots_long$order, 
    labels = kplots_long$Location_ID) +
  geom_smooth(method = "lm", se=FALSE, color='black') +
  stat_regline_equation(aes(label=paste(..eq.label.., ..rr.label.., sep = "~~~~"))) +
  facet_wrap(~variable, scales = 'free') +
  guides(color='none', fill='none', size='none') +
  labs(x='Site', y='Value') + 
  theme(
    axis.text.x = element_text(size=9, angle=90, hjust=0.95, vjust=0.2), 
    axis.title = element_text())

# Print facet grid
varsgrid
