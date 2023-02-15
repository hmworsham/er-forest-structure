# Analysis of Kueppers et al. East River Forest Inventory Data
# Author: H. M. Worsham
# Revised: 03-22-2021

# Load libraries
#library(reshape2)
library(ggplot2)
library(ggpmisc)
library(Hmisc)
library(corrplot)
library(googledrive)
library(broom)
library(cluster)
library(factoextra)
library(raster)
library(dplyr)
library(readxl)
library(tidyverse)

# Set data directories
erdir <- file.path('/Volumes', 'GoogleDrive', 'My Drive', 'Research', 'RMBL')
dendrodir <- file.path(erdir, 'RMBL-East River Watershed Forest Data', 'Data', 'Inventory Plots')
workdir <- file.path(erdir, 'Working_Files', 'Forest_Inventory_Dataset', 'Source')
rasdir <- file.path(erdir, 'RMBL-East River Watershed Forest Data', 'Data', 'Geospatial', 'Worsham_2021_SiteSelection', '2021_Analysis_Layers', 'USGS_1-9_arcsec_DEM')
sfdir <- file.path(erdir, 'RMBL-East River Watershed Forest Data', 'Data', 'Geospatial')

##########################################################################
# Import full clean inventory data
##########################################################################
invdata <- read.csv(file.path('~', 'Desktop', 'EastRiver_AllPlots_Inventory_Data_2018-2021_Collated.csv'), row.names=1)
View(invdata)

##########################################################################
# Distribution of heights and diameters
##########################################################################

ggplot(invdata, aes(x=DBH_Avg_CM)) +
  geom_histogram(aes(y=..density..),binwidth=5, color='darkblue', fill='white', position='identity') +
  labs(title='Diameter distribution of trees by species',
       x='DBH (cm)',
       y='Density',
       color='Elevation (m a.s.l.)') +
  #scale_color_manual(values=mrmoose::icolors('mario'))
  theme_minimal() + 
  theme(axis.title=element_text(size=16),
        axis.text=element_text(size=14),
        title=element_text(size=16, hjust=0.5),
        asp=1) +
  facet_wrap(facets='Sp_Code')

ggplot(invdata, aes(x=Height_Avg_M)) +
  geom_histogram(aes(y=..density..),binwidth=5, color='red', fill='white', position='identity') +
  labs(title='Height distribution of trees by species',
       x='Stem height (m)',
       y='Density',
       color='Elevation (m a.s.l.)') +
  #scale_color_manual(values=mrmoose::icolors('mario'))
  theme_minimal() + 
  theme(axis.title=element_text(size=16),
        axis.text=element_text(size=14),
        title=element_text(size=16, hjust=0.5),
        asp=1) +
  facet_wrap(facets='Sp_Code')

##########################################################################
# Fitting power function for height-diameter allometry 
##########################################################################
# H = aD^b

allom <- inv %>%
  select(Site_Name,
         Height_Avg_M,
         DBH_Avg_CM, 
         Elevation_m)
#  filter(!Site_Name %in% c('XX-PLN1', 'XX-PLN2', 'XX-CAR1', 'XX-CAR2'))

ggplot(allom, aes(x=DBH_Avg_CM, y=Height_Avg_M, color=as.factor(Elevation_m))) +
  geom_point() + 
  geom_smooth()

allom <- na.omit(allom)

nls_groups <- function(df){
  
  sn = unique(df$Site_Name)
  coefs = lapply(sn, function(x){
    subset = df[df$Site_Name==x,]
    nlmod <- nls(Height_Avg_M~a*DBH_Avg_CM^b, 
                 start=list(a=0.5, b=0.01), 
                 data=subset,
                 na.action=na.exclude,
                 control=nls.control(maxiter=1000))
    coef = summary(nlmod)$coefficients
  })
  
  return(coefs)
}

allom.coefs <- nls_groups(allom)
allom.coefs <- do.call(rbind.data.frame, allom.coefs)
allom.coefs$Site <- unlist(lapply(sn[c(1:19,21:23)], rep, 2))
allom.coefs$Param <- unlist(lapply(row.names(allom.coefs), substr, 1,1))
allom.coefs
#allom.coefs <- melt(allom.coefs, c('Site', 'Param', 'Estimate'))
allom.coefs.wide <- data.frame(a=allom.coefs[allom.coefs$Param=='a',c('Site','Estimate')],
                               b=allom.coefs[allom.coefs$Param=='b', c('Estimate')])
allom.coefs.wide

names(allom.coefs.wide) <- c('Site_Name',
                             'a',
                             'b')

pwr = function(x,a,b) a*x^b
allom <- allom %>%
  group_by(Site_Name) %>%
  mutate(yhat = pwr(DBH_Avg_CM, allom.coefs$a, allom.coefs$b))

ggplot(allom, aes(x=DBH_Avg_CM, y=Height_Avg_M, group=Site_Name, color=Elevation_m)) + 
  #geom_point() + 
  stat_smooth(method='nls', 
              formula='y~a*x^b', 
              method.args = list(start=c(a = 1,b=1)),
              se=FALSE) + 
  scale_color_continuous(type='viridis') + 
  labs(title='Diameter-height allometry varies with elevation',
       x='DBH (cm)',
       y='Height (m)',
       color='Elevation (m a.s.l.)') +
  theme_minimal() + 
  theme(axis.title=element_text(size=16),
        axis.text=element_text(size=14),
        title=element_text(size=16, hjust=0.5),
        asp=1)


###############################################
# Linear regression topography on allometry
##############################################
allom.topo <- merge(allom.coefs.wide, siteinfo, on='Site_Name')

a.topo.lm <- lm(a ~ Elevation_m + 
                  Heat_Load + 
                  TWI_1000 + 
                  Slope + 
                  Folded_Aspect_205 +
                  TPI_1000 + 
                  Curvature, 
                data=allom.topo
)
summary(a.topo.lm)

a.el.lm <- lm(a~Elevation_m, data=allom.topo)
summary(a.el.lm)

ggplot(allom.topo, aes(x=Elevation_m, y=a)) + 
  geom_point(color='darkblue', shape=21, size=2) + 
  geom_smooth(method='lm', color='darkblue', fill='lightblue3') + 
  labs(title='Diameter-height allometry varies with elevation',
       x='Elevation (m a.s.l.)',
       y=bquote('A | height = A' (DBH^b))) +
  theme_minimal() + 
  theme(axis.title=element_text(size=16),
        axis.text=element_text(size=14),
        title=element_text(size=16, hjust=0.5),
        asp=1)

?geom_smooth
b.topo.lm <- lm(b ~ Elevation_m + 
                  Heat_Load + 
                  TWI_1000 + 
                  Slope + 
                  Folded_Aspect_205 +
                  TPI_1000 + 
                  Curvature, 
                data=allom.topo
)
summary(b.topo.lm)

b.el.lm <- lm(b~Elevation_m, data=allom.topo)
summary(b.el.lm)

ggplot(allom.topo, aes(x=Elevation_m, y=a)) + 
  geom_point() + 
  geom_smooth(method='lm')


##########################################################################
# QMD ~ topo analysis
##########################################################################

QMD <- inv %>%
  group_by(Site_Name) %>%
  summarise(QMD = sqrt(mean(DBH_Avg_CM^2, na.rm=T))) %>%
  ungroup()

qmd_topo <- merge(QMD, siteinfo, by = 'Site_Name')
el_qmd_fit <- lm(QMD ~ Elevation_m + 
                   Heat_Load + 
                   TWI_1000 + 
                   Slope + 
                   Folded_Aspect_205 +
                   TPI_1000 + 
                   Curvature, 
                 data=qmd_topo
)
summary(el_qmd_fit)

ggplot(qmd_topo, aes(Elevation_m, QMD)) +
  geom_point() +
  geom_line(aes(y = predict(el_qmd_fit))) + 
  stat_poly_eq(formula = x ~ y, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               label.x = 0.05,
               label.y = 0.05,
               parse = TRUE)


##########################################################################
# Gini ~ topo analysis
##########################################################################

gini <- allom %>%
  group_by(Site_Name) %>%
  summarise(gini = Gini(DBH_Avg_CM))

gini_topo <- merge(gini, siteinfo, by = 'Site_Name')

gini_topo_fit <- lm(gini ~ Elevation_m + 
                      Heat_Load + 
                      TWI_1000 + 
                      Slope + 
                      Folded_Aspect_205 +
                      TPI_1000 + 
                      Curvature, 
                    data=gini_topo
)

summary(gini_topo_fit)