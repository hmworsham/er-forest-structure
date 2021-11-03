
## Install and load libraries
pkgs <- c('dplyr',
          'tidyverse',
          'ggplot2',
          'ggpmisc',
          'Hmisc',
          'corrplot',
          'broom',
          'cluster',
          'factoextra',
          'raster',
          'caret',
          'FactoMineR',
          'pca3d')

# Name the packages you want to use here
load.pkgs <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} # Function to install new packages if they're not already installed
load.pkgs(pkgs) # Runs the function on the list of packages defined in pkgs

# Set data directories
setwd(file.path('~', 'Desktop', 'RMBL', 'Projects', fsep = '/'))
fidir <- file.path(getwd(), 'Forest_Inventory_Dataset', 'Output')
wsdir <- file.path(getwd(), 'Watershed_Spatial_Dataset', 'Source')
rasdir <- file.path(getwd(), 'Watershed_Spatial_Dataset', 'Output', 'AOP_Clipped_Topo_Rasters/')

##########################################################################
# Import full clean inventory data
##########################################################################
invdata <- read.csv(file.path(fidir, 'EastRiver_AllPlots_Inventory_Data_2018-2020_Collated.csv'))

##########################################################################
# Import site information
##########################################################################

# Ingest 2020 Kueppers plot characteristics CSVs
siteinfo20 <-  read.csv(file.path(fidir, 'Kueppers_EastRiver_Final_Sites_2020.csv'), header = T)

# Ingest 2020-2021 Kueppers plot info
siteinfo21 <- read.csv(file.path(fidir, 'EastRiver_ProposedSites_2021_25.csv'), header = T)
siteinfo21 <- rename(siteinfo21, Site_Name = Site_ID)

# Refactor a couple of columns in siteinfo20 to row bind with 2021 data
siteinfo20 <- rename(siteinfo20, Site_Name = SFA_ID)
siteinfo20$Established <- as.factor(siteinfo20$Established)

# Merge 2020 site info and inventory data
inv <- merge(invdata, siteinfo20, by = 'Site_Name')

sitetopos21 <- siteinfo21[c('Site_Name', 
                          'Elevation_m', 
                          'Slope', 
                          'Aspect', 
                          'TWI', 
                          'TPI_45', 
                          'TPI_1000', 
                          'TPI_2000',
                          'Radiation')]

sitetopos20 <- siteinfo20[c('Site_Name', 
                            'Elevation_m', 
                            'Slope', 
                            'Aspect', 
                            'TWI', 
                            'TPI_45', 
                            'TPI_1000', 
                            'TPI_2000',
                            'Radiation')]

#################
# Preprocessing
#################
head(sitetopos21)
head(sitetopos20)

# Row bind 2020 and 2021 topographic info
#sitetopos.all <- bind_rows(sitetopos20, sitetopos21)
sitetopos.all <- topos_cut
View(sitetopos.all)

# Specify which plots to remove
outs <- c('Carbon 15',
          'Carbon 21',
          'Carbon 6',
          'Cement Creek 8',
          'Cement Creek 9',
          'Cement Creek 28',
          'Coal Creek Valley North 1', 
          'Coal Creek Valley North 1B',
          'Coal Valley South 1',
          'Coal Creek 1B',
          'Coal Valley South 2',
          'Coal Valley South 4',
          'Coal Valley South 5',
          'Point Lookout North 3',
          'Schuylkill North 2B',
          'Schuylkill North 5B', 
          'Schuylkill North 2',
          'Schuylkill North 5',
          'Snodgrass Convergent 4',
          'Snodgrass East Slope 2',
          'Snodgrass NE Slope 1',
          'Ute Gulch 2')

topos.cut <- sitetopos.all[!sitetopos.all$Site_Name %in% outs, ]
nrow(topos.cut)

# Remove unneeded columns (Site_Name, Radiation)
sitetopos.all
#sitetopos <- sitetopos.all[-c(1,2,7,9)]
sitetopos <- topos_cut[-c(1,2,7,9,10)]

rownames(sitetopos) <- seq(1:nrow(sitetopos))
st.mat <- as.matrix(sitetopos)
st.mat
rescaled <- preProcess(st.mat, method=c("center", "scale"))

# Normalize
plots.norm <- predict(rescaled, st.mat)
heatmap(plots.norm)

#################
# PCA
#################

# Run principle components analysis on rescaled data
pca <- prcomp(plots.norm, scale = F)
summary(pca)
pca$rotation
pca$x

# Scree plot of eigenvectors
fviz_eig(pca)

# Plot of individual field plots' projections onto PC1 and PC2
fviz_pca_ind(pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# Plot of variable loadings
fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = '#B4B4B4'  # Individuals color
)

pca.pca <- PCA(plots.norm, graph = F)
pca.hcpc <- HCPC(pca.pca, graph = F)

fviz_cluster(pca.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)

pca.3 <- data.frame(pca$x[,1:3])
pca.3
pca3.ed <- data.frame(EucDist = as.matrix(dist(pca.3, method = 'euclidean'))[1,])
pca3.ed <- data.frame(EucDist = sqrt((pca.3$PC1^2)+(pca.3$PC2^2)+(pca.3$PC3^2)))
pca3.sel <- sort(pca3.ed$EucDist, index.return = T, decreasing = T)$ix[1:10]
pca3.sel
pca3.ed

sitetopos$PCA3 <- 'Not selected'
sitetopos$PCA3[pca3.sel] <- 'Selected'
sitetopos$PCA3 <- as.factor(sitetopos$PCA3)
sitetopos$PCA3 <- factor(sitetopos$PCA3, levels = rev(levels(sitetopos$PCA3)))

topos_cut$PCA3 <- sitetopos$PCA3
topos_cut$EucDist <- pca3.ed$EucDist

View(topos_cut)
View(sitetopos.all)

pca3d(pca, components = 1:3, group=sitetopos$PCA3, legend = 'topleft', show.labels =T)


# Helper function 
var_coord_func <- function(loadings, comp.sdev){
  loadings*comp.sdev
}

# Compute coordinates of each factor with respect to principal components
loadings <- pca$rotation
sdev <- pca$sdev
var.coord <- t(apply(loadings, 1, var_coord_func, sdev)) 
head(var.coord[, 1:4])

# Compute cos2 (importance of a principal component for each observation)
var.cos2 <- var.coord^2
head(var.cos2[, 1:4])

# Compute contributions of variables
comp.cos2 <- apply(var.cos2, 2, sum)
contrib <- function(var.cos2, comp.cos2){var.cos2*100/comp.cos2}
var.contrib <- t(apply(var.cos2,1, contrib, comp.cos2))
head(var.contrib)

# Compute coordinates of individuals
ind.coord <- pca$x
head(ind.coord)



#################
# End members
#################
sitetopos
plots.norm
asp <- sitetopos[1]
asp <- pi*asp/180
cos(asp)

plots.norm.df <- as.data.frame(plots.norm)
plots.norm.df$Aspect <- cos(asp)
plots.norm

sitetopos

#radiation: -northness, +elevation, -slope, + tpi
#wetness: +twi, -tpi, -slope, -elevation, +northness

radn <- -1*plots.norm.df$Aspect + plots.norm.df$`Elevation (m)` + (-1*plots.norm.df$Slope) + plots.norm.df$TPI
radn

radn <- plots.norm.df$Radiation


moist <- plots.norm.df$TWI - plots.norm.df$TPI + plots.norm.df$`Elevation (m)`

exposure <- data.frame('RadiationIndex' = radn, 'MoistureIndex' = moist)
exposure
plot(exposure$Moisture ~exposure$Radiation, cex=2.2)
text(exposure$Moisture ~exposure$Radiation, labels=rownames(exposure), data=exposure, cex=0.9, font=2)

View(data.frame(sitetopos$Site_Name))

pcasub <- c(2,3,4,5,9,14,20,25,30,31,32,35,36,38)
ps.asp <- sitetopos[pcasub,]

sitetopos$PCA <- 'Not selected'
sitetopos$PCA[c(2,3,4,5,9,14,20,25,30,31,32,35,36,38)] <- 'Selected'
sitetopos$PCA <- as.factor(sitetopos$PCA)
sitetopos$PCA <- factor(sitetopos$PCA, levels = rev(levels(sitetopos$PCA)))

sitetopos$MoisRad <- 'Not selected'
sitetopos$MoisRad[c(2,5,9,11,20,25,35,36,37,38,39)] <- 'Selected'
sitetopos$MoisRad <- as.factor(sitetopos$MoisRad)
sitetopos$MoisRad <- factor(sitetopos$MoisRad, levels = rev(levels(sitetopos$MoisRad)))
sitetopos$MoisRad

sitetopos$Conv <- 'Not selected'
sitetopos$Conv[c(1,3,5,7,8,9,10,15,16,26,28,29,32,35,36,37)] <- 'Selected'
sitetopos$Conv <- as.factor(sitetopos$Conv)
sitetopos$Conv <- factor(sitetopos$Conv, levels = rev(levels(sitetopos$Conv)))
sitetopos$Conv
