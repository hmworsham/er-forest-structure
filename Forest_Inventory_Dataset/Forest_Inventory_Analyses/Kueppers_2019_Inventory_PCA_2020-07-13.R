##Script to run PCA on plot characteristic factors

##Author: Marshall Worsham
##Updated: 07-13-2020

# Set up workspace
## Install and load libraries
pkgs <- c('dplyr',
          'tidyverse',
          'ggplot2',
          'raster',
          'rgdal',
          'ggfortify',
          'caret',
          'factoextra') # Name the packages you want to use here
load.pkgs <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} # Function to install new packages if they're not already installed
load.pkgs(pkgs) # Runs the function on the list of packages defined in pkgs

# Set working directory.
setwd('~/Desktop/RMBL/Projects/Forest_Inventory_Dataset/')
indir <- paste0(getwd(), '/Source')
outdir <- paste0(getwd(), '/Output')

# Ingest plot csv
plot.data <- read.csv(paste0(indir, '/Kueppers_2019_Plots.csv'))
plots.df <- data.frame(plot.data[-c(1,6,7)], stringsAsFactors = F)
plots.df
rownames(plots.df) <- plot.data[,1]
plots.mat <- as.matrix(plots.df)

# Explore distribution
elv <- unlist(plots.df[1])
rad <- unlist(plots.df[2])
twi <- unlist(plots.df[3])
slope <- unlist(plots.df[4])

plot(density(elv))
plot(density(rad))
plot(density(twi))
plot(density(slope))

# Center means and scale 
rescaled <- preProcess(plots.mat, method=c("center", "scale"))

# Normalize
plots.norm <- predict(rescaled, plots.mat)
heatmap(plots.norm)
plots.norm

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

groups <- as.factor(plot.data$Dominance)
fviz_pca_biplot(pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = '#B4B4B4'  # Individuals color
)

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
