#### Combine Separate CSV Files

# Load Packages
library(plyr)
library(readr)


setwd('~/Desktop/RMBL/Projects/Forest_Inventory_Dataset/')

indir <- paste0(getwd(), '/Source')
outdir <- paste0(getwd(), '/Output')


## List filenames to be merged. Put all files into one folder and "path" should be the path to this folder. 
# filenames <- list.files(path="x",pattern="*.csv"); x = your path name
filenames <- list.files(indir, pattern="*.csv")

## Full path to csv filenames
# fullpath=file.path("x",filenames); x = your path name
fullpath <- file.path(indir, filenames)

## Upload all files and create DataFrame
# dat_csv = ldply(fullpath, read_csv); 'dat_csv' can be named anything 
# inventory = ldply(fullpath, read_csv)
inventory <- read.csv(fullpath[1])

## Export DataFrame (df)
write.csv(inventory, "/Users/sarahhettema/Documents/Kueppers\ Lab\\inventory_plots.csv")

# Reclassify DBH_CM column as numberic 
inventory$DBH_CM <- as.numeric(inventory$DBH_CM)

# Create df for small and big trees
## greater than or equal to 10cm
inventory_tagged <- subset(inventory, DBH_CM>=10)
## less than 10cm 
inventory_sappling <- subset(inventory, DBH_CM<10)


######### TWI, Elevation, and Radiation 
library(ggplot2)
# set working directory 
setwd("/Users/sarahhettema/Documents/Kueppers\ Lab")

## TWI
# read csv
TWI <- read.csv(file= 'Kueppers2019Plots_TWI.csv', header = T,)
# add values to make same graph in excel
TWI$NAME <- c("snodgrass_1", "Scho_24", "carbon_1", "snodgrass_2", "pointlookout_1", "splainsgultch_1", "carbon_2", "Scho_19", "Scho_23", "pointlookout_2")
names(TWI)[2] <- "Site_Name"
#### Plot TWI 
TWI_mean <-ggplot(TWI, aes(x= factor(Site_Name, levels = c("snodgrass_1", "Scho_24", "carbon_1", "snodgrass_2", "pointlookout_1", "splainsgultch_1", "carbon_2", "Scho_19", "Scho_23", "pointlookout_2")), y=MEAN)) + 
  geom_point() + 
  geom_path(group=1) +
  ggtitle("TWI") +
  labs(x ="Plot", y = "TWI") +
  theme(legend.position = "top", axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

## Radiation
Radiation <-read.csv(file= 'Kueppers2019Plots_Radiation.csv', header = T,)
Radiation$NAME <- c("pointlookout_1", "carbon_2", "Scho_24", "splainsgultch_1", "pointlookout_2", "Scho_19", "Scho_23", "snodgrass_1",  "carbon_1", "snodgrass_2")
names(Radiation)[2] <- "Site_Name"
#### Plot Radiation
radiation_mean<-ggplot(Radiation, aes(x= factor(Site_Name, levels = c("pointlookout_1", "carbon_2", "Scho_24", "splainsgultch_1", "pointlookout_2", "Scho_19", "Scho_23", "snodgrass_1",  "carbon_1", "snodgrass_2")), y=MEAN)) + 
  geom_point() + 
  geom_path(group=1) +
  ggtitle("Radiation") +
  labs(x ="Plot", y = "Radiation") +
  theme(legend.position = "top", axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

## Elevation
Elevation <- read.csv("Kueppers2019Plots_Elevation.csv", header = T,)
Elevation$NAME <- c("Scho_24", "pointlookout_1", "Scho_19", "carbon_1", "pointlookout_2", "splainsgultch_1", "snodgrass_1", "snodgrass_2", "Scho_23", "carbon_2")  
names(Elevation)[2] <- "Site_Name"
#### Plot Elevation
elevation_mean<- ggplot(Elevation, aes(x= factor(Site_Name, levels = c("Scho_24", "pointlookout_1", "Scho_19", "carbon_1", "pointlookout_2", "splainsgultch_1", "snodgrass_1", "snodgrass_2", "Scho_23", "carbon_2")), y=MEAN)) +
  geom_point() + 
  geom_path(group=1) +
  ggtitle("Elevation") +
  labs(x ="Plot", y = "Elevation") +
  theme(legend.position = "top", axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#### Species Composition 

## Work with df - inventory 
# create table with counts of species within a site 
inven_table <- table(inventory$Site_Name, inventory$Sp)
tagged_table <- table(inventory_tagged$Site_Name, inventory_tagged$Sp)
sappling_table <- table(inventory_sappling$Site_Name, inventory_sappling$Sp)

# use counts to find propotions of each species within the site 
inven_prop <- prop.table(inven_table, margin= 1)
tagged_prop <- prop.table(tagged_table, margin= 1)
sappling_prop <- prop.table(sappling_table, margin= 1)
inven_prop

# Initial plot to visualize the data (with entire inventory) 
library(ggplot2)
sc_plot <- ggplot(as.data.frame(inven_prop), aes(x = Var1, y = Freq, fill = Var2)) + 
  geom_bar(stat="identity", position = "fill") +
  scale_y_continuous(labels = scales::percent, name = "Proportion")
sc_plot

## Sort the data based on ABLA composition
library("data.table")
# create data frame of proportion table (inventory)
data_ordered <- as.data.frame(inven_prop)
tagged_prop <- as.data.frame(tagged_prop)
sappling_prop <- as.data.frame(sappling_prop)

# order data frame based on species and proportion  (inventory)
setorder(data_ordered, Var2, Freq)
setorder(tagged_prop, Var2, Freq)
setorder(sappling_prop, Var2, Freq)
## use the reordered data to determine the order of the plots composition of ABLA from lowest to highest (inventory)
data_ordered
tagged_prop
sappling_prop

## Manually reorder plot bars to reflects highest to lowest propostion of ABLA and manually reorder the fill of bars so ABLA is on top - use 'factor(variable, levels = c("label", "label", "..."))'

sc_ordered <- ggplot(data_ordered, aes(x = factor(Var1, levels = c("Scho_24", "splainsgultch_1",   "snodgrass_2", "pointlookout_2", "snodgrass_1", "Not in Plot", "carbon_2", "Scho_19", "Scho_23", "pointlookout_1", "carbon_1")), y = Freq, fill = factor(Var2, levels=c("ABLA", "PIEN", "PICO", "PHOTO", "QUTR", "POTR", "PSME", "UNKNOWN"))))  + 
  geom_bar(stat="identity") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  ggtitle("Species Composition") +
  theme(legend.position = "top") +
  labs(y= "Freqency", x="Plot", fill= "Species") +
  geom_text(aes(label= round(Freq *100 , digits = 2)), size = 3, position = position_stack(vjust = 0.5))
sc_ordered


## Create data set that includes all other species but excludes ABLA 
# subset of just ABLA in ordered data set
abla <- as.data.frame(subset(data_ordered, data_ordered$Var2=="ABLA"))
abla_tagged <- as.data.frame(subset(tagged_prop, tagged_prop$Var2=="ABLA"))
abla_sappling<- as.data.frame(subset(sappling_prop, sappling_prop$Var2=="ABLA"))

# Rename column header for Tagged Trees 
names(abla)[1]<- "Site_Name"
names(abla_tagged)[1]<- "Site_Name"
names(abla_sappling)[1]<- "Site_Name"

# subtract from 1 - gives the proportion of other species (inventory)
no_abla<- 1 - abla
# Need to manuallly add back in the site names to data frame
no_abla$Var1 <- c("carbon_1", "pointlookout_1", "Scho_23", "Scho_19", "carbon_2", "Not in Plot", "snodgrass_1", "pointlookout_2", "snodgrass_2", "splainsgultch_1", "Scho_24")

### Species Composition Graph with correct order and Trend line  
sc_final<- ggplot(data_ordered, aes(x = factor(Var1, levels = c("Scho_24", "splainsgultch_1",   "snodgrass_2", "pointlookout_2", "snodgrass_1", "Not in Plot", "carbon_2", "Scho_19", "Scho_23", "pointlookout_1", "carbon_1")), y = Freq, fill = Var2))  + 
  geom_bar(stat="identity") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  ggtitle("Species Composition") +
  theme(legend.position = "top", axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  labs(x="Plots", fill= "Species") +
  geom_text(aes(label= round(Freq *100 , digits = 2)), size = 3, position = position_stack(vjust = 0.5)) +
  geom_point(data=no_abla, aes(x = Var1, y = Freq)) +
  geom_path(data=no_abla, aes(x = Var1, y = Freq, group=1))

## Species Compostion Graph without proportion lables (cleaner look)
# Inventory (to add back in legend change theme(legend.position = "top"))
sc_nolabel<- ggplot(data_ordered, aes(x = factor(Var1, levels = c("Scho_24", "splainsgultch_1", "snodgrass_2", "pointlookout_2", "snodgrass_1", "Not in Plot", "carbon_2", "Scho_19", "Scho_23", "pointlookout_1", "carbon_1")), y = Freq, fill = Var2))  + 
  geom_bar(stat="identity") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  ggtitle("Species Composition") +
  theme(legend.position = "none", axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  labs(x="Plots", fill= "Species") +
  geom_point(data=no_abla, aes(x = Var1, y = Freq)) +
  geom_path(data=no_abla, aes(x = Var1, y = Freq, group=1)) +
  scale_fill_brewer(palette = "PuBuGn", direction = -1)
sc_nolabel

# Tagged

sc_tagged<- ggplot(tagged_prop, aes(x = factor (Var1, levels = c("Scho_24", "splainsgultch_1", "Scho_19", "snodgrass_2", "Not in Plot", "Scho_23", "pointlookout_1", "carbon_2", "snodgrass_1", "pointlookout_2", "carbon_1")), y = Freq, fill = Var2))  + 
  geom_bar(stat="identity") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  ggtitle("Species Composition of Tagged Trees") +
  theme(legend.position = "none", axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  labs(x="Plots", fill= "Species") +
  scale_fill_brewer(palette = "PuBuGn", direction = -1)
# Sappling 
sc_sappling<- ggplot(sappling_prop, aes(x = factor(Var1, levels= c("pointlookout_2", "Scho_24", "splainsgultch_1", "snodgrass_1", "snodgrass_2", "carbon_1", "carbon_2", "Scho_19", "Scho_23", "pointlookout_1")), y = Freq, fill = Var2))  + 
  geom_bar(stat="identity") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  ggtitle("Species Composition of Sapplings") +
  theme(legend.position = "none", axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  labs(x="Plots", fill= "Species") +
  scale_fill_brewer(palette = "PuBuGn", direction = -1)
 
# ledend for species composition
legend_sc <- get_legend(sc_nolabel)

## Merge df together (abla with TWI, Elevation, and Radiation)
abla_elevation <- merge(abla, Elevation[, c("Site_Name", "MEAN")], by= "Site_Name")
abla_elevation_radiation <- merge(abla_elevation, Radiation[, c("Site_Name", "MEAN")], by= "Site_Name")
abla_index <- merge(abla_elevation_radiation, TWI[, c("Site_Name", "MEAN")], by= "Site_Name")
# Tagged
abla_elevation_tagged <- merge(abla_tagged, Elevation[, c("Site_Name", "MEAN")], by= "Site_Name")
abla_elevation_radiation_tagged <- merge(abla_elevation_tagged, Radiation[, c("Site_Name", "MEAN")], by= "Site_Name")
abla_index_tagged <- merge(abla_elevation_radiation_tagged, TWI[, c("Site_Name", "MEAN")], by= "Site_Name")
# Sappling 
abla_elevation_sappling <- merge(abla_sappling, Elevation[, c("Site_Name", "MEAN")], by= "Site_Name")
abla_elevation_radiation_sappling <- merge(abla_elevation_sappling, Radiation[, c("Site_Name", "MEAN")], by= "Site_Name")
abla_index_sappling <- merge(abla_elevation_radiation_sappling, TWI[, c("Site_Name", "MEAN")], by= "Site_Name")

## rename colunms 
names(abla_index)[4]<- "Elevation_Mean"
names(abla_index)[5]<- "Radiation_Mean"
names(abla_index)[6]<- "TWI_Mean"
# Tagged
names(abla_index_tagged)[4]<- "Elevation_Mean"
names(abla_index_tagged)[5]<- "Radiation_Mean"
names(abla_index_tagged)[6]<- "TWI_Mean"
# Sappling 
names(abla_index_sappling)[4]<- "Elevation_Mean"
names(abla_index_sappling)[5]<- "Radiation_Mean"
names(abla_index_sappling)[6]<- "TWI_Mean"

## remove extra columns 
abla_index <- subset(abla_index, select = -c(Var2))
abla_index_tagged <- subset(abla_index_tagged, select = -c(Var2))
abla_index_sappling <- subset(abla_index_sappling, select = -c(Var2))


## Scater Plots of Species Composition and Index Values 
sc_elevation <- ggplot() +
  geom_point(data = abla_index, aes(Freq *100, Elevation_Mean, color = Site_Name)) +
  labs(title= "Species Composition VS Elevation", x="Percent Composition of ABLA", y= "Mean Elevation", color= "Site Name") +
  theme(legend.position = "none")
sc_radiation<- ggplot() +
  geom_point(data = abla_index, aes(Freq *100, Radiation_Mean, color = Site_Name)) +
  labs(title= "Species Composition VS Radiation", x="Percent Composition of ABLA", y= "Mean Radiation", color= "Site Name") +
  theme(legend.position = "none")
sc_TWI <- ggplot() +
  geom_point(data = abla_index, aes(Freq *100, TWI_Mean, color = Site_Name)) +
  labs(title= "Species Composition VS TWI", x="Percent Composition of ABLA", y= "Mean TWI", color= "Site Name") +
  theme(legend.position = "none")

# Tagged
sc_elevation_tagged<- ggplot() +
  geom_point(data = abla_index_tagged, aes(Freq *100, Elevation_Mean, color = Site_Name)) +
  labs(title= "Species Composition VS Elevation for Tagged Trees", x="Percent Composition of ABLA", y= "Mean Elevation", color= "Site Name") +
  theme(legend.position = "none")
sc_radiation_tagged<- ggplot() +
  geom_point(data = abla_index_tagged, aes(Freq *100, Radiation_Mean, color = Site_Name)) +
  labs(title= "Species Composition VS Radiation for Tagged Trees", x="Percent Composition of ABLA", y= "Mean Radiation", color= "Site Name") +
  theme(legend.position = "none")
sc_TWI_tagged <- ggplot() +
  geom_point(data = abla_index_tagged, aes(Freq *100, TWI_Mean, color = Site_Name)) +
  labs(title= "Species Composition VS TWI for Tagged Trees", x="Percent Composition of ABLA", y= "Mean TWI", color= "Site Name") +
  theme(legend.position = "none")

# Sappling
sc_elevation_sappling<- ggplot() +
  geom_point(data = abla_index_sappling, aes(Freq *100, Elevation_Mean, color = Site_Name)) +
  labs(title= "Species Composition VS Elevation for Sapplings", x="Percent Composition of ABLA", y= "Mean Elevation", color= "Site Name") +
  theme(legend.position = "none")
sc_radiation_sappling<- ggplot() +
  geom_point(data = abla_index_sappling, aes(Freq *100, Radiation_Mean, color = Site_Name)) +
  labs(title= "Species Composition VS Radiation for Sapplings", x="Percent Composition of ABLA", y= "Mean Radiation", color= "Site Name") +
  theme(legend.position = "none")
sc_TWI_sappling <- ggplot() +
  geom_point(data = abla_index_sappling, aes(Freq *100, TWI_Mean, color = Site_Name)) +
  labs(title= "Species Composition VS TWI for Sapplings", x="Percent Composition of ABLA", y= "Mean TWI", color= "Site Name") +
  theme(legend.position = "none")


library(gridExtra)
legend_scatter <- get_legend(sc_elevation)

ggsave(filename="compare_sc.pdf", plot = compare_sc, device="pdf", path = "/Users/sarahhettema/Documents/site_inventory", scale = 1, dpi = 300, limitsize = T )  

#### Total Number Density

library(dplyr)

# count number of trees in each site
## nrow(subset(inventory, inventory$Site_Name =="sitename"))
# convert m^2 of site polygon to hectares
## ha_sitename <- polygon_area/10000

## Schofield 24 (1)
trees_scho24 <- nrow(subset(inventory, inventory$Site_Name == "Scho_24"))
tagged_scho24 <- nrow(subset(inventory_tagged, inventory_tagged$Site_Name == "Scho_24"))
sappling_scho24 <- nrow(subset(inventory_sappling, inventory_sappling$Site_Name == "Scho_24"))
ha_scho24 <- 1636.32150306000/10000

## Schofield 23 (2)
trees_scho23 <- nrow(subset(inventory, inventory$Site_Name == "Scho_23"))
tagged_scho23 <- nrow(subset(inventory_tagged, inventory_tagged$Site_Name == "Scho_23"))
sappling_scho23 <- nrow(subset(inventory_sappling, inventory_sappling$Site_Name == "Scho_23"))
ha_scho23 <- 1651.17926906000/10000

## Schofield 19 (3)
trees_scho19<-nrow(subset(inventory, inventory$Site_Name == "Scho_19"))
tagged_scho19<-nrow(subset(inventory_tagged, inventory_tagged$Site_Name == "Scho_19"))
sappling_scho19<-nrow(subset(inventory_sappling, inventory_sappling$Site_Name == "Scho_19"))
ha_scho19 <- 2037.52233133000/10000

## Point Lookout 1 (4)
trees_pl1 <- nrow(subset(inventory, inventory$Site_Name == "pointlookout_1"))
tagged_pl1 <- nrow(subset(inventory_tagged, inventory_tagged$Site_Name == "pointlookout_1"))
sappling_pl1 <- nrow(subset(inventory_sappling, inventory_sappling$Site_Name == "pointlookout_1"))
ha_pl1 <- 1234.67811702000/10000

## Point Lookout 2 (5)
trees_pl2<-nrow(subset(inventory, inventory$Site_Name == "pointlookout_2"))
tagged_pl2<-nrow(subset(inventory_tagged, inventory_tagged$Site_Name == "pointlookout_2"))
sappling_pl2<-nrow(subset(inventory_sappling, inventory_sappling$Site_Name == "pointlookout_2"))
ha_pl2 <- 1818.03289186000/10000

## Snodgrass 1 (6)
trees_snod1 <- nrow(subset(inventory, inventory$Site_Name == "snodgrass_1"))
tagged_snod1 <- nrow(subset(inventory_tagged, inventory_tagged$Site_Name == "snodgrass_1"))
sappling_snod1 <- nrow(subset(inventory_sappling, inventory_sappling$Site_Name == "snodgrass_1"))
ha_snod1 <- 1376.37962780000/10000

## Snodgrass 2 (7)
trees_snod2 <- nrow(subset(inventory, inventory$Site_Name == "snodgrass_2"))
tagged_snod2 <- nrow(subset(inventory_tagged, inventory_tagged$Site_Name == "snodgrass_2"))
sappling_snod2 <- nrow(subset(inventory_sappling, inventory_sappling$Site_Name == "snodgrass_2"))
ha_snod2 <- 1511.94994694000/10000

## Splains Gultch 1 (8)
trees_sg1 <- nrow(subset(inventory, inventory$Site_Name == "splainsgultch_1"))
tagged_sg1 <- nrow(subset(inventory_tagged, inventory_tagged$Site_Name == "splainsgultch_1"))
sappling_sg1 <- nrow(subset(inventory_sappling, inventory_sappling$Site_Name == "splainsgultch_1"))
ha_sg1 <- 1278.08171523000/10000

## Carbon 1 (9)
trees_c1 <- nrow(subset(inventory, inventory$Site_Name == "carbon_1"))
tagged_c1 <- nrow(subset(inventory_tagged, inventory_tagged$Site_Name == "carbon_1"))
sappling_c1 <- nrow(subset(inventory_sappling, inventory_sappling$Site_Name == "carbon_1"))
ha_c1 <- 1516.84718392000/10000

## Carbon 2 (10)
trees_c2 <- nrow(subset(inventory, inventory$Site_Name == "carbon_2"))
tagged_c2 <- nrow(subset(inventory_tagged, inventory_tagged$Site_Name == "carbon_2"))
sappling_c2 <- nrow(subset(inventory_sappling, inventory_sappling$Site_Name == "carbon_2"))
ha_c2 <- 1287.17197038000/10000


## make data frame with site names, tree counts, and trees/ha  
trees <- c(trees_scho24, trees_sg1, trees_snod2, trees_pl2, trees_snod1, trees_c2, trees_scho19, trees_scho23,  trees_pl1,  trees_c1)
ha <- c(ha_scho24, ha_sg1, ha_snod2, ha_pl2, ha_snod1, ha_c2, ha_scho19, ha_scho23, ha_pl1,  ha_c1)
site <-c("Scho_24", "splainsgultch_1", "snodgrass_2", "pointlookout_2", "snodgrass_1", "carbon_2", "Scho_19", "Scho_23", "pointlookout_1", "carbon_1")
tree_ha <- data.frame(site, trees, ha, trees/ha)

# Tagged
tagged <- c(tagged_scho24, tagged_sg1, tagged_snod2, tagged_pl2, tagged_snod1, tagged_c2, tagged_scho19, tagged_scho23, tagged_pl1, tagged_c1)
ha <- c(ha_scho24, ha_sg1, ha_snod2, ha_pl2, ha_snod1, ha_c2, ha_scho19, ha_scho23, ha_pl1,  ha_c1)
site <-c("Scho_24", "splainsgultch_1", "snodgrass_2", "pointlookout_2", "snodgrass_1", "carbon_2", "Scho_19", "Scho_23", "pointlookout_1", "carbon_1")
tagged_ha <- data.frame(site, tagged, ha, tagged/ha)

# Sappling 
sappling <- c(sappling_scho24, sappling_sg1, sappling_snod2, sappling_pl2, sappling_snod1, sappling_c2, sappling_scho19, sappling_scho23, sappling_pl1, sappling_c1)
ha <- c(ha_scho24, ha_sg1, ha_snod2, ha_pl2, ha_snod1, ha_c2, ha_scho19, ha_scho23, ha_pl1,  ha_c1)
site <-c("Scho_24", "splainsgultch_1", "snodgrass_2", "pointlookout_2", "snodgrass_1", "carbon_2", "Scho_19", "Scho_23", "pointlookout_1", "carbon_1")
sappling_ha <- data.frame(site, sappling, ha, sappling/ha)

### plot total number density 
total_density <- ggplot()  + 
  geom_bar(data = tree_ha, aes(reorder(site, trees/ha), trees/ha), stat="identity", fill="cornflowerblue") +
  ggtitle("Total Number Density") +
  labs(x="Plots", y= "Total Number Desity")  +
  theme(legend.position = "none", axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  geom_point(data = tree_ha, aes(reorder(site, trees/ha), trees/ha)) +
  geom_line(data = tree_ha, aes(site, trees/ha, group = 1))
total_density

# Tagged
total_density_tagged <- ggplot()  + 
  geom_bar(data = tagged_ha, aes(reorder(site, tagged/ha), tagged/ha), stat="identity", fill="cornflowerblue") +
  ggtitle("Total Number Density - Tagged Trees") +
  labs(x="Plots", y= "Total Number Desity")  +
  theme(legend.position = "none", axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  geom_point(data = tagged_ha, aes(reorder(site, tagged/ha), tagged/ha)) +
  geom_line(data = tagged_ha, aes(site, tagged/ha, group = 1))
# Sappling 
total_density_sappling <- ggplot()  + 
  geom_bar(data = sappling_ha, aes(reorder(site, sappling/ha), sappling/ha), stat="identity", fill="cornflowerblue") +
  ggtitle("Total Number Density - Sapplings") +
  labs(x="Plots", y= "Total Number Desity")  +
  theme(legend.position = "none", axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  geom_point(data = sappling_ha, aes(reorder(site, sappling/ha), sappling/ha)) +
  geom_line(data = sappling_ha, aes(site, sappling/ha, group = 1))


# Rename to Merge df
names(tree_ha)[1]<- "Site_Name"
names(tagged_ha)[1]<- "Site_Name"
names(sappling_ha)[1]<- "Site_Name"

## Merge
density_elevation <- merge(tree_ha, Elevation[, c("Site_Name", "MEAN")], by= "Site_Name")
density_elevation_radiation <- merge(density_elevation, Radiation[, c("Site_Name", "MEAN")], by= "Site_Name")
density_index <- merge(density_elevation_radiation, TWI[, c("Site_Name", "MEAN")], by= "Site_Name")
#Tagged
density_elevation_tagged <- merge(tagged_ha, Elevation[, c("Site_Name", "MEAN")], by= "Site_Name")
density_elevation_radiation_tagged  <- merge(density_elevation_tagged, Radiation[, c("Site_Name", "MEAN")], by= "Site_Name")
density_index_tagged  <- merge(density_elevation_radiation_tagged, TWI[, c("Site_Name", "MEAN")], by= "Site_Name")
# Sappling
density_elevation_sappling <- merge(sappling_ha, Elevation[, c("Site_Name", "MEAN")], by= "Site_Name")
density_elevation_radiation_sappling <- merge(density_elevation_sappling, Radiation[, c("Site_Name", "MEAN")], by= "Site_Name")
density_index_sappling <- merge(density_elevation_radiation_sappling, TWI[, c("Site_Name", "MEAN")], by= "Site_Name")


## Rename Colunms 
names(density_index)[5]<- "Elevation_Mean"
names(density_index)[6]<- "Radiation_Mean"
names(density_index)[7]<- "TWI_Mean"
# Tagged
names(density_index_tagged)[5]<- "Elevation_Mean"
names(density_index_tagged)[6]<- "Radiation_Mean"
names(density_index_tagged)[7]<- "TWI_Mean"
density_index
# Sappling
names(density_index_sappling)[5]<- "Elevation_Mean"
names(density_index_sappling)[6]<- "Radiation_Mean"
names(density_index_sappling)[7]<- "TWI_Mean"


## Scater Plots 
density_elevation<- ggplot() +
  geom_point(data = density_index, aes(trees.ha, Elevation_Mean, color = Site_Name)) +
  labs(title= "Total Number Density VS Elevation", x="Trees per Hectare", y= "Mean Elevation", color= "Site Name") +
  theme(legend.position = "none")
density_radiation<- ggplot() +
  geom_point(data = density_index, aes(trees.ha, Radiation_Mean, color = Site_Name)) +
  labs(title= "Total Number Density VS Radiation", x="Trees per Hectare", y= "Mean Radiation", color= "Site Name") +
  theme(legend.position = "none")
density_TWI <- ggplot() +
  geom_point(data = density_index, aes(trees.ha, TWI_Mean, color = Site_Name)) +
  labs(title= "Total Number Density VS TWI", x="Trees per Hectare", y= "Mean TWI", color= "Site Name")+
  theme(legend.position = "none")
# Tagged
density_elevation_tagged <- ggplot() +
  geom_point(data = density_index_tagged, aes(tagged.ha, Elevation_Mean, color = Site_Name)) +
  labs(title= "Total Number Density VS Elevation for Tagged Trees", x="Trees per Hectare", y= "Mean Elevation", color= "Site Name")+
  theme(legend.position = "none")
density_radiation_tagged<- ggplot() +
  geom_point(data = density_index_tagged, aes(tagged.ha, Radiation_Mean, color = Site_Name)) +
  labs(title= "Total Number Density VS Radiation for Tagged Trees", x="Trees per Hectare", y= "Mean Radiation", color= "Site Name")+
  theme(legend.position = "none")
density_TWI_tagged <- ggplot() +
  geom_point(data = density_index_tagged, aes(tagged.ha, TWI_Mean, color = Site_Name)) +
  labs(title= "Total Number Density VS TWI for Tagged Trees", x="Trees per Hectare", y= "Mean TWI", color= "Site Name")+
  theme(legend.position = "none")
# Sappling
density_elevation_sappling<- ggplot() +
  geom_point(data = density_index_sappling, aes(sappling.ha, Elevation_Mean, color = Site_Name)) +
  labs(title= "Total Number Density VS Elevation for Sappling", x="Trees per Hectare", y= "Mean Elevation", color= "Site Name")+
  theme(legend.position = "none")
density_radiation_sappling<- ggplot() +
  geom_point(data = density_index_sappling, aes(sappling.ha, Radiation_Mean, color = Site_Name)) +
  labs(title= "Total Number Density VS Radiation for Sappling", x="Trees per Hectare", y= "Mean Radiation", color= "Site Name")+
  theme(legend.position = "none")
density_TWI_sappling <- ggplot() +
  geom_point(data = density_index_sappling, aes(sappling.ha, TWI_Mean, color = Site_Name)) +
  labs(title= "Total Number Density VS TWI for Sappling", x="Trees per Hectare", y= "Mean TWI", color= "Site Name")+
  theme(legend.position = "none")


#### Species Total Density 

# use df with proprtion of each species to determine what species are present in each plot
inven_prop

# call stringr
library(stringr)

#Scho_24
scho_24_sp <- subset(inventory, inventory$Site_Name == "Scho_24", select =  Sp)
scho_24_sp_ABLA <- str_count(scho_24_sp, pattern = "ABLA")
scho_24_sp_PIEN <- str_count(scho_24_sp, pattern = "PIEN")
# Splainsgultch 1 
sg_1_sp <-subset(inventory, inventory$Site_Name == "splainsgultch_1", select =  Sp)
sg_1_sp_ABLA <- str_count(sg_1_sp, pattern = "ABLA")
sg_1_sp_PIEN <- str_count(sg_1_sp, pattern = "PIEN")
sg_1_sp_UNKNOWN <- str_count(sg_1_sp, pattern = "UNKNOWN")
# Snodgrass 2 
snod_2_sp <- subset(inventory, inventory$Site_Name == "snodgrass_2", select =  Sp)
snod_2_sp_ABLA <- str_count(snod_2_sp, pattern = "ABLA")
snod_2_sp_PIEN <- str_count(snod_2_sp, pattern = "PIEN")
snod_2_sp_PICO <- str_count(snod_2_sp, pattern = "PICO")
snod_2_sp_UNKNOWN <- str_count(snod_2_sp, pattern = "UNKNOWN")
# Pointlookout 2
pl_2_sp <- subset(inventory, inventory$Site_Name == "pointlookout_2", select =  Sp)
pl_2_sp_ABLA <- str_count(pl_2_sp, pattern = "ABLA")
pl_2_sp_PIEN <- str_count(pl_2_sp, pattern = "PIEN")
pl_2_sp_PICO <- str_count(pl_2_sp, pattern = "PICO")
pl_2_sp_UNKNOWN <- str_count(pl_2_sp, pattern = "UNKNOWN")
# Snodgrass 1
snod_1_sp <- subset(inventory, inventory$Site_Name == "snodgrass_1", select =  Sp)
snod_1_sp_ABLA <- str_count(snod_1_sp, pattern = "ABLA")
snod_1_sp_PIEN <- str_count(snod_1_sp, pattern = "PIEN")
snod_1_sp_PICO <- str_count(snod_1_sp, pattern = "PICO")
snod_1_sp_POTR <- str_count(snod_1_sp, pattern = "POTR")
snod_1_sp_UNKNOWN <- str_count(snod_1_sp, pattern = "UNKNOWN")
# Carbon 2
c_2_sp <- subset(inventory, inventory$Site_Name == "carbon_2", select =  Sp)
c_2_sp_ABLA <- str_count(c_2_sp, pattern = "ABLA")
c_2_sp_PIEN <- str_count(c_2_sp, pattern = "PIEN")
c_2_sp_UNKNOWN <- str_count(c_2_sp, pattern = "UNKNOWN")
# Scho 19
scho_19_sp <- subset(inventory, inventory$Site_Name == "Scho_19", select =  Sp)
scho_19_sp_ABLA <- str_count(scho_19_sp, pattern = "ABLA")
scho_19_sp_PIEN <- str_count(scho_19_sp, pattern = "PIEN")
# Scho 23 
scho_23_sp <- subset(inventory, inventory$Site_Name == "Scho_23", select =  Sp)
scho_23_sp_ABLA <- str_count(scho_23_sp, pattern = "ABLA")
scho_23_sp_PIEN <- str_count(scho_23_sp, pattern = "PIEN")
# Pointlookout 1
pl_1_sp <- subset(inventory, inventory$Site_Name == "pointlookout_1", select =  Sp)
pl_1_sp_ABLA <- str_count(pl_1_sp, pattern = "ABLA")
pl_1_sp_PIEN <- str_count(pl_1_sp, pattern = "PIEN")
pl_1_sp_PICO <- str_count(pl_1_sp, pattern = "PICO")
pl_1_sp_PHOTO <- str_count(pl_1_sp, pattern = "PHOTO")
pl_1_sp_POTR <- str_count(pl_1_sp, pattern = "POTR")
pl_1_sp_PSME <- str_count(pl_1_sp, pattern = "PSME")
pl_1_sp_UNKNOWN <- str_count(pl_1_sp, pattern = "UNKNOWN")
# Carbon 1
c_1_sp <- subset(inventory, inventory$Site_Name == "carbon_1", select =  Sp)
c_1_sp_ABLA <- str_count(c_1_sp, pattern = "ABLA")
c_1_sp_PIEN <- str_count(c_1_sp, pattern = "PIEN")
c_1_sp_PICO <- str_count(c_1_sp, pattern = "PICO")
c_1_sp_UNKNOWN <- str_count(c_1_sp, pattern = "UNKNOWN")

## Combine the counts of each species for all sites into one df
# make df
Site_Name <- c("Scho_24", "splainsgultch_1", "snodgrass_2", "pointlookout_2", "snodgrass_1", "carbon_2", "Scho_19", "Scho_23", "pointlookout_1", "carbon_1") 
ABLA <- c(scho_24_sp_ABLA, sg_1_sp_ABLA, snod_2_sp_ABLA, pl_2_sp_ABLA, snod_1_sp_ABLA, c_2_sp_ABLA, scho_19_sp_ABLA, scho_23_sp_ABLA, pl_1_sp_ABLA, c_1_sp_ABLA)
PIEN <- c(scho_24_sp_PIEN, sg_1_sp_PIEN, snod_2_sp_PIEN, pl_2_sp_PIEN, snod_1_sp_PIEN, c_2_sp_PIEN, scho_19_sp_PIEN, scho_23_sp_PIEN, pl_1_sp_PIEN, c_1_sp_PIEN)
PICO <- c(0, 0, snod_2_sp_PICO, pl_2_sp_PICO, snod_1_sp_PICO, 0, 0, 0, pl_1_sp_PICO, c_1_sp_PICO)
PHOTO <- c(0, 0, 0, 0, 0, 0, 0, 0, pl_1_sp_PHOTO, 0)
POTR <- c(0, 0, 0, 0, snod_1_sp_POTR, 0, 0, 0, pl_1_sp_POTR, 0)
PSME <- c(0, 0, 0, 0, 0, 0, 0, 0, pl_1_sp_PSME, 0)
UNKNOWN <- c(0, sg_1_sp_UNKNOWN, snod_2_sp_UNKNOWN, pl_2_sp_UNKNOWN, snod_1_sp_UNKNOWN, c_2_sp_UNKNOWN, 0, 0, pl_1_sp_UNKNOWN, c_1_sp_UNKNOWN )

# merge to create data frame with total tree count, size of plot, and count of each species 
sp_count<- data.frame(Site_Name, ABLA, PIEN, PICO, PHOTO, POTR, PSME, UNKNOWN)
sp_count_ha <- merge(tree_ha, sp_count)
# Data Frame to create graph
sp_count_ha

library(reshape2)
library(stringr)

# reformat data frame 
sp_ha_melt <- melt(sp_count_ha[,c('Site_Name','ABLA', 'PIEN', 'PICO', 'PHOTO', 'POTR', 'PSME', 'UNKNOWN')], id.vars = 1)
sp_ha_melt

# Graph with just species counts 
sp_count <- ggplot(sp_ha_melt, aes(x = factor(Site_Name, levels = c("splainsgultch_1", "Scho_23", "carbon_1", "snodgrass_1", "Scho_24", "Scho_19", "snodgrass_2", "pointlookout_1", "pointlookout_2", "carbon_2")), y = value, fill = factor(variable, levels = c('PIEN', 'PICO', 'PHOTO', 'POTR', 'PSME', 'UNKNOWN', 'ABLA'))))  + 
  geom_bar(stat="identity") +
  ggtitle("Species Total Density") +
  theme(legend.position = "top", axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  labs(x="Plots", fill= "Species") +
  scale_fill_brewer(palette = "RdYlBu", direction = -1)

library(reshape2)
# melt data so there is a new column of count/hectare 
sp_ha_melt <- melt(sp_count_ha[,c('Site_Name','ABLA', 'PIEN', 'PICO', 'PHOTO', 'POTR', 'PSME', 'UNKNOWN')], id.vars = 1)
## add the indvidual species composition value to the graph
# create vector with values
sp_ha<- c(sp_count_ha$ABLA/sp_count_ha$ha, sp_count_ha$PIEN/sp_count_ha$ha, sp_count_ha$PICO/sp_count_ha$ha, sp_count_ha$PHOTO/sp_count_ha$ha, sp_count_ha$POTR/sp_count_ha$ha, sp_count_ha$PSME/sp_count_ha$ha, sp_count_ha$UNKNOWN/sp_count_ha$ha)
# add row to data frame 
sp_ha_melt$sp_ha <- c(sp_ha)

# Data Frame for graph 
sp_ha_melt

library(ggplot2)
# graph with tree count / plot area per species 
sp_total_density <- ggplot()  + 
  geom_bar(data = sp_ha_melt, aes(reorder(Site_Name, sp_ha), sp_ha, fill = factor(variable)), stat="identity") +
  ggtitle("Species Total Density") +
  geom_point(data = tree_ha, aes(reorder(Site_Name, trees.ha), trees.ha)) +
  theme(legend.position = "top", axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  labs(x="Plots", y= "Tree Count/ Plot Area (ha)", fill= "Species") +
  scale_fill_brewer(palette = "RdYlBu", direction = 1) +
  geom_line(data = tree_ha, aes(Site_Name, trees.ha, group = 1))

sp_total_density

####### Basal Area / Hectare 

# calculate radius 
radius <- inventory$DBH_CM/2
radius_tagged <- inventory_tagged$DBH_CM/2
radius_sappling <- inventory_sappling$DBH_CM/2
# calculate area
area <- (radius^2)*pi
area_tagged <- (radius_tagged^2)*pi
area_sappling <- (radius_sappling^2)*pi
# make DF
radius_area <- data.frame(inventory$Site_Name, inventory$DBH_CM, radius, area)
radius_area_tagged <- data.frame(inventory_tagged$Site_Name, inventory_tagged$DBH_CM, radius_tagged, area_tagged)
radius_area_sappling <- data.frame(inventory_sappling$Site_Name, inventory_sappling$DBH_CM, radius_sappling, area_sappling)

## find sum of area for 10 plots 
# Scho_19 
scho_19_BA <- sum(subset(radius_area, radius_area$inventory.Site_Name == "Scho_19", select = area), na.rm = T)
scho_19_BA_tagged <- sum(subset(radius_area_tagged, radius_area_tagged$inventory_tagged.Site_Name == "Scho_19", select = area_tagged), na.rm = T)
scho_19_BA_sappling <- sum(subset(radius_area_sappling, radius_area_sappling$inventory_sappling.Site_Name == "Scho_19", select = area_sappling), na.rm = T)
#Scho_24
scho_24_BA <- sum(subset(radius_area, radius_area$inventory.Site_Name == "Scho_24", select = area), na.rm = T)
scho_24_BA_tagged <- sum(subset(radius_area_tagged, radius_area_tagged$inventory_tagged.Site_Name == "Scho_24", select = area_tagged), na.rm = T)
scho_24_BA_sappling <- sum(subset(radius_area_sappling, radius_area_sappling$inventory_sappling.Site_Name == "Scho_24", select = area_sappling), na.rm = T)
# Splainsgultch 1 
sg_1_BA <- sum(subset(radius_area, radius_area$inventory.Site_Name == "splainsgultch_1", select = area), na.rm = T)
sg_1_BA_tagged <- sum(subset(radius_area_tagged, radius_area_tagged$inventory_tagged.Site_Name == "splainsgultch_1", select = area_tagged), na.rm = T)
sg_1_BA_sappling <- sum(subset(radius_area_sappling, radius_area_sappling$inventory_sappling.Site_Name == "splainsgultch_1", select = area_sappling), na.rm = T)
# Snodgrass 2 
snod_2_BA <- sum(subset(radius_area, radius_area$inventory.Site_Name == "snodgrass_2", select = area), na.rm = T)
snod_2_BA_tagged <- sum(subset(radius_area_tagged, radius_area_tagged$inventory_tagged.Site_Name == "snodgrass_2", select = area_tagged), na.rm = T)
snod_2_BA_sappling <- sum(subset(radius_area_sappling, radius_area_sappling$inventory_sappling.Site_Name == "snodgrass_2", select = area_sappling), na.rm = T)
# Pointlookout 2
pl_2_BA <- sum(subset(radius_area, radius_area$inventory.Site_Name == "pointlookout_2", select = area), na.rm = T)
pl_2_BA_tagged <- sum(subset(radius_area_tagged, radius_area_tagged$inventory_tagged.Site_Name == "pointlookout_2", select = area_tagged), na.rm = T)
pl_2_BA_sappling <- sum(subset(radius_area_sappling, radius_area_sappling$inventory_sappling.Site_Name == "pointlookout_2", select = area_sappling), na.rm = T)
# Snodgrass 1
snod_1_BA <- sum(subset(radius_area, radius_area$inventory.Site_Name == "snodgrass_1", select = area), na.rm = T)
snod_1_BA_tagged <- sum(subset(radius_area_tagged, radius_area_tagged$inventory_tagged.Site_Name == "snodgrass_1", select = area_tagged), na.rm = T)
snod_1_BA_sappling <- sum(subset(radius_area_sappling, radius_area_sappling$inventory_sappling.Site_Name == "snodgrass_1", select = area_sappling), na.rm = T)
# Carbon 2
c_2_BA <- sum(subset(radius_area, radius_area$inventory.Site_Name == "carbon_2", select = area), na.rm = T)
c_2_BA_tagged <- sum(subset(radius_area_tagged, radius_area_tagged$inventory_tagged.Site_Name == "carbon_2", select = area_tagged), na.rm = T)
c_2_BA_sappling <- sum(subset(radius_area_sappling, radius_area_sappling$inventory_sappling.Site_Name == "carbon_2", select = area_sappling), na.rm = T)
# Scho 19
scho_19_BA <- sum(subset(radius_area, radius_area$inventory.Site_Name == "Scho_19", select = area), na.rm = T)
scho_19_BA_tagged <- sum(subset(radius_area_tagged, radius_area_tagged$inventory_tagged.Site_Name == "Scho_19", select = area_tagged), na.rm = T)
scho_19_BA_sappling <- sum(subset(radius_area_sappling, radius_area_sappling$inventory_sappling.Site_Name == "Scho_19", select = area_sappling), na.rm = T)
# Scho 23 
scho_23_BA <-  sum(subset(radius_area, radius_area$inventory.Site_Name == "Scho_23", select = area), na.rm = T)
scho_23_BA_tagged <-  sum(subset(radius_area_tagged, radius_area_tagged$inventory_tagged.Site_Name == "Scho_23", select = area_tagged), na.rm = T)
scho_23_BA_sappling <-  sum(subset(radius_area_sappling, radius_area_sappling$inventory_sappling.Site_Name == "Scho_23", select = area_sappling), na.rm = T)
# Pointlookout 1
pl_1_BA <-  sum(subset(radius_area, radius_area$inventory.Site_Name == "pointlookout_1", select = area), na.rm = T)
pl_1_BA_tagged <-  sum(subset(radius_area_tagged, radius_area_tagged$inventory_tagged.Site_Name == "pointlookout_1", select = area_tagged), na.rm = T)
pl_1_BA_sappling <-  sum(subset(radius_area_sappling, radius_area_sappling$inventory_sappling.Site_Name == "pointlookout_1", select = area_sappling), na.rm = T)
# Carbon 1
c_1_BA <-  sum(subset(radius_area, radius_area$inventory.Site_Name == "carbon_1", select = area), na.rm = T)
c_1_BA_tagged <-  sum(subset(radius_area_tagged, radius_area_tagged$inventory_tagged.Site_Name == "carbon_1", select = area_tagged), na.rm = T)
c_1_BA_sappling <-  sum(subset(radius_area_sappling, radius_area_sappling$inventory_sappling.Site_Name == "carbon_1", select = area_sappling), na.rm = T)

## make data frame with basal area, area of site, and basal area per hectare
BA <- c(scho_24_BA, sg_1_BA, snod_2_BA, pl_2_BA, snod_1_BA, c_2_BA, scho_19_BA, scho_23_BA, pl_1_BA, c_1_BA)
ha <- c(ha_scho24, ha_sg1, ha_snod2, ha_pl2, ha_snod1, ha_c2, ha_scho19, ha_scho23, ha_pl1,  ha_c1)
Site_Name <-c("Scho_24", "splainsgultch_1", "snodgrass_2", "pointlookout_2", "snodgrass_1", "carbon_2", "Scho_19", "Scho_23", "pointlookout_1", "carbon_1")
BA_ha <- data.frame(Site_Name, BA, ha, BA/ha)

# Tagged
BA_tagged  <- c(scho_24_BA_tagged , sg_1_BA_tagged , snod_2_BA_tagged, pl_2_BA_tagged, snod_1_BA_tagged, c_2_BA_tagged, scho_19_BA_tagged, scho_23_BA_tagged, pl_1_BA_tagged, c_1_BA_tagged)
ha <- c(ha_scho24, ha_sg1, ha_snod2, ha_pl2, ha_snod1, ha_c2, ha_scho19, ha_scho23, ha_pl1,  ha_c1)
Site_Name <-c("Scho_24", "splainsgultch_1", "snodgrass_2", "pointlookout_2", "snodgrass_1", "carbon_2", "Scho_19", "Scho_23", "pointlookout_1", "carbon_1")
BA_ha_tagged <- data.frame(Site_Name, BA_tagged, ha, BA_tagged/ha)
# Sappling
BA_sappling <- c(scho_24_BA_sappling, sg_1_BA_sappling, snod_2_BA_sappling, pl_2_BA_sappling, snod_1_BA_sappling, c_2_BA_sappling, scho_19_BA_sappling, scho_23_BA_sappling, pl_1_BA_sappling, c_1_BA_sappling)
ha <- c(ha_scho24, ha_sg1, ha_snod2, ha_pl2, ha_snod1, ha_c2, ha_scho19, ha_scho23, ha_pl1,  ha_c1)
Site_Name <-c("Scho_24", "splainsgultch_1", "snodgrass_2", "pointlookout_2", "snodgrass_1", "carbon_2", "Scho_19", "Scho_23", "pointlookout_1", "carbon_1")
BA_ha_sappling <- data.frame(Site_Name, BA_sappling, ha, BA_sappling/ha)

library(ggplot2)
# Graph
BA_HA <- ggplot()+ 
  geom_bar(data = BA_ha, aes(reorder(Site_Name, BA.ha ), BA.ha), stat="identity", fill="cornflowerblue") +
  ggtitle("Basal Area") +
  labs(x="Plots", y= "Basal Area per hectare (cm^2/ha)")  +
  theme(legend.position = "top", axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  geom_point(data = BA_ha, aes(reorder(Site_Name, BA.ha ), BA.ha)) +
  geom_line(data = BA_ha, aes(Site_Name, BA.ha, group = 1))

BA_HA_tagged <- ggplot()+ 
  geom_bar(data = BA_ha_tagged, aes(reorder(Site_Name, BA_tagged.ha ), BA_tagged.ha), stat="identity", fill="cornflowerblue") +
  ggtitle("Basal Area for Tagged Trees") +
  labs(x="Plots", y= "Basal Area per hectare (cm^2/ha)")  +
  theme(legend.position = "top", axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  geom_point(data = BA_ha_tagged, aes(reorder(Site_Name, BA_tagged.ha ), BA_tagged.ha)) +
  geom_line(data = BA_ha_tagged, aes(Site_Name, BA_tagged.ha, group = 1))

BA_HA_sappling <- ggplot()+ 
  geom_bar(data = BA_ha_sappling, aes(reorder(Site_Name, BA_sappling.ha), BA_sappling.ha), stat="identity", fill="cornflowerblue") +
  ggtitle("Basal Area for Sapplings") +
  labs(x="Plots", y= "Basal Area per hectare (cm^2/ha)")  +
  theme(legend.position = "top", axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  geom_point(data = BA_ha_sappling, aes(reorder(Site_Name, BA_sappling.ha ), BA_sappling.ha)) +
  geom_line(data = BA_ha_sappling, aes(Site_Name, BA_sappling.ha, group = 1))

## Merge BA with index values
BA_elevation <- merge(BA_ha, Elevation[, c("Site_Name", "MEAN")], by= "Site_Name")
BA_elevation_radiation <- merge(BA_elevation, Radiation[, c("Site_Name", "MEAN")], by= "Site_Name")
BA_index <- merge(BA_elevation_radiation, TWI[, c("Site_Name", "MEAN")], by= "Site_Name")
# Tagged
BA_elevation_tagged <- merge(BA_ha_tagged, Elevation[, c("Site_Name", "MEAN")], by= "Site_Name")
BA_elevation_radiation_tagged <- merge(BA_elevation_tagged, Radiation[, c("Site_Name", "MEAN")], by= "Site_Name")
BA_index_tagged <- merge(BA_elevation_radiation_tagged, TWI[, c("Site_Name", "MEAN")], by= "Site_Name")
# Sappling 
BA_elevation_sappling <- merge(BA_ha_sappling, Elevation[, c("Site_Name", "MEAN")], by= "Site_Name")
BA_elevation_radiation_sappling <- merge(BA_elevation_sappling, Radiation[, c("Site_Name", "MEAN")], by= "Site_Name")
BA_index_sappling <- merge(BA_elevation_radiation_sappling, TWI[, c("Site_Name", "MEAN")], by= "Site_Name")


## Rename Colunms 
names(BA_index)[5]<- "Elevation_Mean"
names(BA_index)[6]<- "Radiation_Mean"
names(BA_index)[7]<- "TWI_Mean"
# Tagged
names(BA_index_tagged)[5]<- "Elevation_Mean"
names(BA_index_tagged)[6]<- "Radiation_Mean"
names(BA_index_tagged)[7]<- "TWI_Mean"
# Sappling
names(BA_index_sappling)[5]<- "Elevation_Mean"
names(BA_index_sappling)[6]<- "Radiation_Mean"
names(BA_index_sappling)[7]<- "TWI_Mean"


## Scater Plots 
BA_elevation<- ggplot() +
  geom_point(data = BA_index, aes(Elevation_Mean, BA.ha, color = Site_Name)) +
  labs(title= "Basal Area/ha VS Elevation", x="Mean Elevation", y= "Basal Area per Hectare", color= "Site Name")+
  theme(legend.position = "top")
BA_radiation<- ggplot() +
  geom_point(data = BA_index, aes(BA.ha, Radiation_Mean, color = Site_Name)) +
  labs(title= "Basal Area/ha VS Radiation", x="Basal Area per Hectare", y= "Mean Radiation", color= "Site Name") +
  theme(legend.position = "top")
BA_TWI <- ggplot() +
  geom_point(data = BA_index, aes(BA.ha, TWI_Mean, color = Site_Name)) +
  labs(title= "Basal Area/ha VS TWI", x="Basal Area per Hectare", y= "Mean TWI", color= "Site Name") +
  theme(legend.position = "top")
# Tagged 
BA_elevation_tagged <- ggplot() +
  geom_point(data = BA_index_tagged, aes(BA_tagged.ha, Elevation_Mean, color = Site_Name)) +
  labs(title= "Basal Area/ha VS Elevation for Tagged Trees", x="Basal Area per Hectare", y= "Mean Elevation", color= "Site Name") +
  theme(legend.position = "top")
BA_radiation_tagged <- ggplot() +
  geom_point(data = BA_index_tagged, aes(BA_tagged.ha, Radiation_Mean, color = Site_Name)) +
  labs(title= "Basal Area/ha VS Radiation for Tagged Trees", x="Basal Area per Hectare", y= "Mean Radiation", color= "Site Name") +
  theme(legend.position = "top")
BA_TWI_tagged <- ggplot() +
  geom_point(data = BA_index_tagged, aes(BA_tagged.ha, TWI_Mean, color = Site_Name)) +
  labs(title= "Basal Area/ha VS TWI for Tagged Trees", x="Basal Area per Hectare", y= "Mean TWI", color= "Site Name") +
  theme(legend.position = "top")
# Sappling 
BA_elevation_sappling <- ggplot() +
  geom_point(data = BA_index_sappling, aes(BA_sappling.ha, Elevation_Mean, color = Site_Name)) +
  labs(title= "Basal Area/ha VS Elevation for Sapplings", x="Basal Area per Hectare", y= "Mean Elevation", color= "Site Name") +
  theme(legend.position = "top")
BA_radiation_sappling <- ggplot() +
  geom_point(data = BA_index_sappling, aes(BA_sappling.ha, Radiation_Mean, color = Site_Name)) +
  labs(title= "Basal Area/ha VS Radiation for Sapplings", x="Basal Area per Hectare", y= "Mean Radiation", color= "Site Name") +
  theme(legend.position = "top")
BA_TWI_sappling <- ggplot() +
  geom_point(data = BA_index_sappling, aes(BA_sappling.ha, TWI_Mean, color = Site_Name)) +
  labs(title= "Basal Area/ha VS TWI for Sapplings", x="Basal Area per Hectare", y= "Mean TWI", color= "Site Name") +
  theme(legend.position = "top")
