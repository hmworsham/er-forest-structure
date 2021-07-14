##Script to inventory Kueppers et al. 2020 RMBL/East River data

##Author: Marshall Worsham
##Updated: 06-17-2020

## Install and load libraries
pkgs <- c('dplyr',
          'tidyverse',
          'ggplot2',
          'XML',
          'RCurl',
          'googledrive') # Name the packages you want to use here
load.pkgs <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} # Function to install new packages if they're not already installed
load.pkgs(pkgs) # Runs the function on the list of packages defined in pkgs

# Set working directory to a local directory
setwd('~/Desktop/RMBL/Projects/Watershed_Spatial_Dataset/')

# Download data inventory spreadsheet from Google Drive file using persistent google ID
drive_download(as_id('1kbW09dYBoY3uhDBeWVtEhvJXawmwIOODb5ScGkL32Pg'), type = 'csv', overwrite = T) # downloads as .csv file

# Ingest the csv and clean up a bit
inv.sheet <- paste0(getwd(), '/RMBL Data Inventory.csv')
inventory <- read.csv(inv.sheet, header = T)[-1,]


# Create the xml tree and add the top-level node, which we'll call "RMBL_East_River_Data_Inventory"
inv.xml <- xmlTree('RMBL_EastRiver_Data_Inventory')
inv.xml$addNode('Datasets', close = F)

# Function to add nodes and populate with entries for each dataset listed in the inventory
for(i in 1:nrow(inventory)) {
  
  # Define node names and contents based on inventory table entries
  desc = inventory[i,'Description']
  beg = names(inventory)[c(1:11)] # The "beginning" of the dataset, the first few cols
  begtxt = inventory[i, c(1:11)]
  loc = names(inventory)[12] # We make a separate section for geospatial info
  loctxt = inventory[i, 'Location']
  cov = names(inventory)[13]
  covtxt = inventory[i, 'Spatial_Coverage']
  cord = names(inventory)[c(14:17)] # We make another section for xy coordinates
  cordtxt = inventory[i, c(14:17)]
  res = names(inventory)[18]
  restxt = inventory[i, 'Spatial_Res']
  err = names(inventory)[19]
  errtxt = inventory[i, 'Spatial_Error']
  rest = names(inventory)[c(20:32)] # The "rest" of the dataset
  resttxt = inventory[i, c(20:32)]
  vers = names(inventory)[c(33:35)] # We make a separate section for inventory version info
  verstxt = inventory[i, c(33:35)]
  
  # Add nodes by calling the variables
  inv.xml$addNode('Dataset', desc, close = F)
    #inv.xml$addNode(names(inventory)[1], inventory[i,1], close = T)
    for(j in 1:length(beg)){
      inv.xml$addNode(beg[j], begtxt[,j], close = T)
    }
    inv.xml$addNode('Geospatial_Information', close = F)
      inv.xml$addNode(loc, loctxt, close = T)
      inv.xml$addNode(cov, covtxt, close = T)
      inv.xml$addNode('Extent', close = F)
        for(co in 1:length(cord)){
          inv.xml$addNode(cord[co], cordtxt[,co], close = T)
        }
      inv.xml$closeNode()
      inv.xml$addNode(res, restxt, close = T)
      inv.xml$addNode(err, errtxt, close = T)
    inv.xml$closeNode()
    for(k in 1:length(rest)){
      inv.xml$addNode(rest[k], resttxt[,k], close = T)
    }
    inv.xml$addNode('Inventory_Versioning_Info', close = F)
    for(l in 1:length(vers)){
      inv.xml$addNode(vers[l], verstxt[,l], close = T)
    }
    inv.xml$closeNode()
  inv.xml$closeNode()  
}

# Print the XML tree to the console
cat(xml(inv.xml))

# Save tree as XML file to working directory
saveXML(inv.xml, 'RMBL_Data_Inventory.xml')

# Pull a particular node set
inv.intxml <- xmlInternalTreeParse(xml(inv.xml))
getNodeSet(xx, '//Acquisition_Year')



           