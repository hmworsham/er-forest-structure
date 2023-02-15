# Load packages
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(googledrive)
library(googlesheets4)

# Set data directories
wkdir = file.path('/Volumes', 'GoogleDrive', 'My Drive', 'Research', 'RMBL', 'Working_Files')
fidir = file.path(wkdir, 'Forest_Inventory_Dataset', 'Source')
wsdir <- file.path(wkdir, 'Watershed_Spatial_Dataset', 'Source')
erdir <- file.path('/Volumes', 'GoogleDrive', 'My Drive', 'Research', 'RMBL')
stemdir <- file.path(erdir, 'RMBL-East River Watershed Forest Data', 'Data', 'Geospatial', 'Kueppers_EastRiver_Stem_Geolocations_WGS84UTM13N')

##########################################################################
# Download inventory data from Google Drive
##########################################################################
invsheets <- drive_find(pattern='inventory_data', type='spreadsheet')
invnames <- invsheets$name
invids <- invsheets$id

for(j in seq(length(invids))){
  drive_download(
    as_id(paste0('https://docs.google.com/spreadsheets/d/', invids[j])),
    path = file.path(fidir, 'Inventory_Files_23-01-04', paste0(invnames[j], '.xlsx')),
    overwrite = T
  )
}

drive_download(as_id('115wWuKrivjCsWh7J9CaiWs9hnUmmMwJe6liFyzDQFAw'), path = paste0(fidir, 'Inventory_Files_21-03-10/', i, '.xlsx'))

##########################################################################
# Import inventory data
##########################################################################

# Define files to import
datadir <- file.path(fidir, 'Inventory_Files_23-01-04')
files <- list.files(datadir, full.names = T)

# Define column types
coltypes = c('text', #site name
             'numeric', #census number
             'date', #censusstart
             'date', #census end
             'numeric', #tag number
             'numeric', #previous tag number
             'date', #tag date
             rep('text',6), #spp data
             rep('numeric',3), #heights
             'date', #height date
             'text', #height method
             rep('numeric', 3), #dbh data
             'date', #dbh date
             'text', #dbh method
             'numeric', #dbh hom
             'numeric', #cii
             'date', #cii date
             'text', #canopy position
             'date', # canopy position date
             'text', #beetles qual
             'numeric', #beetles index
             'date', # beetles date
             rep('text',3), #status, health, comments
             'logical', #geotagged
             'numeric', #geotag assoc ref
             'numeric', #geotag assoc dist
             'text', #geotag assoc dir
             'date', #geotag date
             rep('numeric',2), #lat lon
             'text', #gps filename
             'date', #entry date
             'text' #entry personnel
)

# Check length of column types to ensure match
length(coltypes)==44

# Import files with read_xlsx
df <- lapply(files, function(file) {
  data.frame(read_xlsx(file, 
                       sheet = 1,
                       col_types = coltypes,
                       na = 'NA'))
}
)

treesdir <- file.path(fidir, 'TreeCoords')
trees <- list.files(treesdir, full.names = T)

treesdf <- lapply(trees, function(file){
  data.frame(read.csv(file, 
                      colClasses = c("X" = 'double', "Y" = 'double'),
                      row.names=1))
})

alldata <- bind_rows(df)
alltrees <- bind_rows(treesdf)
rownames(alldata) <- NULL
rownames(alltrees) <- NULL
View(alldata)
View(alltrees)

colnames(alltrees)
#colnames(alltrees)[4] <- 'Tag_Number'

# Filter to 2021 records only
data20 <- alldata %>%
  filter(Census_End > '2021-01-01')
View(data20)

# Find records of trees tagged in prior years and missed in 2020
invtrees[!invtrees$Tag_Number %in% alldata$Tag_Number,]

# Find duplicate records and drop those not needed 
dupts <- alltrees[duplicated(alltrees$Tag_Number),]$Tag_Number
alltrees[alltrees$Tag_Number %in% dupts,]
alltrees.drop <- alltrees[-c(50,1501,1475,1406),]
alltrees.drop[alltrees.drop$Tag_Number %in% dupts,]

# Merge inventory records with geotag records
invtrees <- merge(data20, alltrees.drop, by = 'Tag_Number', all = T)

# Make sure each record is attached to the correct site
invtrees[is.na(invtrees$Site_Name),]$Site_Name <- invtrees[is.na(invtrees$Site_Name),]$Site

# Find new duplicates in merged dataset
invtrees[duplicated(invtrees$Tag_Number),] # There are a few - best cleaned up by plot


##########################################################################
# Drop columns and rearrange
##########################################################################
names(invtrees)
colorder <- c(2:5,1,6:39,46,45,42:44)
invtrees <- invtrees[, colorder]
colnames(invtrees)[40:41] <- c('Latitude', 'Longitude')
length(invtrees) == 44 #Check that there are 44 columns in resulting df

# Split merged record into separate dfs by site name
sepdfs <- split(invtrees, with(invtrees, Site_Name), drop = F)
View(sepdfs)
#list2env(sepdfs, envir = .GlobalEnv)

# Look at how total numbers match up
for(d in sepdfs){
  print(c(d$Site_Name[1], ' total:', nrow(d)))
  print(c(d$Site_Name[1],' unique:',length(unique(d$Tag_Number))))
}

for(d in df){
  print(c(d$Site_Name[1], ' total:', nrow(d)))
  print(c(d$Site_Name[1],' unique:',length(unique(d$Tag_Number))))
}

##########################################################################
# Final checks on each site's df
##########################################################################

names(sepdfs)

# CC_UC1 final check
cc_uc1 <- sepdfs[[1]]
row.names(cc_uc1) <- NULL
cc_uc1[duplicated(cc_uc1$Tag_Number),] # No duplicates
nrow(cc_uc1[(is.na(cc_uc1$Longitude)) | (is.na(cc_uc1$Latitude)), ]) # Missing 39 coordinates
View(cc_uc1)

# CC-UC2 final check
cc_uc2 <- sepdfs[[2]]
row.names(cc_uc2) <- NULL
cc_uc2[duplicated(cc_uc2$Tag_Number),] # No duplicates
nrow(cc_uc2[(is.na(cc_uc2$Longitude)) | (is.na(cc_uc2$Latitude)), ]) # Missing 1 coordinate
View(cc_uc2)

# ER-APL1 final check
er_apl1 <- sepdfs[[3]]
row.names(er_apl1) <- NULL
er_apl1[duplicated(er_apl1$Tag_Number),] # 1 duplicate
er_apl1[er_apl1$Tag_Number == 1143,]
er_apl1 <- er_apl1[-c(143),]
nrow(er_apl1[(is.na(er_apl1$Longitude)) | (is.na(er_apl1$Latitude)), ]) # Missing 5 coordinates
row.names(er_apl1) <- NULL
View(er_apl1)

# ER-APU1 final check
er_apu1 <- sepdfs[[4]]
row.names(er_apu1) <- NULL
er_apu1[duplicated(er_apu1$Tag_Number),] # 0 duplicates
nrow(er_apu1[(is.na(er_apu1$Longitude)) | (is.na(er_apu1$Latitude)), ]) # Missing 5 coordinates
# One tree was mislabeled in the gps data; the steps below correct that and associate coordinates of (incorrect) 4757 with (correct) 7457
er_apu1[er_apu1$Tag_Number == 7457,]$Longitude <- er_apu1[er_apu1$Tag_Number == 4757,]$Longitude
er_apu1[er_apu1$Tag_Number == 7457,]$Latitude <- er_apu1[er_apu1$Tag_Number == 4757,]$Latitude
er_apu1 <- er_apu1[-c(1),]
nrow(er_apu1[(is.na(er_apu1$Longitude)) | (is.na(er_apu1$Latitude)), ]) # Now missing 4 coordinates
View(er_apu1)

# ER-BME1 final check
er_bme1 <- sepdfs[[5]]
row.names(er_bme1) <- NULL
er_bme1[duplicated(er_bme1$Tag_Number),] # 2 duplicates
er_bme1[er_bme1$Tag_Number %in% c(1335,1337),]
er_bme1 <- er_bme1[-c(79,82),]
er_bme1[duplicated(er_bme1$Tag_Number),] # Now 0 duplicates
nrow(er_bme1[(is.na(er_bme1$Longitude)) | (is.na(er_bme1$Latitude)), ]) # Missing 0 coordinates
row.names(er_bme1) <- NULL
View(er_bme1)

# ER-BME2 final check
er_bme2 <- sepdfs[[6]]
row.names(er_bme2) <- NULL
er_bme2[duplicated(er_bme2$Tag_Number),] # 0 duplicates
nrow(er_bme2[(is.na(er_bme2$Longitude)) | (is.na(er_bme2$Latitude)), ]) # Missing 1 coordinate
# One tree was mislabeled in the gps data; the steps below correct that and associate coordinates of (incorrect) 2123 with (correct) 4123
er_bme2[er_bme2$Tag_Number == 4123,]$Latitude <- er_bme2[er_bme2$Tag_Number == 2123,]$Latitude 
er_bme2[er_bme2$Tag_Number == 4123,]$Longitude <- er_bme2[er_bme2$Tag_Number == 2123,]$Longitude
er_bme2 <- er_bme2[-c(1),]
nrow(er_bme2[(is.na(er_bme2$Longitude)) | (is.na(er_bme2$Latitude)), ]) # Now missing 0 coordinates
row.names(er_bme2) <- NULL
View(er_bme2)

# ER-GT1 final check
er_gt1 <- sepdfs[[7]]
row.names(er_gt1) <- NULL
er_gt1[duplicated(er_gt1$Tag_Number),] # 3 duplicates
er_gt1[er_gt1$Tag_Number %in% c(331,342,364),]
er_gt1 <- er_gt1[-c(84,96,118),] # drop incorrect duplicates
nrow(er_gt1[(is.na(er_gt1$Longitude)) | (is.na(er_gt1$Latitude)), ]) # Missing 14 coordinates
row.names(er_gt1) <- NULL
nrow(er_gt1)
View(er_gt1)

# SG-NES2
sg_nes2 <- sepdfs[[8]]
row.names(sg_nes2) <- NULL
sg_nes2[duplicated(sg_nes2$Tag_Number),] # 0 duplicates
nrow(sg_nes2[(is.na(sg_nes2$Longitude)) | (is.na(sg_nes2$Latitude)), ]) # Missing 10 coordinates
sg_nes2[is.na(sg_nes2$Longitude),]
# One tree was mislabeled in the gps data; the steps below correct that and associate coordinates of (incorrect) 3999 with (correct) 3399
sg_nes2[sg_nes2$Tag_Number == 3399,]$Longitude <- sg_nes2[sg_nes2$Tag_Number == 3999,]$Longitude
sg_nes2[sg_nes2$Tag_Number == 3399,]$Latitude <- sg_nes2[sg_nes2$Tag_Number == 3999,]$Latitude
nrow(sg_nes2[(is.na(sg_nes2$Longitude)) | (is.na(sg_nes2$Latitude)), ]) # Now missing 9 coordinates
sg_nes2 <- sg_nes2[-c(334),]
row.names(sg_nes2) <- NULL
nrow(sg_nes2)
View(sg_nes2)

# SG-SWR1
sg_swr1 <- sepdfs[[9]]
row.names(sg_swr1) <- NULL
sg_swr1[duplicated(sg_swr1$Tag_Number),] # 0 duplicates
nrow(sg_swr1[(is.na(sg_swr1$Longitude)) | (is.na(sg_swr1$Latitude)), ]) # Missing 8 coordinates
sg_swr1[is.na(sg_swr1$Longitude),]
# One tree was mislabeled in the gps data; the steps below correct that and associate coordinates of (incorrect) 3954 with (correct) 4954
sg_swr1[sg_swr1$Tag_Number == 4954,]$Longitude <- sg_swr1[sg_swr1$Tag_Number == 3954,]$Longitude
sg_swr1[sg_swr1$Tag_Number == 4954,]$Latitude <- sg_swr1[sg_swr1$Tag_Number == 3954,]$Latitude
nrow(sg_swr1[(is.na(sg_swr1$Longitude)) | (is.na(sg_swr1$Latitude)), ]) # Now missing 7 coordinates
sg_swr1 <- sg_swr1[-c(1),] # Drop 3954
row.names(sg_swr1) <- NULL
sg_swr1[is.na(sg_swr1$Longitude),]

# Update geotagged column
sg_swr1[!is.na(sg_swr1$GPS_File_Name),]$Geotagged <- TRUE
sg_swr1[is.na(sg_swr1$GPS_File_Name),]$Geotagged <- FALSE
nrow(sg_swr1)
View(sg_swr1)

# WG-WGM1
wg_wgm1 <- sepdfs[[10]]
row.names(wg_wgm1) <- NULL
wg_wgm1[duplicated(wg_wgm1$Tag_Number),] # 0 duplicates
nrow(wg_wgm1[(is.na(wg_wgm1$Longitude)) | (is.na(wg_wgm1$Latitude)), ]) # Missing 4 coordinates
# Two trees were mislabeled in the gps data; the steps below correct that and associate coordinates of (incorrect) 9897,6991 with (correct) 6897, 6891
wg_wgm1[wg_wgm1$Tag_Number == 6897,]$Longitude <- wg_wgm1[wg_wgm1$Tag_Number == 9897,]$Longitude
wg_wgm1[wg_wgm1$Tag_Number == 6897,]$Latitude <- wg_wgm1[wg_wgm1$Tag_Number == 9897,]$Latitude
wg_wgm1[wg_wgm1$Tag_Number == 6891,]$Longitude <- wg_wgm1[wg_wgm1$Tag_Number == 6991,]$Longitude
wg_wgm1[wg_wgm1$Tag_Number == 6891,]$Latitude <- wg_wgm1[wg_wgm1$Tag_Number == 6991,]$Latitude
wg_wgm1 <- wg_wgm1[-c(228,229),]

nrow(wg_wgm1[(is.na(wg_wgm1$Longitude)) | (is.na(wg_wgm1$Latitude)), ]) # Now missing 2 coordinates
row.names(wg_wgm1) <- NULL
nrow(wg_wgm1)
View(wg_wgm1)

# XX-PLN1
xx_pln1 <- sepdfs[[11]]
row.names(xx_pln1) <- NULL
xx_pln1[duplicated(xx_pln1$Tag_Number),] # 0 duplicates
nrow(xx_pln1[(is.na(xx_pln1$Longitude)) | (is.na(xx_pln1$Latitude)), ]) # Missing 1 coordinate
row.names(xx_pln1) <- NULL
nrow(xx_pln1)
View(xx_pln1)

# Write out
names(sepdfs)
cleandfs <- list(cc_uc1,
                 cc_uc2,
                 er_apl1,
                 er_apu1,
                 er_bme1,
                 er_bme2,
                 er_gt1,
                 sg_nes2,
                 sg_swr1,
                 wg_wgm1,
                 xx_pln1)

for(d in cleandfs){
  write.csv(d, paste0(getwd(), '/clean_',d[1,1],'.csv'))
  print(d[1,1])
}
