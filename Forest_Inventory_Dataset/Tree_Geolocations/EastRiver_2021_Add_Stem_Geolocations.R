# Load packages
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(googledrive)
library(googlesheets4)
library(sf)

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
    as_id(invids[j]),
    path = file.path(fidir, 'Inventory_Files_23-02-10', paste0(invnames[j], '.xlsx')),
    overwrite = T
  )
}

##########################################################################
# Import inventory data
##########################################################################

# Define files to import
datadir <- file.path(fidir, 'Inventory_Files_23-02-10')
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

# treesdir <- file.path(fidir, 'TreeCoords')
# trees <- list.files(treesdir, full.names=T)
# trees20 <- list.files(treesdir, full.names = T, pattern='Missed2020')
# trees21 <- setdiff(trees, trees20)
# 
# treesdf <- lapply(trees21, function(file){
#   data.frame(read.csv(file, 
#                       #colClasses = c("X" = 'double', "Y" = 'double'),
#                       row.names=1))
# })

trees <- list.files(stemdir, pattern='.shp', full.names = T)
trees.df <- lapply(trees, function(file){
  st_read(file)
})

trees.df <- lapply(seq_along(trees.df), function(x){
  names(trees.df[[x]])[c(2,3,16)] <-  c('Tag_Number', 'Sp_Code', 'Geotag_Association')
  trees.df[[x]] <- st_transform(trees.df[[x]], 'EPSG:4326')
  trees.df[[x]]$X <- st_coordinates(trees.df[[x]])[,1]
  trees.df[[x]]$Y <- st_coordinates(trees.df[[x]])[,2]
  trees.df[[x]]
})

trees.df <- lapply(trees.df, function(x) {
  x$Sp_Code <- as.character(x$Sp_Code)
  x$Geotag_Association <- as.numeric(x$Geotag_Association)
  x$Comment <- as.character(x$Comment)
  x
  })

alldata <- bind_rows(df)
alltrees <- bind_rows(trees.df)
rownames(alldata) <- NULL
rownames(alltrees) <- NULL
View(alldata)
View(alltrees)

alldata$SiTag <- paste0(alldata$Site_Name, '_', alldata$Tag_Number)
alltrees$SiTag <- paste0(alltrees$Site, '_', alltrees$Tag_Number)

# Filter to 2021 records only
data21 <- alldata %>%
  filter(Census_End > '2021-01-01') %>%
  filter(!Site_Name=='SG-NWS1')

data21 <- alldata %>%
  filter(Site_Name=='XX-CAR3')

unique(data21$Census_Start)
unique(data21$Site_Name)

dim(data21)

# Find records of trees tagged in prior years and missed in 2021
alltrees[!alltrees$Tag_Number %in% alldata$Tag_Number,]

# Find duplicate records and drop those not needed 
dupes <- alltrees[duplicated(alltrees$SiTag),]

#alltrees.drop <- alltrees[-c(50,1501,1475,1406),]
#alltrees.drop[alltrees.drop$Tag_Number %in% dupes,]

# Merge inventory records with geotag records
invtrees <- merge(data21, alltrees, by = 'SiTag', all = T)

# Make sure each record is attached to the correct site
invtrees[is.na(invtrees$Site_Name),]$Site_Name <- invtrees[is.na(invtrees$Site_Name),]$Site
invtrees[is.na(invtrees$Site_Name),]$Site_Name

# Find new duplicates in merged dataset
invtrees[duplicated(invtrees$SiTag),] # There are a few - best cleaned up by plot

length(which(is.na(invtrees$X)))
View(invtrees[c('Site_Name', 'Tag_Number.x', 'Geotagged', 'Geotag_Association_Ref', 'Geotag_Association_Dist', 'Geotag_Association_Dir', 'X')])

##########################################################################
# Add GPS data file entry
###########################################################################
rawgpsdir <- file.path(erdir, 'RMBL-East River Watershed Forest Data', 'RMBL 2019', 'Data', 'Inventory Plots', '02_Inventory_Plots_GPS_Data', '2021'
)

rawgpsfiles <- list.files(rawgpsdir, pattern='Project.shp$', recursive = T, full.names=T)
library(stringr)
rg <- rawgpsfiles[str_detect(rawgpsfiles, unique(data21$Site_Name))]
rawgps <- lapply(rawgpsfiles, st_read)
rgps <- bind_rows(rawgps)
rgps[is.na(rgps$Other),]$Other <- rgps[is.na(rgps$Other),]$Other2

names(rgps)[5] <- 'Tag_Number'
rgps <- as.data.frame(rgps)
rgps <- rgps[c('Tag_Number', 'Datafile')]

invtrees.2 <- merge(invtrees, rgps, by='Tag_Number', all=T)

invtrees.2$GPS_File_Name <- gsub('.cor', '', invtrees.2$Datafile)

View(invtrees.2[c('Site_Name', 'Tag_Number', 'Geotagged', 'Geotag_Association_Ref', 'Geotag_Association_Dist', 'GPS_File_Name')])

##########################################################################
# Drop columns and rearrange
##########################################################################
View(alltrees)
names(invtrees)

invtrees$Longitude <- invtrees$X
invtrees$Latitude <- invtrees$Y

invtrees <- select(
  invtrees, 
  c(
    'Site_Name':'Census_End',
    'Tag_Number.x',
    'Previous_Tag_Number':'Entry_Personnel'
  )
)
colnames(invtrees)[c(5,8)] <- c('Tag_Number', 'Sp_Code')

invtrees.2 <- select(
  invtrees.2, 
  c(
    'Site_Name':'Census_End',
    'Tag_Number',
    'Previous_Tag_Number':'Entry_Personnel'
  )
)

colnames(invtrees.2)[c(5,8)] <- c('Tag_Number', 'Sp_Code')
length(invtrees) == 44 #Check that there are 44 columns in resulting df

View(invtrees)

# Split merged record into separate dfs by site name
sepdfs <- split(invtrees.2, with(invtrees.2, Site_Name), drop = F)
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

# CC-CVN1 final check
cc_cvn1 <- sepdfs[[1]]
row.names(cc_cvn1) <- NULL
cc_cvn1[duplicated(cc_cvn1$Tag_Number),] # No duplicates
nrow(cc_cvn1[(is.na(cc_cvn1$Longitude)) | (is.na(cc_cvn1$Latitude)), ]) # Missing 28 coordinates
nrow(cc_cvn1[((is.na(cc_cvn1$Longitude)) | (is.na(cc_cvn1$Latitude))) & (cc_cvn1$Geotagged==T),]) # 3 missing that we supposedly tagged
View(cc_cvn1)

## CC-CVN2 final check
cc_cvn2 <- sepdfs[[2]]
cc_cvn2[duplicated(cc_cvn2$Tag_Number),] # Two duplicates (1786, 1987)
cc_cvn2 <- cc_cvn2[!duplicated(cc_cvn2$Tag_Number),]
cc_cvn2[cc_cvn2$Tag_Number %in% c(1786, 1987),]
nrow(cc_cvn2[(is.na(cc_cvn2$Longitude)) | (is.na(cc_cvn2$Latitude)), ]) # Missing 16 coordinates
nrow(cc_cvn2[((is.na(cc_cvn2$Longitude)) | (is.na(cc_cvn2$Latitude))) & (cc_cvn2$Geotagged==T),]) # 10 missing that we supposedly tagged
cc_cvn2 <- cc_cvn2[!is.na(cc_cvn2$Tag_Number),]
row.names(cc_cvn2) <- NULL

## CC-CVS1 final check
cc_cvs1 <- sepdfs[[3]]
cc_cvs1[duplicated(cc_cvs1$Tag_Number),] # One duplicate (1196)
cc_cvs1[cc_cvs1$Tag_Number %in% c(1196),]
cc_cvs1 <- cc_cvs1[!duplicated(cc_cvs1$Tag_Number),]
nrow(cc_cvs1[(is.na(cc_cvs1$Longitude)) | (is.na(cc_cvs1$Latitude)), ]) # Missing 69 coordinates
nrow(cc_cvs1[((is.na(cc_cvs1$Longitude)) | (is.na(cc_cvs1$Latitude))) & (cc_cvs1$Geotagged==T),]) # 1 missing that we supposedly tagged
cc_cvs1 <- cc_cvs1[!is.na(cc_cvs1$Tag_Number),]
row.names(cc_cvs1) <- NULL
View(cc_cvs1)

## CC-EMN1 final check
cc_emn1 <- sepdfs[[4]]
cc_emn1[duplicated(cc_emn1$Tag_Number),] # No duplicates
nrow(cc_emn1[(is.na(cc_emn1$Longitude)) | (is.na(cc_emn1$Latitude)), ]) # Missing 61 coordinates
nrow(cc_emn1[((is.na(cc_emn1$Longitude)) | (is.na(cc_emn1$Latitude))) & (cc_emn1$Geotagged==T),]) # 1 missing that we supposedly tagged
cc_emn1 <- cc_emn1[!is.na(cc_emn1$Tag_Number),]
row.names(cc_emn1) <- NULL
View(cc_emn1)

## SG-NES1 final check
sg_nes1 <- sepdfs[[12]]
sg_nes1[duplicated(sg_nes1$Tag_Number),] # 5 duplicates
sg_nes1[sg_nes1$Tag_Number %in% c(4456, 4589, 4628, 4774),]
sg_nes1 <- sg_nes1[!duplicated(sg_nes1$Tag_Number),]
sg_nes1[sg_nes1$Tag_Number %in% 4386,]$Latitude <- 38.93293972
sg_nes1[sg_nes1$Tag_Number %in% 4386,]$Longitude <- -106.98628092
nrow(sg_nes1[(is.na(sg_nes1$Longitude)) | (is.na(sg_nes1$Latitude)), ]) # Missing 219 coordinates
nrow(sg_nes1[((is.na(sg_nes1$Longitude)) | (is.na(sg_nes1$Latitude))) & (sg_nes1$Geotagged==T),]) # 10 missing that we supposedly tagged
sg_nes1 <- sg_nes1[!is.na(sg_nes1$Tag_Number),]
row.names(sg_nes1) <- NULL
View(sg_nes1)

## SG-NES3 final check
sg_nes3 <- sepdfs[[14]]
sg_nes3[duplicated(sg_nes3$Tag_Number),] # 2 duplicates
sg_nes3[sg_nes3$Tag_Number %in% c(8149, 8151),]
sg_nes3 <- sg_nes3[!duplicated(sg_nes3$Tag_Number),]
nrow(sg_nes3[(is.na(sg_nes3$Longitude)) | (is.na(sg_nes3$Latitude)), ]) # Missing 167 coordinates
nrow(sg_nes3[((is.na(sg_nes3$Longitude)) | (is.na(sg_nes3$Latitude))) & (sg_nes3$Geotagged==T),]) # 10 missing that we supposedly tagged
sg_nes3 <- sg_nes3[!is.na(sg_nes3$Tag_Number),]
row.names(sg_nes3) <- NULL
View(sg_nes3)

## SR-PVG1 final check
sr_pvg1 <- sepdfs[[16]]
sr_pvg1[duplicated(sr_pvg1$Tag_Number),] # 0 duplicates
sr_pvg1 <- sr_pvg1[!duplicated(sr_pvg1$Tag_Number),]
nrow(sr_pvg1[(is.na(sr_pvg1$Longitude)) | (is.na(sr_pvg1$Latitude)), ]) # Missing 3 coordinates
nrow(sr_pvg1[((is.na(sr_pvg1$Longitude)) | (is.na(sr_pvg1$Latitude))) & (sr_pvg1$Geotagged==T),]) # none missing that we supposedly tagged
sr_pvg1 <- sr_pvg1[!is.na(sr_pvg1$Tag_Number),]
row.names(sr_pvg1) <- NULL
View(sr_pvg1)
plot(sr_pvg1$Latitude, sr_pvg1$Longitude)


## XX-CAR1 final check
xx_car1 <- sepdfs[[18]]
xx_car1[duplicated(xx_car1$Tag_Number),] # 0 duplicates
xx_car1 <- xx_car1[!duplicated(xx_car1$Tag_Number),]
nrow(xx_car1[(is.na(xx_car1$Longitude)) | (is.na(xx_car1$Latitude)), ]) # Missing 96 coordinates
nrow(xx_car1[((is.na(xx_car1$Longitude)) | (is.na(xx_car1$Latitude))) & (xx_car1$Geotagged==T),]) # 11 missing that we supposedly tagged
xx_car1 <- xx_car1[!is.na(xx_car1$Tag_Number),]
row.names(xx_car1) <- NULL
View(xx_car1)
plot(xx_car1$Latitude, xx_car1$Longitude)


## XX-CAR3 final check
xx_car3 <- sepdfs[[19]]
xx_car3[duplicated(xx_car3$Tag_Number),] # 0 duplicates
xx_car3 <- xx_car3[!duplicated(xx_car3$Tag_Number),]
nrow(xx_car3[(is.na(xx_car3$Longitude)) | (is.na(xx_car3$Latitude)), ]) # Missing 44 coordinates
nrow(xx_car3[((is.na(xx_car3$Longitude)) | (is.na(xx_car3$Latitude))) & (xx_car3$Geotagged==T),]) # 1 missing that we supposedly tagged
xx_car3 <- xx_car3[!is.na(xx_car3$Tag_Number),]
row.names(xx_car3) <- NULL
View(xx_car3)
plot(xx_car3$Latitude, xx_car3$Longitude)

## XX-PLN1 final check
xx_pln1 <- sepdfs[[20]]
xx_pln1[duplicated(xx_pln1$Tag_Number),] # 0 duplicates
View(xx_pln1[xx_pln1$Tag_Number %in% xx_pln1[duplicated(xx_pln1$Tag_Number),'Tag_Number'],])
xx_pln1 <- xx_pln1[!duplicated(xx_pln1$Tag_Number),]
nrow(xx_pln1[(is.na(xx_pln1$Longitude)) | (is.na(xx_pln1$Latitude)), ]) # Missing 258 coordinates
nrow(xx_pln1[((is.na(xx_pln1$Longitude)) | (is.na(xx_pln1$Latitude))) & (xx_pln1$Geotagged==T),]) # 184 missing that we supposedly tagged
xx_pln1 <- xx_pln1[!is.na(xx_pln1$Tag_Number),]
row.names(xx_pln1) <- NULL
View(xx_pln1)
plot(xx_pln1$Latitude, xx_pln1$Longitude)


## XX-PLN2 final check
xx_pln2 <- sepdfs[[21]]
xx_pln2[duplicated(xx_pln2$Tag_Number),] # 0 duplicates
View(xx_pln2[xx_pln2$Tag_Number %in% xx_pln2[duplicated(xx_pln2$Tag_Number),'Tag_Number'],])
xx_pln2 <- xx_pln2[!duplicated(xx_pln2$Tag_Number),]
nrow(xx_pln2[(is.na(xx_pln2$Longitude)) | (is.na(xx_pln2$Latitude)), ]) # Missing 24 coordinates
nrow(xx_pln2[((is.na(xx_pln2$Longitude)) | (is.na(xx_pln2$Latitude))) & (xx_pln2$Geotagged==T),]) # 5 missing that we supposedly tagged
xx_pln2 <- xx_pln2[!is.na(xx_pln2$Tag_Number),]
row.names(xx_pln2) <- NULL
View(xx_pln2)
plot(xx_pln2$Latitude, xx_pln2$Longitude)


# Write out
names(sepdfs)
cleandfs <- list(cc_cvn1,
                 cc_cvn2,
                 cc_cvs1,
                 cc_emn1,
                 sg_nes1,
                 sg_nes3,
                 sr_pvg1,
                 xx_car1,
                 xx_car3,
                 xx_pln1,
                 xx_pln2)

for(d in cleandfs){
  write.csv(d, file.path(wkdir, 'Forest_Inventory_Dataset', 'Output', 'CleanInvLatLon', paste0('clean_',d[1,1],'.csv')))
  print(d[1,1])
}


ggplot(xx_pln2, aes(x=Longitude, y=Latitude, size=pi*(DBH_Avg_CM/2)^2)) + 
  geom_point(color='darkgreen', fill='white', shape=21, alpha=0.3) + 
  labs(size='Area')
  