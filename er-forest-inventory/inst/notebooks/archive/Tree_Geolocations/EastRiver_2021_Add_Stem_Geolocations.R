# Add stem geolocations to inventory datasheets
# Reads in tree stem locations from shapefile and appends longitude-latitude
# values in CRS EPSG:4326 to forest inventory datasheets.
# Author: Marshall Worsham
# Created: 02-06-23
# Revised: 02-15-23

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

# Configure drive oauth
drive_auth_configure(path=config$driveauth, api_key=readLines(config$drivekey))

#### Ingest data ####

# Import inventory data from Google Drive
invsheets <- drive_find(pattern=config$extdata$invpattern, type='spreadsheet')
invnames <- invsheets$name
invids <- invsheets$id
inventory <- lapply(invnames, load.inventory)

# Import tree geolocation shapefiles from Google Drive
trees <- load.trees(
  path=as_id(config$extdata$stemid),
  pattern=config$extdata$stempattern)

# Import raw tree geolocation GPS data from Google Drive
# We use this for associating a filename with geolocation records

# NOTE: Because this is a large request, it sometimes returns a cryptic https error.
# I think it has to do with request rate limits on a shared GCS credential that
# tidyverse/googledrive run in the background. Keep trying till it works
rgps <- load.rawgps(
  as_id(config$extdata$rawgpsid),
  pattern='Project',
  nmax=950)

#### Clean data ####

# Row bind inventory data
inv.df <- bind_rows(inventory)

# Row bind tree geolocation data
trees.df <- bind_rows(trees)

# Row bind raw gps data
rawgps <- bind_rows(rawgps)

# Reassign rownames sequentially
rownames(inv.df) <- NULL
rownames(trees.df) <- NULL
rownames(rawgps) <- NULL

# Create a concatenated site-tag column to make unique site-tree IDs
# (A few tag sequences occur in more than one plot)
inv.df$SiTag <- paste0(inv.df$Site_Name, '_', inv.df$Tag_Number)

trees.df$SiTag <- paste0(trees.df$Site_Name, '_', trees.df$Tag_Number)

# Review
head(inv.df)
head(trees.df)

# Filter to 2021 records only
data21 <- inv.df %>%
  filter((Census_End > '2021-01-01') & (Census_End < '2021-12-31'))

# Check
unique(data21$Census_Start)
unique(data21$Site_Name)
dim(data21)

# Find duplicate records and drop those not needed
dupes <- trees.df[duplicated(trees.df$SiTag),]
dupes <- trees.df[trees.df$Tag_Number %in% dupes$Tag_Number,]
#trees.df.drop[trees.df.drop$Tag_Number %in% dupes,]

#### Merge inventory records with geotag records ####

# Perform the merge on `SiTag`
invtrees <- merge(data21, trees.df, by = 'SiTag', all = T)

# Make sure each record is attached to the correct site
invtrees[is.na(invtrees$Site_Name.x),]$Site_Name.x <- invtrees[
  is.na(invtrees$Site_Name.x),]$Site_Name.y
invtrees[is.na(invtrees$Site_Name.x),]$Site_Name.x

# Make sure each record has a uniquely valid tag number
invtrees[is.na(invtrees$Tag_Number.x),]$Tag_Number.x <- invtrees[is.na(invtrees$Tag_Number.x),]$Tag_Number.y
invtrees[is.na(invtrees$Tag_Number.x),]

# Find new duplicates in merged dataset
invtrees[duplicated(invtrees$SiTag),] # There shouldn't be any
tagdupes <- invtrees[duplicated(invtrees$Tag_Number.x),] # There are many; it's OK

# Check how many observations are missing coordinates
length(which(is.na(invtrees$X))) # Should = 1236

# Inspect
# View(invtrees[c('Site_Name.x', 'Tag_Number.x', 'Geotagged', 'Geotag_Association_Ref', 'Geotag_Association_Dist', 'Geotag_Association_Dir', 'X')])

# Assign longitude and latitude values from shapefiles
invtrees$Longitude <- invtrees$X
invtrees$Latitude <- invtrees$Y

# Drop unnecessary columns
invtrees <- select(
  invtrees,
  c(
    'Site_Name.x':'Geotag_Date',
    'Latitude',
    'Longitude',
    'GPS_File_Name':'Entry_Personnel',
  )
)
colnames(invtrees)
colnames(invtrees)[c(1, 5, 8)] <- c('Site_Name', 'Tag_Number', 'Sp_Code')

#### Add GPS data file entry ####

# Assign tag numbers recorded in wrong field where possible
rawgps[is.na(rawgps$Other),]$Other <- rawgps[is.na(rawgps$Other),]$Other2
names(rawgps)[5] <- 'Tag_Number'

# Convert raw gps to dataframe.
rawgps <- as.data.frame(rawgps)

# Subset the raw gps dataframe to `Tag_Number` and `Datafile` columns
rgps.trim <- rgps[c('Tag_Number', 'Datafile')]

# Merge rgps with invtrees
invtrees.2 <- merge(invtrees, rgps.trim, by='Tag_Number', all=T)

# Drop the .cor from filename
invtrees.2$GPS_File_Name <- gsub('.cor', '', invtrees.2$Datafile)

# Drop observations where Site_Name is NA
# These are mostly plot corners, sensor locations, and extraneous points
invtrees.2 <- invtrees.2[!is.na(invtrees.2$Site_Name),]

# Inspect
View(invtrees.2[c('Site_Name', 'Tag_Number', 'Geotagged', 'Geotag_Association_Ref', 'Geotag_Association_Dist', 'Longitude', 'GPS_File_Name')])

# Select columns to preserve
invtrees.2 <- select(
  invtrees.2,
  c(
    'Site_Name':'Census_End',
    'Tag_Number',
    'Previous_Tag_Number':'Entry_Personnel'
  )
)

colnames(invtrees.2)
colnames(invtrees.2)[c(5,8)] <- c('Tag_Number', 'Sp_Code')
length(invtrees) == 44 #Check that there are 44 columns in resulting df

#### Split merged records ####

# Split into separate dfs by site name
sepdfs <- split(invtrees.2, with(invtrees.2, Site_Name), drop = F)

# Look at how total numbers match up
check.fin <- invtrees.2 %>%
  group_by(Site_Name) %>%
  summarize(fin.n=n())

check.fin.u <- invtrees.2 %>%
  group_by(Site_Name) %>%
  summarize(fin.u=n_distinct(Tag_Number))

check.org <- inv.df %>%
  group_by(Site_Name) %>%
  summarize(org.n=n())

check.org.u <- inv.df %>%
  group_by(Site_Name) %>%
  summarize(org.u=n_distinct(Tag_Number))

checks <- list(
  check.org,
  check.org.u,
  check.fin,
  check.fin.u)

check.n <- checks %>%
  reduce(
    inner_join,
    by='Site_Name'
  )

View(check.n)
View(inv.df)
View(invtrees.2)
View(invtrees.2[invtrees.2$Tag_Number %in% c(
  14,
  3169,
  322,
  349,
  352,
  362,
  6291,
  6377,
  6378,
  6380,
  6381,
  7241,
  7245
),])
#### Final checks on each site's df ####

# CC-CVN1 final check
cc_cvn1 <- sepdfs$`CC-CVN1`
cc_cvn1[duplicated(cc_cvn1$Tag_Number),] # 1 duplicate
cc_cvn1[cc_cvn1$Tag_Number %in% cc_cvn1[duplicated(cc_cvn1$Tag_Number),'Tag_Number'],]
cc_cvn1 <- cc_cvn1[!duplicated(cc_cvn1$Tag_Number),]
nrow(cc_cvn1[(is.na(cc_cvn1$Longitude)) | (is.na(cc_cvn1$Latitude)), ]) # Missing 28 coordinates
nrow(cc_cvn1[((is.na(cc_cvn1$Longitude)) | (is.na(cc_cvn1$Latitude))) & (cc_cvn1$Geotagged==T),]) # 3 missing that we supposedly tagged
row.names(cc_cvn1) <- NULL
# View(cc_cvn1)

## CC-CVN2 final check
cc_cvn2 <- sepdfs$`CC-CVN2`
cc_cvn2 <- cc_cvn2[!is.na(cc_cvn2$Tag_Number),]
cc_cvn2[duplicated(cc_cvn2$Tag_Number),] # Two duplicates (1786, 1987)
cc_cvn2 <- cc_cvn2[!duplicated(cc_cvn2$Tag_Number),]
cc_cvn2[cc_cvn2$Tag_Number %in% c(1786, 1987),]
nrow(cc_cvn2[(is.na(cc_cvn2$Longitude)) | (is.na(cc_cvn2$Latitude)), ]) # Missing 16 coordinates
nrow(cc_cvn2[((is.na(cc_cvn2$Longitude)) | (is.na(cc_cvn2$Latitude))) & (cc_cvn2$Geotagged==T),]) # 10 missing that we supposedly tagged
row.names(cc_cvn2) <- NULL

## CC-CVS1 final check
cc_cvs1 <- sepdfs$`CC-CVS1`
cc_cvs1[duplicated(cc_cvs1$Tag_Number),] # One duplicate (1196)
cc_cvs1[cc_cvs1$Tag_Number %in% c(1196),]
cc_cvs1 <- cc_cvs1[!duplicated(cc_cvs1$Tag_Number),]
nrow(cc_cvs1[(is.na(cc_cvs1$Longitude)) | (is.na(cc_cvs1$Latitude)), ]) # Missing 69 coordinates
nrow(cc_cvs1[((is.na(cc_cvs1$Longitude)) | (is.na(cc_cvs1$Latitude))) & (cc_cvs1$Geotagged==T),]) # 1 missing that we supposedly tagged
cc_cvs1 <- cc_cvs1[!is.na(cc_cvs1$Tag_Number),]
row.names(cc_cvs1) <- NULL
# View(cc_cvs1)

## CC-EMN1 final check
cc_emn1 <- sepdfs$`CC-EMN1`
cc_emn1[duplicated(cc_emn1$Tag_Number),] # No duplicates
nrow(cc_emn1[(is.na(cc_emn1$Longitude)) | (is.na(cc_emn1$Latitude)), ]) # Missing 61 coordinates
nrow(cc_emn1[((is.na(cc_emn1$Longitude)) | (is.na(cc_emn1$Latitude))) & (cc_emn1$Geotagged==T),]) # 1 missing that we supposedly tagged
cc_emn1 <- cc_emn1[!is.na(cc_emn1$Tag_Number),]
row.names(cc_emn1) <- NULL
# View(cc_emn1)

## SG-NES1 final check
sg_nes1 <- sepdfs$`SG-NES1`
# View(sg_nes1)
sg_nes1 <- sg_nes1[!is.na(sg_nes1$Tag_Number),]
sg_nes1[duplicated(sg_nes1$Tag_Number),] # 5 duplicates
sg_nes1[sg_nes1$Tag_Number %in% c(4456, 4589, 4628, 4774),]
sg_nes1 <- sg_nes1[!duplicated(sg_nes1$Tag_Number),]
sg_nes1[sg_nes1$Tag_Number %in% 4386,]$Latitude <- 38.93293972
sg_nes1[sg_nes1$Tag_Number %in% 4386,]$Longitude <- -106.98628092
nrow(sg_nes1[(is.na(sg_nes1$Longitude)) | (is.na(sg_nes1$Latitude)), ]) # Missing 219 coordinates
# View(sg_nes1[((is.na(sg_nes1$Longitude)) | (is.na(sg_nes1$Latitude))) & (sg_nes1$Geotagged==T),]) # 10 missing that we supposedly tagged
row.names(sg_nes1) <- NULL
# View(sg_nes1)

## SG-NES3 final check
sg_nes3 <- sepdfs$`SG-NES3`
sg_nes3 <- sg_nes3[!is.na(sg_nes3$Tag_Number),]
sg_nes3[duplicated(sg_nes3$Tag_Number),] # 2 duplicates
sg_nes3[sg_nes3$Tag_Number %in% c(8149, 8151),]
sg_nes3 <- sg_nes3[!duplicated(sg_nes3$Tag_Number),]
nrow(sg_nes3[(is.na(sg_nes3$Longitude)) | (is.na(sg_nes3$Latitude)), ]) # Missing 167 coordinates
nrow(sg_nes3[((is.na(sg_nes3$Longitude)) | (is.na(sg_nes3$Latitude))) & (sg_nes3$Geotagged==T),]) # 10 missing that we supposedly tagged
row.names(sg_nes3) <- NULL
# View(sg_nes3)

## SR-PVG1 final check
sr_pvg1 <- sepdfs$`SR-PVG1`
sr_pvg1 <- sr_pvg1[!is.na(sr_pvg1$Tag_Number),]
sr_pvg1[duplicated(sr_pvg1$Tag_Number),] # 0 duplicates
sr_pvg1 <- sr_pvg1[!duplicated(sr_pvg1$Tag_Number),]
nrow(sr_pvg1[(is.na(sr_pvg1$Longitude)) | (is.na(sr_pvg1$Latitude)), ]) # Missing 3 coordinates
nrow(sr_pvg1[((is.na(sr_pvg1$Longitude)) | (is.na(sr_pvg1$Latitude))) & (sr_pvg1$Geotagged==T),]) # none missing that we supposedly tagged
row.names(sr_pvg1) <- NULL
# View(sr_pvg1)
plot(sr_pvg1$Latitude, sr_pvg1$Longitude)

## XX-CAR1 final check
xx_car1 <- sepdfs$`XX-CAR1`
xx_car1 <- xx_car1[!is.na(xx_car1$Tag_Number),]
xx_car1[duplicated(xx_car1$Tag_Number),] # 0 duplicates
xx_car1 <- xx_car1[!duplicated(xx_car1$Tag_Number),]
nrow(xx_car1[(is.na(xx_car1$Longitude)) | (is.na(xx_car1$Latitude)), ]) # Missing 96 coordinates
nrow(xx_car1[((is.na(xx_car1$Longitude)) | (is.na(xx_car1$Latitude))) & (xx_car1$Geotagged==T),]) # 11 missing that we supposedly tagged
row.names(xx_car1) <- NULL
# View(xx_car1)
plot(xx_car1$Latitude, xx_car1$Longitude)

## XX-CAR3 final check
xx_car3 <- sepdfs$`XX-CAR3`
xx_car3 <- xx_car3[!is.na(xx_car3$Tag_Number),]
xx_car3[duplicated(xx_car3$Tag_Number),] # 5 duplicates
xx_car3 <- xx_car3[!duplicated(xx_car3$Tag_Number),]
nrow(xx_car3[(is.na(xx_car3$Longitude)) | (is.na(xx_car3$Latitude)), ]) # Missing 44 coordinates
nrow(xx_car3[((is.na(xx_car3$Longitude)) | (is.na(xx_car3$Latitude))) & (xx_car3$Geotagged==T),]) # 1 missing that we supposedly tagged
row.names(xx_car3) <- NULL
# View(xx_car3)
plot(xx_car3$Latitude, xx_car3$Longitude)

## XX-PLN1 final check
xx_pln1 <- sepdfs$`XX-PLN1`
xx_pln1[duplicated(xx_pln1$Tag_Number),] # 0 duplicates
# View(xx_pln1[xx_pln1$Tag_Number %in% xx_pln1[duplicated(xx_pln1$Tag_Number),'Tag_Number'],])
xx_pln1 <- xx_pln1[!duplicated(xx_pln1$Tag_Number),]
nrow(xx_pln1[(is.na(xx_pln1$Longitude)) | (is.na(xx_pln1$Latitude)), ]) # Missing 258 coordinates
nrow(xx_pln1[((is.na(xx_pln1$Longitude)) | (is.na(xx_pln1$Latitude))) & (xx_pln1$Geotagged==T),]) # 184 missing that we supposedly tagged
xx_pln1 <- xx_pln1[!is.na(xx_pln1$Tag_Number),]
row.names(xx_pln1) <- NULL
# View(xx_pln1)
plot(xx_pln1$Latitude, xx_pln1$Longitude)


## XX-PLN2 final check
xx_pln2 <- sepdfs$`XX-PLN2`
xx_pln2[duplicated(xx_pln2$Tag_Number),] # 0 duplicates
# View(xx_pln2[xx_pln2$Tag_Number %in% xx_pln2[duplicated(xx_pln2$Tag_Number),'Tag_Number'],])
xx_pln2 <- xx_pln2[!duplicated(xx_pln2$Tag_Number),]
nrow(xx_pln2[(is.na(xx_pln2$Longitude)) | (is.na(xx_pln2$Latitude)), ]) # Missing 24 coordinates
nrow(xx_pln2[((is.na(xx_pln2$Longitude)) | (is.na(xx_pln2$Latitude))) & (xx_pln2$Geotagged==T),]) # 5 missing that we supposedly tagged
xx_pln2 <- xx_pln2[!is.na(xx_pln2$Tag_Number),]
row.names(xx_pln2) <- NULL
# View(xx_pln2)
plot(xx_pln2$Latitude, xx_pln2$Longitude)
# View(sepdfs$`ER-APL1`)

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
  write.csv(
    d,
    file.path(
      wkdir,
      'Forest_Inventory_Dataset',
      'Output',
      'CleanInvLatLon',
      paste0('clean_',
             d[1,1],
             '.csv')))
  print(d[1,1])
}


ggplot(xx_pln2, aes(x=Longitude, y=Latitude, size=pi*(DBH_Avg_CM/2)^2)) +
  geom_point(color='darkgreen', fill='white', shape=21, alpha=0.3) +
  labs(size='Area')

