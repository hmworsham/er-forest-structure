# Cleaning existing Kueppers inventory data (up to 2021)
# Marshall Worsham
# Updated 12-30-22

# Load packages
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(googledrive)

# Set data directories
erdir <- file.path('/Volumes', 'GoogleDrive', 'My Drive', 'Research', 'RMBL')
dendrodir <- file.path(erdir, 'RMBL-East River Watershed Forest Data', 'Data', 'Inventory Plots')
workdir <- file.path(erdir, 'Working_Files', 'Forest_Inventory_Dataset', 'Source')

##########################################################################
# Download inventory data from Google Drive
##########################################################################
invsheets <- drive_find(pattern='inventory_data', type='spreadsheet')
invnames <- invsheets$name
invids <- invsheets$id

for(j in seq(length(invids))){
  drive_download(
    as_id(paste0('https://docs.google.com/spreadsheets/d/', invids[j])),
    path = file.path(workdir, 'Inventory_Files_22-12-02', paste0(invnames[j], '.xlsx')),
    overwrite = T
  )
}

##########################################################################
# Import inventory data
##########################################################################

# Define files to import
datadir <- file.path(workdir, 'Inventory_Files_22-12-02')
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

# Bind the dataframes into one and filter rows with all NA values
alldata <- bind_rows(df)
invdata <- alldata[rowSums(is.na(alldata)) != length(alldata), ]

# Count entries by site name
View(invdata %>%
  group_by(Site_Name) %>%
  count())

##########################################################################
# Cleaning and deduplicating
##########################################################################

# Find records missing tag numbers
nt <- invdata[which(is.na(invdata$Tag_Number)),]

# Find date range of missing tag numbers
unique(nt$Census_Start)
# All seem to be in 2018 and 2019, except for one (associated with SG-NWS1, which had Census_Start entered incorrectly)

# Find entries where the tag number is missing but shouldn't be (ie, DBH>10 cm but no associated tag number)
nt.gt10 <- nt[which(nt$DBH_1_CM>10),]
nt.gt10 %>% 
  group_by(Site_Name, Census_Start) %>%
  summarise(n())
# It's only a few, and they're all from 2018/2019 so we can assume subsequent censuses captured them, or that it's otherwise OK to drop these observations

# Remove all NA tag numbers and all "NT" tag numbers (which are coded NA on read)
inv <- invdata %>%
  filter(!is.na(Tag_Number)) 

# Check results
assertthat::are_equal(length(which(is.na(invdata$Tag_Number))), nrow(invdata)-nrow(inv))
length(which(is.na(inv$Tag_Number))) # Check number of NA tag numbers (should = 0)

# Filter records in sites outside AOP domain
# inv <- inv %>%
#   filter(!Site_Name %in% c('XX-CAR1', 'XX-CAR2', 'XX-PLN2', 'XX-PLN1', 'SG-NWS1'))

# Check site names
unique(inv$Site_Name)
length(unique(inv$Site_Name))

# Check dimensions of inv
dim(inv)

# Add column to inv that merges site ID and tag number
inv$SiTag <- paste(inv$Site_Name, inv$Tag_Number, sep='_')

####################################
# DEAL WITH QUADRUPLICATE ENTRIES
####################################

# Subset where the same tree tag has 4 entries
tag4 <- inv %>% 
  group_by(SiTag) %>%
  count() %>%
  filter(n > 3) %>%
  ungroup()

quadrupes <- inv[(inv$SiTag %in% tag4$SiTag),]
View(quadrupes)
# There should be no instances of 4 or more observations of any tree

##################
# TRIPLICATES
##################

# Subset where the same tree tag has 3 entries
tag3 <- inv %>% 
  group_by(SiTag) %>%
  count() %>%
  filter(n == 3) %>%
  ungroup()

trips <- inv[(inv$SiTag %in% tag3$SiTag),]
nrow(tag3)
nrow(trips)
View(trips)

# Find where tag numbers with three entries have multiple height entries
trips_ht <- trips %>%
  group_by(SiTag) %>%
  filter(!is.na(Height_1_M)) %>%
  count() %>%
  filter(n > 1)

# Find where tag numbers with three entries have multiple DBH entries
trips_dbh <- trips %>%
  group_by(SiTag) %>%
  filter(!is.na(DBH_1_CM)) %>%
  count() %>%
  filter(n>1)

# Find where tag numbers with three entries have multiple canopy position entries
trips %>%
  group_by(SiTag) %>%
  filter(!is.na(CII)) %>%
  count() %>%
  filter(n > 1)

# Find where tag numbers with three entries have multiple latitude entries
trips %>%
  group_by(SiTag) %>%
  filter(!is.na(Latitude)) %>%
  count() %>%
  filter(n > 1)

# Remove erroneous duplicate height or dbh entries
View(inv[inv$SiTag %in% trips_ht$SiTag,])
View(inv[inv$SiTag %in% trips_dbh$SiTag,])

inv <- inv[-c(which((inv$Tag_Number==364)&(inv$Height_1_M==24.9)),
       which((inv$Tag_Number==1139)&(inv$DBH_1_CM==22.6))
       ),]

tag3 <- inv %>% 
  group_by(SiTag) %>%
  count() %>%
  filter(n == 3) %>%
  ungroup()

trips <- inv[(inv$SiTag %in% tag3$SiTag),]
nrow(tag3)
nrow(trips)

k <- function(x){
  ifelse(isFALSE(first(x[!is.na(x)])), NA, first(x[!is.na(x)]))
}

# Merge triplicates on most recent observation to produce unique entries per tag number
inv_detrip <- trips %>%
  group_by(SiTag) %>%
  mutate(Site_Name = k(Site_Name),
         Census_Number = k(Census_Number),
         Census_Start = as.Date.POSIXct(k(Census_Start)), 
         Census_End = as.Date.POSIXct(k(Census_End)),
         Tag_Date = as.Date.POSIXct(k(Tag_Date)),
         Sp_Code = k(Sp_Code),
         Family = k(Family),
         Genus = k(Genus), 
         Species = k(Species),
         Subspecies = k(Subspecies),
         Authority = k(Authority), 
         Height_1_M = k(Height_1_M),
         Height_2_M = k(Height_2_M),
         Height_Avg_M = k(Height_Avg_M),
         Height_Method = k(Height_Method),
         Height_Date = as.Date.POSIXct(k(Height_Date)),
         DBH_1_CM = k(DBH_1_CM),
         DBH_2_CM = k(DBH_2_CM),
         DBH_Avg_CM = k(DBH_Avg_CM),
         DBH_Date = as.Date.POSIXct(k(DBH_Date)),
         DBH_Method = k(DBH_Method),
         DBH_HOM = k(DBH_HOM),
         CII = k(CII),
         CII_Date = as.Date.POSIXct(k(CII_Date)),
         Canopy_Position = k(Canopy_Position),
         Canopy_Position_Date = as.Date.POSIXct(k(Canopy_Position_Date)),
         Beetles_Qual = k(Beetles_Qual),
         Beetles_Index = k(Beetles_Index),
         Beetles_Date = as.Date.POSIXct(k(Beetles_Date)),
         Comments = k(Comments),
         Health = k(Health)) %>%
  arrange(desc(Entry_Date), .by_group = T) %>%
  mutate(Entry_Date = as.Date.POSIXct(k(Entry_Date)),
         Entry_Personnel = k(Entry_Personnel)) %>%
  slice(1) %>%
  ungroup()

#Check
detrip_reps <- inv_detrip %>% 
  group_by(SiTag) %>%
  count() %>%
  filter(n > 1) %>%
  ungroup()

dim(detrip_reps)

##################
#DUPLICATES
##################

# Subset where the same tree tag has 2 entries
tag2 <- inv %>% 
  group_by(SiTag) %>%
  count() %>%
  filter(n == 2) %>%
  ungroup()
nrow(tag2)
dupes <- inv[(inv$SiTag %in% tag2$SiTag),]
nrow(dupes)
View(dupes)

# Find where tag numbers with two entries have two height observations
dupes2ht <- dupes %>%
  group_by(SiTag) %>%
  filter(!is.na(Height_1_M)) %>%
  count() %>%
  filter(n > 1)
dupes2ht <- inv[inv$SiTag %in% dupes2ht$SiTag,]
View(dupes2ht)

# Find where tag numbers with two entries have two DBH observations
dupes2dbh <- dupes %>%
  group_by(SiTag) %>%
  filter(!is.na(DBH_1_CM)) %>%
  count() %>%
  filter(n>1)
dupes2dbh <- inv[inv$SiTag %in% dupes2dbh$SiTag,]
View(dupes2dbh)

# Find where tag numbers with two entries have multiple canopy position entries
dupes %>%
  group_by(SiTag) %>%
  filter(!is.na(Canopy_Position)) %>%
  count() %>%
  filter(n > 1)

# Find where tag numbers with two entries have two latitude entries
dupeslat <- dupes %>%
  group_by(SiTag) %>%
  filter(!is.na(Latitude)) %>%
  count() %>%
  filter(n > 1)
dupeslat <- inv[inv$SiTag %in% dupeslat$SiTag,]
nrow(dupeslat)
View(dupeslat)

# Merge duplicates on various criteria to produce unique entries per tag number
inv_dedupe <- dupes %>%
  group_by(SiTag) %>%
  arrange(desc(Height_Date), by.group = T) %>%
  mutate(Site_Name = k(Site_Name),
         Census_Number = k(Census_Number),
         Census_Start = as.Date.POSIXct(k(Census_Start)), 
         Census_End = as.Date.POSIXct(k(Census_End)),
         Tag_Date = as.Date.POSIXct(k(Tag_Date)),
         Sp_Code = k(Sp_Code),
         Family = k(Family),
         Genus = k(Genus), 
         Species = k(Species),
         Subspecies = k(Subspecies),
         Authority = k(Authority), 
         Height_1_M = k(Height_1_M),
         Height_2_M = k(Height_2_M),
         Height_Avg_M = k(Height_Avg_M),
         Height_Method = k(Height_Method),
         Height_Date = as.Date.POSIXct(k(Height_Date))) %>%
  arrange(desc(DBH_Date), by.group = T) %>%
  mutate(DBH_1_CM = k(DBH_1_CM),
         DBH_2_CM = k(DBH_2_CM),
         DBH_Avg_CM = k(DBH_Avg_CM),
         DBH_Date = as.Date.POSIXct(k(DBH_Date)),
         DBH_Method = k(DBH_Method),
         DBH_HOM = k(DBH_HOM)) %>%
  arrange(desc(CII_Date), by.group = T) %>%
  mutate(CII = k(CII),
         CII_Date = as.Date.POSIXct(k(CII_Date)),
         Canopy_Position = k(Canopy_Position),
         Canopy_Position_Date = as.Date.POSIXct(k(Canopy_Position_Date))) %>%
  arrange(Beetles_Date, by.group = T) %>%
  mutate(Beetles_Qual = k(Beetles_Qual),
         Beetles_Index = k(Beetles_Index),
         Beetles_Date = as.Date.POSIXct(k(Beetles_Date)),
         Comments = k(Comments),
         Health = k(Health)) %>%
  arrange(desc(Entry_Date), by.group = T) %>%
  mutate(Entry_Date = as.Date.POSIXct(k(Entry_Date)),
         Entry_Personnel = k(Entry_Personnel)) %>%
  slice(1) %>%
  ungroup()
View(inv_dedupe)

#Check
dedupe_reps <- inv_dedupe %>% 
  group_by(SiTag) %>%
  count() %>%
  filter(n > 1) %>%
  ungroup()
dim(dedupe_reps)
dedupe_reps

################
# SINGLETONS
################

# Find singleton entries
singletons <- inv %>%
  group_by(SiTag) %>%
  count() %>%
  filter(n == 1) %>%
  ungroup()
singletons <- inv[(inv$SiTag %in% singletons$SiTag),]


##########################################################################
# Create a merged set of unique entries per tag number
##########################################################################

full_unq_inv <- rbind(singletons, inv_dedupe, inv_detrip)
row.names(full_unq_inv) <- NULL
dim(full_unq_inv)
dim(full_unq_inv) == c(8312, 45)
# The final dataset should have 4207 entries, all unique, including the XX-CAR and XX-PLN sites

##########################################################################
# Checks
##########################################################################

# Check for repeat entries
finalrepeats <- full_unq_inv %>% 
  group_by(SiTag) %>%
  count() %>%
  filter(n > 2) %>%
  ungroup()

dim(finalrepeats) == c(0,2)

all_reps <- full_unq_inv %>% 
  group_by(SiTag) %>%
  count() %>%
  filter(n > 1) %>%
  ungroup()
all_reps
dim(dedupe_reps)

# Find species ID conflicts
sppcon <- inv %>% 
  group_by(SiTag, Sp_Code) %>%
  count() %>%
  ungroup %>%
  group_by(SiTag) %>%
  count() %>%
  filter(n > 1)
sppcon

# Did we miss any tags in 2020 that had been measured in prior years?
cenpre2020 <- inv %>% 
  filter(Census_End < '2020-01-01')

cen2020 <- inv %>%
  filter(Census_End > '2020-01-01')

misses20 <- cenpre2020[!cenpre2020$Tag_Number %in% cen2020$Tag_Number, which(colnames(cenpre2020) %in% c('Site_Name','Tag_Number'))] %>%
  filter(!Site_Name %in% c('XX-CAR1', 'XX-CAR2', 'XX-PLN2', 'XX-PLN1'))

misses20 <- full_unq_inv[full_unq_inv$Tag_Number %in% misses20$Tag_Number,]
misses20
# Yes, we missed 20.
write.csv(misses20, 'EastRiver_2020_Missed_Measurements.csv')

##########################################################################
# Export full clean dataset
##########################################################################
write.csv(full_unq_inv, '~/Desktop/EastRiver_AllPlots_Inventory_Data_2018-2021_Collated.csv')
dim(full_unq_inv)
