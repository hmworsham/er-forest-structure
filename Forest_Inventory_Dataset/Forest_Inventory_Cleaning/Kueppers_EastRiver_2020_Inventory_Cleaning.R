# Cleaning existing Kueppers inventory data (up to 2020)
# Marshall Worsham
# Update 03-10-2021

# Load packages
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(googledrive)

# Set data directories
fidir <- paste0(getwd(), '/Desktop/RMBL/Projects/Forest_Inventory_Dataset/Source/')
wsdir <- paste0(getwd(), '/Desktop/RMBL/Projects/Watershed_Spatial_Dataset/Source/')

##########################################################################
# Download inventory data from Google Drive
##########################################################################

invsheets <- drive_ls(path = as_id('1twTtetxdIAwN8wCMXy8oagGjHVa56ZU8'), pattern = 'inventory_data', type = 'spreadsheet', recursive = T)
invnames <- invsheets$name
invids <- invsheets$id

for(j in invids){
  drive_download(
    as_id(paste0('https://docs.google.com/spreadsheets/d/', j)),
    path = paste0(fidir, 'Inventory_Files_21-03-14/', j, '.xlsx'),
    overwrite = T
  )
}

##########################################################################
# Import inventory data
##########################################################################

# Define files to import
datadir <- paste0(fidir, 'Inventory_Files_21-03-14')
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
                       na = 'NA'
            ))})

# Bind the dataframes into one and filter rows with all NA values
alldata <- bind_rows(df)
invdata <- alldata %>%
  filter_all(any_vars(!is.na(.)))

# Count entries by site name
invdata %>%
  group_by(Site_Name) %>%
  count()
##########################################################################
# Cleaning and deduplicating
##########################################################################

# Filter out plots we didn't inventory in 2020 and records without tag numbers
inv <- invdata %>%
  #filter(!Site_Name %in% c('XX-CAR1', 'XX-CAR2', 'XX-PLN2', 'XX-PLN1')) %>%
  filter(!is.na(Tag_Number)) # remove all "NT" tag numbers (coded NA)
nrow(inv)
length(inv) == 44 #Check that there are 44 columns in resulting df
View(inv)

##################
# QUADRUPLICATES
##################

# Subset where the same tree tag has 4 entries
tag4 <- inv %>% 
  group_by(Tag_Number) %>%
  count() %>%
  filter(n > 3) %>%
  ungroup()

quadrupes <- inv[(inv$Tag_Number %in% tag4$Tag_Number),]
quadrupes

##################
# TRIPLICATES
##################

# Subset where the same tree tag has 3 entries
tag3 <- inv %>% 
  group_by(Tag_Number) %>%
  count() %>%
  filter(n == 3) %>%
  ungroup()

trips <- inv[(inv$Tag_Number %in% tag3$Tag_Number),]
nrow(tag3)
nrow(trips)
View(trips)

# Find where tag numbers with three entries have multiple height entries
trips_ht <- trips %>%
  group_by(Tag_Number) %>%
  filter(!is.na(Height_1_M)) %>%
  count() %>%
  filter(n > 1)

# Find where tag numbers with three entries have multiple DBH entries
trips_dbh <- trips %>%
  group_by(Tag_Number) %>%
  filter(!is.na(DBH_1_CM)) %>%
  count() %>%
  filter(n>1)

# Find where tag numbers with three entries have multiple canopy position entries
trips %>%
  group_by(Tag_Number) %>%
  filter(!is.na(CII)) %>%
  count() %>%
  filter(n > 1)

# Find where tag numbers with three entries have multiple latitude entries
trips %>%
  group_by(Tag_Number) %>%
  filter(!is.na(Latitude)) %>%
  count() %>%
  filter(n > 1)

# Remove erroneous duplicate height or dbh entries
inv[inv$Tag_Number %in% trips_ht$Tag_Number,]
inv[inv$Tag_Number %in% trips_dbh$Tag_Number,]

inv <- inv[-c(2666, # tag 364
                 1054 # tag 1139
),]

tag3 <- inv %>% 
  group_by(Tag_Number) %>%
  count() %>%
  filter(n == 3) %>%
  ungroup()

trips <- inv[(inv$Tag_Number %in% tag3$Tag_Number),]
nrow(tag3)
nrow(trips)

k <- function(x){
  ifelse(isFALSE(first(x[!is.na(x)])), NA, first(x[!is.na(x)]))
}

# Merge triplicates on most recent observation to produce unique entries per tag number
inv_detrip <- trips %>%
  group_by(Tag_Number) %>%
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
  group_by(Tag_Number) %>%
  count() %>%
  filter(n > 1) %>%
  ungroup()
dim(detrip_reps)

##################
#DUPLICATES
##################

# Subset where the same tree tag has 2 entries
tag2 <- inv %>% 
  group_by(Tag_Number) %>%
  count() %>%
  filter(n == 2) %>%
  ungroup()
nrow(tag2)
dupes <- inv[(inv$Tag_Number %in% tag2$Tag_Number),]
nrow(dupes)
View(dupes)

# Find where tag numbers with two entries have multiple height entries
dupes2ht <- dupes %>%
  group_by(Tag_Number) %>%
  filter(!is.na(Height_1_M)) %>%
  count() %>%
  filter(n > 1)
dupes2ht <- inv[inv$Tag_Number %in% dupes2ht$Tag_Number,]
View(dupes2ht)

# Find where tag numbers with two entries have multiple DBH entries
dupes2dbh <- dupes %>%
  group_by(Tag_Number) %>%
  filter(!is.na(DBH_1_CM)) %>%
  count() %>%
  filter(n>1)
dupes2dbh <- inv[inv$Tag_Number %in% dupes2dbh$Tag_Number,]
View(dupes2dbh)

# Find where tag numbers with two entries have multiple canopy position entries
dupes %>%
  group_by(Tag_Number) %>%
  filter(!is.na(Canopy_Position)) %>%
  count() %>%
  filter(n > 1)

# Find where tag numbers with two entries have multiple latitude entries
dupeslat <- dupes %>%
  group_by(Tag_Number) %>%
  filter(!is.na(Latitude)) %>%
  count() %>%
  filter(n > 1)
dupeslat <- inv[inv$Tag_Number %in% dupeslat$Tag_Number,]
nrow(dupeslat)
View(dupeslat)

# Merge triplicates on various criteria to produce unique entries per tag number
inv_dedupe <- dupes %>%
  group_by(Tag_Number) %>%
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
  group_by(Tag_Number) %>%
  count() %>%
  filter(n > 1) %>%
  ungroup()
dim(dedupe_reps)

################
# SINGLETONS
################

# Find singleton entries
singletons <- inv %>%
  group_by(Tag_Number) %>%
  count() %>%
  filter(n == 1) %>%
  ungroup()
singletons <- inv[(inv$Tag_Number %in% singletons$Tag_Number),]

##########################################################################
# Create a merged set of unique entries per tag number
##########################################################################

full_unq_inv <- rbind(singletons, inv_dedupe, inv_detrip)
row.names(full_unq_inv) <- NULL
dim(full_unq_inv) == c(4207, 44)
# The final dataset should have 4207 entries, all unique, including the XX-CAR and XX-PLN sites

##########################################################################
# Checks
##########################################################################

# Check for repeat entries
finalrepeats <- full_unq_inv %>% 
  group_by(Tag_Number) %>%
  count() %>%
  filter(n > 2) %>%
  ungroup()
dim(finalrepeats) == c(0,2)

all_reps <- full_unq_inv %>% 
  group_by(Tag_Number) %>%
  count() %>%
  filter(n > 1) %>%
  ungroup()
all_reps
dim(dedupe_reps)

# Find species ID conflicts
sppcon <- inv %>% 
  group_by(Tag_Number, Sp_Code) %>%
  count() %>%
  ungroup %>%
  group_by(Tag_Number) %>%
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
# Yes, we missed 20.
write.csv(misses20, 'EastRiver_2020_Missed_Measurements.csv')

##########################################################################
# Export full clean dataset
##########################################################################
write.csv(full_unq_inv, 'EastRiver_AllPlots_Inventory_Data_2018-2020_Collated.csv')