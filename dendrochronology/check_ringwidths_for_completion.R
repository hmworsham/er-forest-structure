library(readxl)

# Ingest index
idx <- read_xlsx('/Volumes/GoogleDrive/My Drive/Research/RMBL/RMBL-East River Watershed Forest Data/Data/Dendrochronology/Dendro_Core_Index.xlsx', sheet='Cores')

# Ingest names of measurement files
files <- list.files('/Volumes/GoogleDrive/My Drive/Research/RMBL/RMBL-East River Watershed Forest Data/Data/Dendrochronology/Ring_Widths', pattern='.csv')

# Pull core IDs
idx.did <- paste0(idx$ITRDB_SiteID, sprintf('%04d', as.numeric(idx$Tree)), idx$Core)
files.did <- unlist(lapply(strsplit(files, '_'), '[', 1))

# List all core IDs in index
idx.did

# List all core IDs in measurement files
files.did       

# Find duplicate core IDs in index and measurement files
files.did[which(duplicated(files.did))]
idx.did[which(duplicated(idx.did))]

# Find measurement files not in index and index entries not in measurement files
# Both should return 0
idx.did[which(!idx.did %in% files.did)]
files.did[which(!files.did %in% idx.did)]
