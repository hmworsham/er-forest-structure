# Predict DBH on modeled trees

## ---------------------------------------------------------------------------------------------------
# Load config
config <- config::get(file=file.path('~',
                                     'Repos',
                                     'er-forest-structure',
                                     'config',
                                     'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

## ---------------------------------------------------------------------------------------------------
## Configure Drive auth
drive_auth(path=config$drivesa)

## ---------------------------------------------------------------------------------------------------
## Ingest data

# Ingest trees
treefiles <- list.files('/global/scratch/users/worsham/trees_ls_100m', pattern='.csv', full.names=T)

# Ingest field data
tmpfile <- drive_download(
  as_id(config$extdata$invid),
  type='csv',
  path=file.path(tempdir(), config$extdata$invid),
  overwrite=T)$local_path

inv <- read.csv(tmpfile)

# Clean field data
inv <- inv[grep('XX', inv$Site_Name, invert=T),] # Target plots
inv <- inv[grep('outside plot', inv$Comments, invert=T),] # Outside plots
inv <- inv[inv$Status == 'Live',] # Living stems
inv <- inv[!is.na(inv$Latitude | !is.na(inv$Longitude)),]
# inv <- inv[inv$DBH_Avg_CM >= 5,]

# Summary stats on detected trees
trees <- mclapply(treefiles, read.csv, mc.cores=24)
alltrees <- data.table::rbindlist(trees, idcol='file')

qmd <- sqrt(mean(trees$DBH_est^2, na.rm=T))
mean.ht <- mean(trees$Z, na.rm=T)
sd.ht <- sd(trees$Z, na.rm=T)
sd.ht
p90.ht <- quantile(trees$Z, .9, na.rm=T)
p90.ht

# Summary stats on inventory trees
inv.qmd <- sqrt(mean(inv$DBH_Avg_CM^2, na.rm=T))
inv.median.ht <- median(inv$Height_Avg_M, na.rm=T)
inv.p90.ht <- quantile(inv$Height_Avg_M, .9, na.rm=T)

