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
drive_auth(path=config$drivesa)

# Ingest trees
#treefiles <- list.files(config$extdata$trees)
treefiles <- list.files('/global/scratch/users/worsham/trees_lmffw_csv', pattern='.csv', full.names=T)


# Ingest field data
tmpfile <- drive_download(
  as_id(config$extdata$invid),
  type='csv',
  path=file.path(tempdir(), config$extdata$invid),
  overwrite=T)$local_path

inv <- read.csv(tmpfile)

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

