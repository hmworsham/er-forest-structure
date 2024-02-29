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

# Set number of cores
nCores <- as.integer(availableCores()-2)

## ---------------------------------------------------------------------------------------------------
## Configure Drive auth
drive_auth(path=config$drivesa)

## ---------------------------------------------------------------------------------------------------
## Ingest data

# Ingest detected trees
alltrees <- read_csv(file.path(config$extdata$scratch, 'trees_masked_100m.csv'))

# Ingest field data
tmpfile <- drive_download(
  as_id(config$extdata$invid),
  type='csv',
  path=file.path(tempdir(), config$extdata$invid),
  overwrite=T)$local_path

inv <- read.csv(tmpfile)

## ---------------------------------------------------------------------------------------------------
## Clean data

# Clean field data
inv <- inv %>%
  filter(!Site_Name %in% c('XX-CAR1', 'XX-CAR2', 'XX-CAR3',
                           'XX-PLN1', 'XX-PLN2', 'SG-NWS1',
                           'XX-FAR1', 'ER-BME3'),
         Height_Avg_M>=1.3,
         Height_Avg_M/DBH_Avg_CM > 0.17,
         Height_Avg_M/DBH_1_CM < 10,
         !grepl('outside plot', Comments),
         Status=='Live',
         !is.na(inv$Latitude) | !is.na(inv$Longitude))

# Remove unlikely detected trees
alltrees <- alltrees[alltrees$H<=60,]

## ---------------------------------------------------------------------------------------------------
## Summary stats on detected trees

qmd <- sqrt(mean(alltrees$DBH_est^2, na.rm=T))
sd.dbh <- sd(alltrees$DBH_est, na.rm=T)
median.ht <- median(alltrees$H, na.rm=T)
p90.ht <- quantile(alltrees$H, .9, na.rm=T)
sd.ht <- sd(alltrees$H, na.rm=T)

data.frame('QMD'=qmd,
           'SD DBH'=sd.dbh,
           'Median Height'=median.ht,
           'Percentile-90 Height'=p90.ht,
           'SD Height'=sd.ht)

## ---------------------------------------------------------------------------------------------------

## Summary stats on inventory trees
inv.qmd <- sqrt(mean(inv$DBH_Avg_CM^2, na.rm=T))
inv.sd.dbh <- sd(inv$DBH_Avg_CM, na.rm=T)
inv.median.ht <- median(inv$Height_Avg_M, na.rm=T)
inv.p90.ht <- quantile(inv$Height_Avg_M, .9, na.rm=T)
inv.sd.ht <- sd(inv$Height_Avg_M, na.rm=T)

data.frame('QMD'=inv.qmd,
           'SD DBH'=inv.sd.dbh,
           'Median Height'=inv.median.ht,
           'Percentile-90 Height'=inv.p90.ht,
           'SD Height'=inv.sd.ht)
