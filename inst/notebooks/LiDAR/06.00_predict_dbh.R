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
treefiles <- list.files('/global/scratch/users/worsham/trees_lmffw', pattern='.shp', full.names=T)
treefiles.mfp <- list.files('/global/scratch/users/worsham/trees_lmffw_missing_fp', pattern='.shp', full.names=T) # tree files from original NEON LAS for missing flightpath
treefiles <- c(treefiles, treefiles.mfp)

# Ingest field data
tmpfile <- drive_download(
  as_id(config$extdata$invid),
  type='csv',
  path=file.path(tempdir(), config$extdata$invid),
  overwrite=T)$local_path

inv <- read.csv(tmpfile)

# Select the variables of interest
allom <- inv %>%
  filter(!Site_Name %in% c('XX-CAR1', 'XX-CAR2', 'XX-CAR3',
                           'XX-PLN1', 'XX-PLN2', 'SG-NWS1',
                           'XX-FAR1', 'ER-BME3')) %>%
  dplyr::select(Site_Name,
         Height_Avg_M,
         DBH_Avg_CM) %>%
  na.omit()

# Scatterplot with a smooth fit, grouped by Site_Name
ggplot(allom, aes(x=DBH_Avg_CM, y=Height_Avg_M)) +
  geom_point(shape=21) +
  geom_smooth(se=F) +
  labs(x='DBH (cm)', y='Height (m)')

# Fit exponential model to field height and diameter
nthroot = function(x,n) {
  (abs(x)^(1/n))*sign(x)
}

nlmod <- nls(DBH_Avg_CM~nthroot(Height_Avg_M/a, b),
             start=list(a=0.1, b=0.01),
             data=allom,
             na.action=na.exclude,
             control=nls.control(maxiter=1000))

# nlmod <- nls(Height_Avg_M~a*DBH_Avg_CM^b,
#              start=list(a=0.1, b=0.01),
#              data=allom,
#              na.action=na.exclude,
#              control=nls.control(maxiter=1000))

# Pull coefficients
coef <- summary(nlmod)$coefficients
a <- coef[1,'Estimate']
b <- coef[2, 'Estimate']
a.se <- coef[1, 'Std. Error']
b.se <- coef[2, 'Std. Error']

# Predict diameter from height in field data
#yhat <- predict(nlmod, allom$DBH_Height_M)
yhat <- nthroot(allom$Height_Avg_M/a, b)
allom$yhat <- yhat
resid <- allom$yhat-allom$DBH_Avg_CM

# What is RMSE of DBH estimate?
dbh.est.rmse <- sqrt(mean(resid^2))
dbh.est.rmse

# Plot predictions
ggplot(allom, aes(x=Height_Avg_M)) +
  geom_point(aes(y=DBH_Avg_CM), shape=21, color='blue') +
  geom_point(aes(y=yhat), shape=21, color='red') +
  geom_line(aes(y=yhat))

# Predict diameter from height in model data and write to csv
#trees <- bind_rows(map(treefiles, st_read))

mclapply(treefiles, \(x) {
  tf <- st_read(x)
  tf$DBH_est <- nthroot(tf$Z/a, b)
  tf$DBH_lb <- nthroot(tf$Z/(a-a.se), b-b.se)
  tf$DBH_ub <- nthroot(tf$Z/(a+a.se), b+b.se)
  tf$BA_est <- pi*(tf$DBH_est/2)**2
  tf$BA_est_lb <- pi*(tf$DBH_lb/2)**2
  tf$BA_est_ub <- pi*(tf$DBH_ub/2)**2
  outname <- str_split(basename(x), '\\.', simplify=T)[1]
  st_write(data.frame(tf), file.path('/global/scratch/users/worsham/trees_lmffw_csv', paste0(outname, '.csv')), layer_options = "GEOMETRY=AS_XY")
},
mc.cores = getOption("mc.cores", 30)
)


