# Predict DBH on modeled trees
# Author: Marshall Worsham | worsham@berkeley.edu
# Created: 03-02-24
# Revised: 07-23-24

#############################
# Set up working environment
#############################

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

# Configure drive auth
drive_auth(path=config$drivesa)

# Define parallel scope
nCores <- as.integer(availableCores()-2)

# Define directories
datadir <- file.path(config$extdata$scratch, 'trees_ls_100m')

#############################
# Data ingest
#############################

# Ingest trees
treefiles <- list.files(datadir, pattern='.shp', full.names=T)

# Ingest field data
tmpfile <- drive_download(
  as_id(config$extdata$invid),
  type='csv',
  path=file.path(tempdir(), config$extdata$invid),
  overwrite=T)$local_path

inv <- read.csv(tmpfile)

#############################
# Clean data
#############################

# Select the variables of interest

allom <- inv %>%
  filter(!Site_Name %in% c('XX-CAR1', 'XX-CAR2', 'XX-CAR3',
                           'XX-PLN1', 'XX-PLN2', 'SG-NWS1',
                           'XX-FAR1', 'ER-BME3'),
         Height_Avg_M>=1.3,
         Height_Avg_M/DBH_Avg_CM > 0.17,
         Height_Avg_M/DBH_1_CM < 10,
         !grepl('outside plot', Comments),
         Status=='Live',
         !is.na(inv$Latitude) | !is.na(inv$Longitude)) %>%
  dplyr::select(Site_Name,
                Tree_ID=Tag_Number,
                Height_Avg_M,
                DBH_Avg_CM) %>%
  mutate(Tree_ID=as.character(Tree_ID)) %>%
  na.omit()

# Scatterplot
ggplot(allom, aes(x=Height_Avg_M, y=DBH_Avg_CM)) +
  geom_point(shape=21) +
  labs(x='Height (m)', y='DBH (cm)')

# Break data into bins
# allom.bins <- allom %>%
#   mutate(bin = ntile(Height_Avg_M, 50)) %>%
#   group_by(bin) %>%
#   summarise(across(Height_Avg_M:DBH_Avg_CM, \(x) mean(x, na.rm=T)))

#############################
# Setup allometry estimation
#############################

## Jucker approach

RMSE<-function(Obs,Pred){sqrt(mean((Obs-Pred)^2))} # function to calculate Root Mean Square Error (RMSE) of model predictions
bias<-function(Obs,Pred){mean((Pred-Obs)/Obs)*100} # function to calculate average systematic bias (%) of model predictions
relative_err<-function(Obs,Pred){((Pred-Obs)/Obs)*100} # function to calculate the relative error (%) of each model prediction
CV<-function(Obs,Pred){sqrt(sum((Pred-Obs)^2)/(length(Obs)))/mean(Obs)*100} # function to calculate the tree-level coefficient of variation of the model
sigma_v<-function(Obs,Pred){sqrt(sum((log(Obs)-log(Pred))^2)/(length(Obs)-2))} # function to calculate the residual standard deviation
Prediction_interval<-function(t_crit,sigma_v,n,x_new,x_mean,x_sd){
  t_crit * sigma_v * sqrt(1 + 1/n + (((x_new-x_mean)^2)/((n-1)*x_sd^2)))} # function to calculate prediction intervals

# Break data into log-scale diameter classes
allom.bins <- allom %>%
  mutate(bin = ntile(log(DBH_Avg_CM), 20))

#############################
# Data binning
#############################

## Create empty dataframes to store model fit statistics for each randomization step
reps<-100
raw.model <- data.frame(matrix(nrow=reps, ncol=6))
colnames(raw.model)[1:6]<-c("alpha","beta","sigma","RMSE","bias","CV")
bin.model <- data.frame(matrix(nrow=reps, ncol=7))
colnames(bin.model)[1:7]<-c("alpha","beta","sigma","sigma_v","RMSE","bias","CV")
bin.nl.model <- data.frame(matrix(nrow=reps, ncol=8))
colnames(bin.nl.model)[1:8] <- c('alpha', 'beta', 'gamma', 'sigma',
                              'sigma_v', 'RMSE', 'bias', 'CV')

## Create empty plot to visualize model errors for each randomization step
err.rand.plt <- function() {
  plot(1,1,log='x', pch=16, col="white",bty="l",xlim=c(1,200),ylim=c(-100,100),
       xlab="Observed diameter (cm)",ylab="Error (%)",las=1,cex.axis=0.8,cex.lab=0.9)
  grid(equilogs=FALSE,lty=2,col="grey80",lwd=0.5)
  abline(h=0,col="grey20",lty=2)
  legend("bottomleft",lwd=2,col=c("forestgreen","grey20", 'dodgerblue'),
         c(as.expression(bquote("log(DBH) ~"~alpha + beta~log(H[raw])~x~e^(σ^2/2))),
           as.expression(bquote("log(DBH) ~"~alpha + beta~log(H[binned])~x~e^(σ^2/2))),
           as.expression(bquote("log(DBH) ~"~alpha + log(H[binned]-beta)^gamma~x~e^(σ^2/2)))
         ),
                                       bty="n", cex=0.8)

  for (i in 1:reps){

    #### Set up validation and model datasets
    val.data <- allom.bins %>%
      group_by(bin) %>%
      slice_sample(prop=0.25) %>%
      ungroup() %>%
      rename(H=Height_Avg_M,
             DBH=DBH_Avg_CM)

    mod.trees <- data.frame(Tree_ID=setdiff(allom.bins$Tree_ID, val.data$Tree_ID))

    mod.data <- merge(allom.bins, mod.trees) %>%
      rename(H=Height_Avg_M,
             DBH=DBH_Avg_CM)

    #### Data binning: calculate mean values per size class for each allometric component
    bin.data <- mod.data %>%
      group_by(bin) %>%
      summarise(n.trees=length(DBH),
                DBH=mean(DBH),
                H=mean(H)) %>%
      filter(n.trees>2)

    #### Fit models and generate predictions

    ## Model 1: model fit to raw data
    m1 <- lm(log(DBH) ~ log(H), data=mod.data)
    val.data$DBH_pred_m1 <- exp(predict(m1, val.data))*exp(summary(m1)$sigma^2/2)
    val.data$error_m1 <- relative_err(val.data$DBH, val.data$DBH_pred_m1)
    lines(smooth.spline(val.data$error_m1 ~ val.data$DBH, nknots=5),
          col='forestgreen',
          lwd=0.5)

    ## Model 2: model fit to binned data
    m2 <- lm(log(DBH) ~ log(H), data=bin.data)
    val.data$DBH_pred_m2 <- exp(predict(m2, val.data))*exp(summary(m2)$sigma^2/2)
    val.data$error_m2 <- relative_err(val.data$DBH, val.data$DBH_pred_m2)
    lines(smooth.spline(val.data$error_m2 ~ val.data$DBH, nknots=5),
          col='grey20',
          lwd=0.5)

    ## Model 3: nonlinear model fit to binned data
    m3 <- nls(log(DBH) ~ a + log((H-b)^g),
              start=list(a=1.6, b=1.3, g=.8),
              weights=H,
              lower=c(0, 1, 0.2),
              algorithm='port',
              data=bin.data,
              na.action=na.exclude,
              control=nls.control(maxiter=1000))
    val.data$DBH_pred_m3 <- exp(predict(m3, val.data))*exp(summary(m3)$sigma^2/2)
    val.data$error_m3 <- relative_err(val.data$DBH, val.data$DBH_pred_m3)
    lines(smooth.spline(val.data$error_m3 ~ val.data$DBH, nknots=5),
          col='dodgerblue',
          lwd=0.5)

    #### Compute and store model error statistics

    ## Model fit to raw data
    raw.model[i,"alpha"]<-coef(m1)[1]
    raw.model[i,"beta"]<-coef(m1)[2]
    raw.model[i,"sigma"]<-summary(m1)$sigma
    raw.model[i,"RMSE"]<-RMSE(val.data$DBH, val.data$DBH_pred_m1)
    raw.model[i,"bias"]<-bias(val.data$DBH, val.data$DBH_pred_m1)
    raw.model[i,"CV"]<-CV(val.data$DBH ,val.data$DBH_pred_m1)

    ## Model fit to binned data
    bin.model[i,"alpha"]<-coef(m2)[1]
    bin.model[i,"beta"]<-coef(m2)[2]
    bin.model[i,"sigma"]<-summary(m2)$sigma
    bin.model[i,"sigma_v"]<-sigma_v(val.data$DBH, val.data$DBH_pred_m2)
    bin.model[i,"RMSE"]<-RMSE(val.data$DBH,val.data$DBH_pred_m2)
    bin.model[i,"bias"]<-bias(val.data$DBH,val.data$DBH_pred_m2)
    bin.model[i,"CV"]<-CV(val.data$DBH, val.data$DBH_pred_m2)

    ## NLS model fit to binned data
    bin.nl.model[i,"alpha"] <- coef(m3)[1]
    bin.nl.model[i,"beta"] <- coef(m3)[2]
    bin.nl.model[i,'gamma'] <- coef(m3)[3]
    bin.nl.model[i,"sigma"] <- summary(m3)$sigma
    bin.nl.model[i,"sigma_v"] <- sigma_v(val.data$DBH, val.data$DBH_pred_m3)
    bin.nl.model[i,"RMSE"] <- RMSE(val.data$DBH, val.data$DBH_pred_m3)
    bin.nl.model[i,"bias"] <- bias(val.data$DBH, val.data$DBH_pred_m3)
    bin.nl.model[i,"CV"] <- CV(val.data$DBH, val.data$DBH_pred_m3)

  }
}

#####################################################
# Estimate uncertainty for models fit to raw data
#####################################################

# Final model - updates parameters based on means from prior runs
mean.raw.coef <- summarise(raw.model, across(alpha:CV, \(x) sqrt(mean(x^2))))
mean.coef <- summarise(bin.model, across(alpha:CV, \(x) sqrt(mean(x^2))))
mean.nl.coef <- summarise(bin.nl.model, across(alpha:CV, \(x) sqrt(mean(x^2))))

t_crit <- 1.96 # critical value of the Student's t distribution
n <- length(val.data$DBH) # number of observations
sigma.mean <- mean(bin.model$sigma) # mean sigma value based on 100 randomization steps
sigma.v.mean <- mean(bin.model$sigma_v) # mean sigma_v value based on 100 randomization steps
H.new <- seq(min(log(val.data$H)), max(log(val.data$H)),len=100) # new data for which to make predictions
H.mean <- mean(log(val.data$H)) # mean value of the explanatory variable
H.sd <- sd(log(val.data$H)) # standard deviation of the explanatory variable

# Plot
plot(DBH ~ H, val.data,
     col='grey80', pch=16, cex=0.2, bty="l",
     log='xy', xlim=c(0.5,500), ylim=c(1,300),
     ylab="Diameter (cm)", xlab="Height (m)", xaxt="n", cex.lab=1.1, cex.axis=0.9)
axis(1,at=c(0.5,5,50,500,5000),c("0.5","5","50","500","5000"),cex.lab=1.1,cex.axis=0.9)

new.dat <- data.frame(H=exp(H.new))
new.dat$DBH_pred <- exp(-mean.coef$alpha + mean.coef$beta*log(new.dat$H))*exp(mean.coef$sigma^2/2)
new.dat$DBH_pred <- exp(predict(m2,new.dat))*exp(summary(m2)$sigma^2/2)
points(DBH_pred ~ H, new.dat[new.dat$DBH_pred>1,], type="l", col="red", lwd=3)

new.dat$PI_hi <- exp(log(new.dat$DBH_pred)+Prediction_interval(t_crit,sigma.v.mean,n,H.new,H.mean, H.sd))
new.dat$PI_lo <- exp(log(new.dat$DBH_pred)-Prediction_interval(t_crit,sigma.v.mean,n,H.new,H.mean,H.sd))
new.dat$PI_sig_hi <- exp(log(new.dat$DBH_pred)+Prediction_interval(t_crit,sigma.mean,n,H.new,H.mean,H.sd))
new.dat$PI_sig_lo <- exp(log(new.dat$DBH_pred)-Prediction_interval(t_crit,sigma.mean,n,H.new,H.mean,H.sd))
points(PI_hi~H,new.dat[new.dat$PI_hi>1,],type="l",col="#4682B4",lty=2,lwd=2)
points(PI_lo~H,new.dat[new.dat$PI_lo>1,],type="l",col="#4682B4",lty=2,lwd=2)
points(PI_sig_hi~H,new.dat[new.dat$PI_hi>1,],type="l",col="black",lty=3,lwd=1)
points(PI_sig_lo~H,new.dat[new.dat$PI_lo>1,],type="l",col="black",lty=3,lwd=1)

########################
# Run on validation data
########################

# 1:1 plot for val data
# Select the variables of interest
allom.val <- inv %>%
  filter(Site_Name %in% c('XX-CAR1', 'XX-CAR2', 'XX-CAR3',
                           'XX-PLN1', 'XX-PLN2', 'SG-NWS1',
                           'XX-FAR1', 'ER-BME3'),
         Height_Avg_M>=1.3,
         Height_Avg_M/DBH_Avg_CM > 0.17,
         Height_Avg_M/DBH_1_CM < 10) %>%
  dplyr::select(Site_Name,
                Tree_ID=Tag_Number,
                Height_Avg_M,
                DBH_Avg_CM) %>%
  mutate(Tree_ID=as.character(Tree_ID)) %>%
  rename(DBH=DBH_Avg_CM,
         H=Height_Avg_M) %>%
  sample_n(800) %>%
  na.omit()

# Scatterplot
ggplot(allom.val, aes(x=log(H), y=log(DBH))) +
  geom_point(shape=21) +
  labs(x='Height (m)', y='DBH (cm)')

# Predict DBH from original
allom.val$DBH_pred <- exp(-mean.coef$alpha + mean.coef$beta*log(allom.val$H))*exp(mean.coef$sigma^2/2)

# Compute RMSE, bias, CV.
allom.val.RMSE <- RMSE(allom.val$DBH, allom.val$DBH_pred)
allom.val.bias <- bias(allom.val$DBH, allom.val$DBH_pred)
allom.val.CV <- CV(allom.val$DBH, allom.val$DBH_pred)

## Use densCols() output to get density at each point
dens <- densCols(allom.val$DBH, allom.val$DBH_pred,
                 colramp=colorRampPalette(c('black', 'white')))

allom.val$dens <- col2rgb(dens)[1,] + 1L

## Reorder rows so that densest points are plotted on top
allom.val <- allom.val[order(allom.val$dens),]

## Plot
allom.val.plt <- ggplot(allom.val, aes(x=DBH, y=DBH_pred, color=dens)) +
  geom_point() +
  scale_color_viridis_c(name='Number of observations',
                        guide=guide_colorbar(
                          title.position='right',
                          title.theme=element_text(angle=-90, hjust=0.5))) +
  geom_abline(intercept=0, slope=1) +
  labs(x='Field-observed DBH (cm)', y='Allometry-estimated DBH (cm)') +
  coord_fixed(ratio=1, xlim=c(0,100), ylim=c(0,100)) +
  ggthemes::theme_calc(base_size=12, base_family='Arial') +
  theme(aspect.ratio = 1,
        legend.key.height=unit(0.1, 'npc'))

# Export plot
cairo_pdf('~/Desktop/FigS3a.pdf', width=190/25.4, height=190/25.4, onefile=T,
          family='Arial', bg='white')

err.rand.plt()

dev.off()

cairo_pdf('~/Desktop/FigS3b.pdf', width=190/25.4, height=190/25.4, onefile=T,
          family='Arial', bg='white')

allom.val.plt

dev.off()

########################
# Predict
########################

# Predict diameter from height in model data and write to csv
mclapply(treefiles, \(x) {
  tf <- st_read(x, quiet=T)
  tf <- rename(tf, H=Z)[,-which(names(tf)=='id_1')]

  t_crit <- 1.96 # critical value of the Student's t distribution
  n <- nrow(tf)
  H.mean <- mean(log(tf$H)) # mean value of the explanatory variable
  H.sd <- sd(log(tf$H)) # standard deviation of the explanatory variable

  tf$DBH_est <- exp(-mean.coef$alpha + mean.coef$beta*log(tf$H))*exp(mean.coef$sigma^2/2)
  tf$DBH_lb <- exp(log(tf$DBH_est)-Prediction_interval(t_crit, sigma.v.mean, n, tf$H, H.mean, H.sd))
  tf$DBH_ub <- exp(log(tf$DBH_est)+Prediction_interval(t_crit, sigma.v.mean, n, tf$H, H.mean, H.sd))
  tf$BA_est <- pi*(tf$DBH_est/2)**2
  tf$BA_est_lb <- tf$BA_est - Prediction_interval(t_crit, sigma.v.mean, n, tf$DBH_est^2, mean(tf$DBH_est), sd(tf$DBH_est))
  tf$BA_est_ub <- tf$BA_est + Prediction_interval(t_crit, sigma.v.mean, n, tf$DBH_est^2, mean(tf$DBH_est), sd(tf$DBH_est))
  outname <- str_split(basename(x), '\\.', simplify=T)[1]
  st_write(data.frame(tf),
           file.path('/global/scratch/users/worsham/trees_ls_100m_csv', paste0(outname, '.csv')),
           layer_options = "GEOMETRY=AS_XY",
           append=F)
  },
  mc.cores = getOption("mc.cores", nCores)
)
