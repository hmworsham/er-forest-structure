# Pull detection summary statistics and figures
# Author: Marshall Worsham | worsham@berkeley.edu
# Created: 03-21-24
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

#############################
# Data ingest
#############################

# Ingest detected trees
alltrees <- read_csv(
  drive_download(as_id('18zy_9ofHJ2-Cjl5perwN3mpjq8WTKB_6'),
                 path=file.path(tempdir(), 'alltrees.csv'),
                 overwrite = T)$local_path)

# Ingest match data
ls.match <- read.csv(drive_download(
  as_id('1LCafch6gd2bYqYTT0eeW1o5QSHXv5O50'),
  path=file.path(tempdir(), 'opt_matches.csv'),
  overwrite = T)$local_path)

# Ingest field data
tmpfile <- drive_download(
  as_id(config$extdata$invid),
  type='csv',
  path=file.path(tempdir(), config$extdata$invid),
  overwrite=T)$local_path

inv <- read.csv(tmpfile)

#############################
# Data cleaning
#############################

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

####################################
# Summary stats on inventory trees
####################################

inv.qmd <- sqrt(mean(inv$DBH_Avg_CM^2, na.rm=T))
inv.sd.dbh <- sd(inv$DBH_Avg_CM, na.rm=T)
inv.median.ht <- median(inv$Height_Avg_M, na.rm=T)
inv.p25.ht <- quantile(inv$Height_Avg_M, .25, na.rm=T)
inv.p75.ht <- quantile(inv$Height_Avg_M, .75, na.rm=T)
inv.p95.ht <- quantile(inv$Height_Avg_M, .95, na.rm=T)
inv.p99.ht <- quantile(inv$Height_Avg_M, .99, na.rm=T)
inv.sd.ht <- sd(inv$Height_Avg_M, na.rm=T)
inv.max.ht <- max(inv$Height_Avg_M, na.rm=T)

data.frame('QMD'=inv.qmd,
           'SD DBH'=inv.sd.dbh,
           'Median Height'=inv.median.ht,
           'Percentile-25 Height'=inv.p25.ht,
           'Percentile-75 Height'=inv.p75.ht,
           'Percentile-95 Height'=inv.p95.ht,
           'Percentile-99 Height'=inv.p99.ht,
           'SD Height'=inv.sd.ht,
           'Max height'=inv.max.ht)

######################################################
## Summary stats on detected trees in training
######################################################

ls.match.det <- ls.match %>%
  filter(src==1) %>%
  dplyr::select(Zpred) %>%
  mutate(H=Zpred)

ls.match.det$DBH_est <- exp(-mean.coef$alpha + mean.coef$beta*log(ls.match.det$H))*exp(mean.coef$sigma^2/2)
ls.qmd <- sqrt(mean(ls.match.det$DBH_est^2, na.rm=T))
ls.sd.dbh <- sd(ls.match.det$DBH_est, na.rm=T)
ls.median.ht <- median(ls.match.det$H, na.rm=T)
ls.p25.ht <- quantile(ls.match.det$H, .25, na.rm=T)
ls.p75.ht <- quantile(ls.match.det$H, .75, na.rm=T)
ls.p95.ht <- quantile(ls.match.det$H, .95, na.rm=T)
ls.p99.ht <- quantile(ls.match.det$H, 0.99, na.rm=T)
ls.sd.ht <- sd(ls.match.det$H, na.rm=T)
ls.max.ht <- max(ls.match.det$H, na.rm=T)

data.frame('QMD'=ls.qmd,
           'SD DBH'=ls.sd.dbh,
           'Median Height'=ls.median.ht,
           'Percentile-25 Height'=ls.p25.ht,
           'Percentile-75 Height'=ls.p75.ht,
           'Percentile-95 Height'=ls.p95.ht,
           'Percentile-99 Height'=ls.p99.ht,
           'SD Height'=ls.sd.ht,
           'Max Height'=ls.max.ht)

###############################################
# Summary stats on ALL detected trees
###############################################

ntrees <- nrow(alltrees)

qmd <- sqrt(mean(alltrees$DBH_est^2, na.rm=T))
sd.dbh <- sd(alltrees$DBH_est, na.rm=T)
median.ht <- median(alltrees$H, na.rm=T)
p90.ht <- quantile(alltrees$H, .95, na.rm=T)
sd.ht <- sd(alltrees$H, na.rm=T)
p25.ht <- quantile(alltrees$H, .25, na.rm=T)
p75.ht <- quantile(alltrees$H, .75, na.rm=T)

data.frame('QMD'=qmd,
           'SD DBH'=sd.dbh,
           'Median Height'=median.ht,
           'Percentile-90 Height'=p90.ht,
           'SD Height'=sd.ht,
           'Percentile-25 Height'=p25.ht,
           'Percentile-75 Height'=p75.ht)

####################################################
# Esimate correction factor on ALL detected trees
####################################################

alltrees.corx <- alltrees %>%
  mutate(bin = cut(H, breaks=seq(0,max(H),1))) %>%
  group_by(bin) %>%
  summarise(n=n(),
            freq=n()/nrow(.))

lstrees.obs.corx <- ls.match %>%
  filter(src==0) %>%
  mutate(bin = cut(Zobs, breaks=seq(0,max(Zobs),1))) %>%
  group_by(bin) %>%
  summarise(n_obs=sum(!is.na(Zobs)),
            f_obs=sum(!is.na(Zobs))/nrow(.))

lstrees.det.corx <- ls.match %>%
  filter(src==1) %>%
  mutate(bin = cut(Zpred, breaks=seq(0,max(Zpred),1))) %>%
  group_by(bin) %>%
  summarise(n_det=sum(!is.na(Zpred)),
            f_det=sum(!is.na(Zpred))/nrow(.))

lstrees.corx <- full_join(lstrees.obs.corx, lstrees.det.corx, by='bin') %>%
  mutate(pct_diff=f_obs-f_det,
         pct_scale=1+pct_diff)

lstrees.corx.l <- lstrees.corx %>%
  pivot_longer(cols=c(f_obs, f_det))

alltrees.corx <- alltrees.corx %>%
  full_join(lstrees.corx, by='bin') %>%
  mutate(n_corr=n*pct_scale,
         f_corr=n_corr/23915758,
         hbin=1:nrow(.))

alltrees.corx.l <- alltrees.corx %>%
  pivot_longer(cols=c(n, n_corr)) %>%
  mutate(name=ifelse(name=='n', 'Pre-correction', 'Post-correction'))

# Number of corrected trees
sum(alltrees.corx$n_corr, na.rm=T)

# Plot corrected vs original
alltrees.corx.plt <- ggplot(alltrees.corx.l, aes(x=hbin, y=value, group=name, fill=name)) +
  geom_col(width=.8, position=position_dodge(.8)) +
  scale_fill_manual(values=c('#E31A1C', '#FD8D3C'), name=NULL) +
  scale_x_continuous(limits=c(0,40)) +
  labs(x='Height (1 m bins)', y='Number of detected tree crown objects') +
  ggthemes::theme_calc(base_size=8) +
  theme(legend.position = c(1,1),
        legend.justification = c('right', 'top'),
        legend.box.background = element_rect(fill = "white", color = "black"))

cairo_pdf('~/Desktop/Fig4.pdf', width=90/25.4, height=90/25.4, onefile=T,
          family='Arial', bg='white')

print(alltrees.corx.plt)

dev.off()
