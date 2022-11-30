install.packages('ggsci')

library(tidyverse)
library(reshape2)
library(dplR)
library(ggplot2)
library(ggsci)
library(wesanderson)
library(forecast)
library(waveslim)
library(car)

# Source local functions
source(file.path('.', 'rwl', 'dendro.helpers.R'))

######################
# Set up environment
######################

# Define directory of ring-width data
datadir <- file.path('/Applications', 'DendroApps')
#dendrodir <- file.path(datadir, 'Dendrochronology')
#rwdir <- file.path(dendrodir, 'Ring_Widths')
rwldir <- file.path(datadir, 'rwl')
climdir <- file.path(datadir, 'Climate')
precipdir <- file.path(climdir, 'NOAA_CDO', 'COOP', 'Crested_Butte')
tempdir <- file.path(climdir, 'Berkeley_Earth', 'CrestedButte_927113_T_Breakpoint_Corrected')

###################################
# Generate ring-width time series
###################################

# Aggregate rwls from all sites and species
rwfiles <- list.files(rwldir, pattern='_dated', full.names=T)
rws <- lapply(rwfiles, read.tucson)
sites <- c('APL', 'GTA', 'CRA', 'CRB', 'BMA')
spp <- c('ABLA', 'PIEN')
ids <- paste(rep(sites, each = length(spp)), spp, sep = "_")

# Build master chronologies for each site-spp set based on raw ring measurements
crn <- lapply(rws, chron)

# Detrend series
dt <- lapply(rws, dplR::detrend, method='Spline')

# Build master chronologies for each site-spp set based on detrended ring measurements
crn <- lapply(dt, chron)

# The next five chunks are just dataframe manipulation; the end result is a dataframe
# of all the years represented across all series, and the associated averaged and 
# standardized annual chronology values for each site+species

# Assign year as distinct column
for(i in seq(length(crn))){
  crn[[i]]$YEAR = as.numeric(rownames(crn[[i]]))
}

# Make a wide df of years and rwi
crn.long <- do.call('rbind', crn) # Rowbind the list of chronologies
crn.df <- data.frame('YEAR'=seq(as.numeric(min(rownames(crn.long))), 2019)) # Initialize df with years

# Iteratively add site-spp rwis and sample depths by year
for(i in crn){
  crn.df = left_join(crn.df, i, by = 'YEAR')
}

# Rename columns so they're interpretable
names(crn.df) <- c('YEAR', unlist(lapply(ids, paste, c('STDDEV', 'SAMPLE_DEPTH'), sep='_')))

# Select year and rwi values only
crn.df <- select(crn.df, matches('YEAR|STDDEV'))

###############
# Climate
###############

# Ingest precip data
precipfiles <- list.files(precipdir, full.names=T)
precip <- read.csv(precipfiles[1])

# Create precip record
precip <- precip[,c('DATE', 'PRCP')]
precip$DATE <- as.POSIXct(precip$DATE)
precip <-  precip %>% 
  mutate(YEAR=as.numeric(format(precip$DATE, '%Y')),
         MONTH=as.numeric(format(precip$DATE, '%M'))) %>% # adds new columns for year and month in which precip was recorded
  group_by(YEAR) %>%
  summarise(ANN_P = sum(PRCP, na.rm=T)) %>% # averages monthly precip to annual values
  mutate(P_MEAN = mean(ANN_P), # adds a column for mean precip across all years
         P_ANOM = ANN_P-P_MEAN,  # adds a column for precip anomaly (p in year i - P_MEAN)
         P_ANOM_NEG = P_ANOM < 0, # adds a boolean column indicating + or - anomaly from mean
         P_ANOM_STD = (P_ANOM-mean(P_ANOM))/sd(P_ANOM), # adds a column with standardized anomaly value (mean=0)
         P_ANOM_CAT = case_when((P_ANOM_STD <= -1) ~ 'Extreme low', # adds colum with categorical values for extreme, moderate, and typical conditions, based on 0, 0.5, and 1 standard deviations from the mean
                                (P_ANOM_STD <= -0.5 & P_ANOM_STD > -1) ~ 'Moderate low',
                                (P_ANOM_STD <= 0.5 & P_ANOM_STD > -0.5) ~ 'Median conditions',
                                (P_ANOM_STD <= 1 & P_ANOM_STD > 0.5) ~ 'Moderate high',
                                (P_ANOM_STD >= 1) ~ 'Extreme high',
                                TRUE ~ 'NA'))

p.anom <- precip[c('YEAR', 'P_ANOM_STD', 'P_ANOM_CAT')] # selects just the year, standardized p anomaly and categorical columns
p.anom.rep <- p.anom[rep(seq_len(nrow(p.anom)), length(crn)),] 
p.anom.rep$SITE <- sort(rep(ids, nrow(precip)))

View(precip)

###################################
# Merge precip and chronologies
###################################
# Merge precip and rwi
crn.df.long <- melt(crn.df, id=c('YEAR'))
crn.p <- merge(crn.df.long, p.anom, on='YEAR')

crn.p$SITE <- unlist(lapply(str_split(crn.p$variable, '_'), '[', 1))
crn.p$SP <- unlist(lapply(str_split(crn.p$variable, '_'), '[', 2))
head(crn.p, length(crn))

crn.p.si.means <- crn.p %>%
  group_by(SITE, YEAR) %>%
  mutate(RWI_SITE_MEAN=mean(value)) %>%
  ungroup()

crn.p.sp.means <- crn.p %>%
  group_by(SP, YEAR) %>%
  mutate(RWI_SP_MEAN=mean(value)) %>%
  #summarize(RWI_SP_MEAN=mean(value))%>%
  ungroup()

crn.p.sp.means <- crn.p.sp.means[crn.p.sp.means$YEAR<2019,]
crn.p.site.means <- crn.p.si.means[crn.p.si.means$YEAR<2019,]

###########################
# Cross correlation
###########################
crn.p.wide <- inner_join(crn.df, precip, on='YEAR')

# Find autocorrelations in a RWI chronology by lags
acf(crn.p.wide$APL_ABLA_STDDEV, lag.max=25) # Autocorrelation
pacf(crn.p.wide$APL_ABLA_STDDEV, lag.max=25) # Partial autocorrelation

# Find autocorrelations in precipitation record
acf(crn.p.wide$P_ANOM, lag.max=25)
pacf(crn.p.wide$P_ANOM, lag.max=25)

# Find cross-correlations betwen RWI chronology and P record at a given lag
crosscorr.AA.P <- ccf(crn.p.wide$P_ANOM, crn.p.wide$APL_ABLA_STDDEV, lag=5)

########################
# Simple linear models
########################

# Add lags as dataframe columns
crn.p.wide$P_LAG1 <- lag(crn.p.wide$P_ANOM, 1)
crn.p.wide$P_LAG2 <- lag(crn.p.wide$P_ANOM, 2)
crn.p.wide$P_LAG3 <- lag(crn.p.wide$P_ANOM, 3)
crn.p.wide$P_LAG4 <- lag(crn.p.wide$P_ANOM, 4)
crn.p.wide$P_LAG5 <- lag(crn.p.wide$P_ANOM, 5)

# Time series linear models
# Is there a significant linear relationship between precipitation and the APL_ABLA time series at any given lag? Is there a difference in R2 or estimated coefficient for different lags? 
lm.AA <- tslm(ts(crn.p.wide$APL_ABLA_STDDEV) ~ 
                 ts(crn.p.wide$P_LAG1)
               + ts(crn.p.wide$P_LAG2)
               + ts(crn.p.wide$P_LAG3)
               + ts(crn.p.wide$P_LAG4)
               + ts(crn.p.wide$P_LAG5))

summary(lml.AA)

# Challenge: try it for other species or sites
lm.GP <- tslm(xxxxx)

######################
# Plots
######################
# Define a pretty color palette
pal <- hcl.colors('Sunset', n=10)
summary(crn.p$P_ANOM_STD)

# Precip anomaly and ringwidths from each site and species
ggplot(crn.p, aes(x=YEAR)) + 
  geom_line(aes(y=value, color=variable)) +
  #facet_grid(vars(variable)) +
  geom_col(aes(y=P_ANOM_STD/10)) + 
  scale_color_manual(values=pal) +
  scale_y_continuous(
    name=bquote('Ring-width Index (unitless)'),
    sec.axis = sec_axis(~., name=bquote('Standardized Precipitation Anomaly (unitless)')))

# Precip anomaly and RWI by species
ggplot(crn.p.sp.means, aes(x=YEAR)) + 
  #facet_wrap(vars(SP)) +
  geom_line(aes(y=RWI_SP_MEAN, color=SP)) +
  geom_col(aes(y=P_ANOM_STD/10)) + 
  #scale_color_manual(values=pal) +
  #scale_color_breer(palette='Oranges') + 
  scale_color_jco(labels= c('ABLA',
                            'PIEN')) +
  scale_y_continuous(
    name=bquote('Ring-width Index'),
    sec.axis = sec_axis(~., name=bquote('Precipitation Anomaly' (mm ~y^-1)))) +
  labs(title = 'Ring Width Index by Spp. vs. Precipitation Anomaly, 1909-2018',
       color = 'Species') +
  xlab('Year') +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        plot.title = element_text(size=20))

# Precip anomaly and ring width by site
ggplot(crn.p.si.means, aes(x=YEAR)) + 
  #facet_wrap(vars(SP)) +
  geom_line(aes(y=RWI_SITE_MEAN, color=SITE)) +
  geom_col(aes(y=P_ANOM_STD/100)) + 
  #scale_color_manual(values=pal) +
  #scale_color_breer(palette='Oranges') + 
  scale_color_npg(labels= c('APL',
                            'GTA',
                            'CRA',
                            'CRB',
                            'BMA')) +
  scale_y_continuous(
    name=bquote('Ring-width Index'),
    sec.axis = sec_axis(~.*100, name=bquote('Precipitation Anomaly' (mm ~y^-1)))) +
  labs(title = 'Ring Width Index by Site vs. Precipitation Anomaly, 1909-2018',
       color = 'Site') +
  xlab('Year') +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        plot.title = element_text(size=20))

# Boxplot: ringwidth by site and precip categorical
crn.p.order <- crn.p %>%
  arrange(value) %>%
  mutate(P_CAT=factor(P_ANOM_CAT, levels=c('Extreme low', 
                                           'Moderate low', 
                                           'Mean conditions', 
                                           'Moderate high',
                                           'Extreme high')))

ggplot(crn.p.order, aes(x=value, y=SP, fill=P_CAT)) +
  geom_boxplot() + 
  scale_fill_brewer(palette='RdYlBu') +
  coord_flip()

#######################
# ANOVA
#########################

crn.p.aov <- aov(value ~ P_ANOM_CAT, data=crn.p)
summary(crn.p.aov)
TukeyHSD(crn.p.aov)
pairwise.t.test(crn.p$value, crn.p$P_ANOM_CAT,
                p.adjust.method = "BH",
                pool.sd=F)

car::leveneTest(value ~ factor(P_ANOM_CAT), data=crn.p)

plot(crn.p.aov,1)
