######################
# Load packages
######################
# Load libraries
library(readxl)
library(tidyverse)
library(ggplot2)
library(data.table)
library(dplR)
library(devtools)
library(dplyr)

# Source local functions
source(file.path('.', 'eastriver', 'dendrochronology', 'dendro.helpers.R'))

######################
# Set up environment
######################

# Define directory of ring-width data
dendrodir <- file.path('/Volumes', 'GoogleDrive', 'My Drive', 'Research', 'RMBL', 'RMBL-East River Watershed Forest Data', 'Data', 'Dendrochronology')
rwdir <- file.path(dendrodir, 'Ring_Widths')
rwldir <- file.path(dendrodir, 'rwl')

######################
# Ingest data 
######################

# Ingest master dendro index
di.path <- file.path(dendrodir, 'Dendro_Core_Index.xlsx')
dendroindex <- read_excel(
  di.path, 
  sheet='Cores', 
  col_types=c(
    'text', 
    'text', 
    'text', 
    'numeric', 
    'numeric', 
    'text', 
    'numeric', 
    'numeric', 
    rep('guess', 17)))

dendroindex <- dendroindex[which(rowSums(is.na(dendroindex))<20),]

# Ingest master site name index
siteindex <- read_excel(di.path, sheet='Sites', col_types=c('numeric', 'text', 'text', 'text', 'numeric', rep('guess', 9)))

# # Create name key from site index
# namekey <- unique(data.frame('plotid'=siteindex$Plot_ID, 'siteid'=siteindex$ITRDB_SiteID))
# namekey <- namekey[order(namekey$plotid),]
# rownames(namekey) <- NULL

# Ingest ring-width data from csv files
rwfiles <- list.files(rwdir, full.names = T, pattern='[*[:digit:]].csv$')
ringwidths <- lapply(rwfiles, read.csv, header=T)
names(ringwidths) <- lapply(strsplit(rwfiles, '/|_'), '[', 12)

# Check that dimensions of ringwidth arrays are correct
all(sapply(ringwidths, function(x) dim(x)[2]==6))

######################################
# Convert to rwl and write to tucson
######################################

# List sites and species to process
sites <-  c('CRB', 'GTA', 'CRA', 'APL')
spps <- c('ABLA', 'PIEN')
names(sites) <- sites
names(spps) <- spps

# Restructure ringwidth dataframes
rw <- restructure.rw(ringwidths, dendroindex)

# Split dataframes by site and species
sss = outer(sites, spps, split.site.spp)

# Create rwl and write out to tucson format
rwl <- generate.rwl(sites, spps, sss, dendroindex, outdir=rwldir)

# Check it worked
ts <- read.tucson(file.path(rwldir, 'CRA_PIEN_dated.rwl'))
spag.plot(ts, zfac=0.5)

################################################
# Plotting, detrending, etc.
################################################

# For one series
gta.p <- read.tucson(file.path(rwldir, 'GTA_PIEN_dated.rwl'))
cd <- corr.rwl.seg(gta.p, seg.length=30, pcrit=0.05, bin.floor=10)
spag.plot(gta.p, zfac=0.5)

re <- rwl.report(gta.p)
detrend.series(gta.p$GTA0004A, method='ModNegExp')
dt <- dplR::detrend(gta.p, method='ModNegExp')
crn <- chron(dt)
interseries.cor(gta.p)


# For all sites and series
rwfiles <- list.files(rwldir, pattern='_dated', full.names=T)
rws <- lapply(rwfiles, read.tucson)
ids <- c('APL_ABLA', 'APL_PIEN', 'GTA_ABLA', 'GTA_PIEN')
dt <- lapply(rws, dplR::detrend, method='ModNegExp')
crn <- lapply(dt, chron)

for(i in seq(length(crn))){
  #crn[[i]]$id = ids[i]
  crn[[i]]$YEAR = as.numeric(rownames(crn[[i]]))
}

# Bind
crn.df <- do.call('rbind', crn)
crn.df <- data.frame('YEAR'=seq(as.numeric(min(rownames(crn.df))), 2019))

for(i in crn){
  crn.df = left_join(crn.df, i, by = 'YEAR')
}

names(crn.df) <- c('YEAR', unlist(lapply(ids, paste, c('STDDEV', 'SAMPLE_DEPTH'), sep='_')))

crn.df <- select(crn.df, matches('YEAR|STDDEV'))

# Ingest climate data

clim <- read.csv(climfiles[3])
precip <- clim[,c('DATE', 'PRCP')]
precip$DATE <- as.POSIXct(precip$DATE)
precip <-  precip%>% 
  mutate(YEAR=as.numeric(format(precip$DATE, '%Y'))) %>%
  group_by(YEAR) %>%
  summarise(ANN_P = sum(PRCP, na.rm=T)) %>%
  mutate(P_MEAN = mean(ANN_P),
         P_ANOM = ANN_P-P_MEAN, 
         P_ANOM_NEG = P_ANOM < 0)

p.anom.neg <- precip[c('YEAR', 'P_ANOM')]
p.anom.neg[p.anom.neg$P_ANOM >=0, 'P_ANOM'] <- 0
p.anom.neg$P_ANOM_STD <- scale(p.anom.neg$P_ANOM)

p.an.rep <- p.anom.neg[rep(seq_len(nrow(p.anom.neg)), 4), ]
p.an.rep$SITE <- sort(rep(ids, 114))

# Merge precip and chronologies
crn.df.long <- melt(crn.df, id=c('YEAR'))
crn.p <- merge(crn.df.long, p.anom.neg, on='YEAR')
crn.p
crn.p$SITE <- unlist(lapply(str_split(crn.p$variable, '_'), '[', 1))
crn.p$SP <- unlist(lapply(str_split(crn.p$variable, '_'), '[', 2))
head(crn.p)

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

# Cross correlation
crn.p.wide <- inner_join(crn.df, precip, on='YEAR')
crosscorr.AA <- ccf(crn.p.wide$P_ANOM, crn.p.wide$APL_ABLA_STDDEV)
crosscorr.AP <- ccf(crn.p.wide$P_ANOM, crn.p.wide$APL_PIEN_STDDEV)
crosscorr.GA <- ccf(crn.p.wide$P_ANOM, crn.p.wide$GTA_ABLA_STDDEV)
crosscorr.GP <- ccf(crn.p.wide$P_ANOM, crn.p.wide$GTA_PIEN_STDDEV)
crosscorr.GP
crn.p.wide$P_LAG3 <- lag(crn.p.wide$P_ANOM, 3)
crn.p.wide$P_LAG4 <- lag(crn.p.wide$P_ANOM, 4)
crn.p.wide$P_LAG5 <- lag(crn.p.wide$P_ANOM, 5)
crn.p.wide$P_LAG6 <- lag(crn.p.wide$P_ANOM, 6)

df <- crn.p.wide

pacf(crn.p.wide$P_ANOM, lag.max=25)
detrend <- residuals(lm(crn.p.wide$P_ANOM ~ c(1:length(crn.p.wide$P_ANOM))))

ggplot(df, aes(y=APL_ABLA_STDDEV)) +
  geom_point(aes(x=P_ANOM), color='grey10') +
  geom_point(aes(x=P_LAG3), color='blue') + 
  geom_point(aes(x=P_LAG4), color='green') + 
  geom_point(aes(x=P_LAG5), color='red') + 
  geom_point(aes(x=P_LAG6), color='pink') + 
  geom_smooth(method='lm', aes(P_ANOM, APL_ABLA_STDDEV)) + 
  geom_smooth(method='lm', aes(P_LAG3, APL_ABLA_STDDEV)) + 
  geom_smooth(method='lm', aes(P_LAG4, APL_ABLA_STDDEV)) + 
  geom_smooth(method='lm', aes(P_LAG5, APL_ABLA_STDDEV)) + 
  geom_smooth(method='lm', aes(P_LAG6, APL_ABLA_STDDEV))
  
lml.AA <- tslm(ts(crn.p.wide$APL_ABLA_STDDEV) ~ ts(crn.p.wide$P_LAG5))
lml.AP <- lm(crn.p.wide$APL_PIEN_STDDEV ~ crn.p.wide$P_LAG6)
lml.GA <- lm(crn.p.wide$GTA_ABLA_STDDEV ~ crn.p.wide$P_LAG3)
lml.GP <- lm(crn.p.wide$GTA_PIEN_STDDEV ~ crn.p.wide$P_LAG6)
summary(lml.AA)
summary(lml.AP)
summary(lml.GA)
summary(lml.GP)
summary(lml1)
residuals(lml1)

# Plots
ggplot(crn.p, aes(x=YEAR)) + 
  geom_line(aes(y=value, color=variable)) +
  #facet_grid(vars(variable)) +
  geom_col(aes(y=P_ANOM/1000)) + 
  scale_y_continuous(
    name=bquote('Ring-width Index (unitless)'),
    sec.axis = sec_axis(~.*1000, name=bquote('Precipitation Anomaly' (mm ~d^-1))))

pal <- wes_palette("Darjeeling1", n=5, type = "discrete")
ggplot(crn.p.sp.means, aes(x=YEAR)) + 
  #facet_wrap(vars(SP)) +
  geom_line(aes(y=RWI_SP_MEAN, color=SP)) +
  geom_col(aes(y=P_ANOM/1000)) + 
  #scale_color_manual(values=pal) +
  #scale_color_breer(palette='Oranges') + 
  scale_color_jco(labels= c('ABLA',
                            'PIEN')) +
  scale_y_continuous(
    name=bquote('Ring-width Index'),
    sec.axis = sec_axis(~.*1000, name=bquote('Precipitation Anomaly' (mm ~y^-1)))) +
  labs(title = 'Ring Width Index by Spp. vs. Precipitation Anomaly, 1909-2018',
       color = 'Species') +
  xlab('Year') +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        plot.title = element_text(size=20))

ggplot(crn.p.si.means, aes(x=YEAR)) + 
  #facet_wrap(vars(SP)) +
  geom_line(aes(y=RWI_SITE_MEAN, color=SITE)) +
  geom_col(aes(y=P_ANOM/1000)) + 
  #scale_color_manual(values=pal) +
  #scale_color_breer(palette='Oranges') + 
  scale_color_npg(labels= c('APL',
                            'GTA')) +
  scale_y_continuous(
    name=bquote('Ring-width Index'),
    sec.axis = sec_axis(~.*1000, name=bquote('Precipitation Anomaly' (mm ~y^-1)))) +
  labs(title = 'Ring Width Index by Site vs. Precipitation Anomaly, 1909-2018',
       color = 'Site') +
  xlab('Year') +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        plot.title = element_text(size=20))
