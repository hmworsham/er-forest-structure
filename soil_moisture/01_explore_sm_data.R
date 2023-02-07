library(dplyr)
library(readxl)
library(ggplot2)
library(stringr)
library(data.table)

# Define paths
erdir <- file.path('/Volumes', 'GoogleDrive', 'My Drive', 'Research', 'RMBL', 'RMBL-East River Watershed Forest Data', 'Data')
smdir <- file.path(erdir, 'Sensors', 'Soil_VWC-T-EC')
climdir <- file.path(erdir, 'Climate', 'NOAA_CDO')

# List soil moisture data input files
smfiles <- list.files(smdir, full.names=T, recursive=T, pattern='.xlsx')
climfiles <- list.files(climdir, full.names=T)

# Pull column names from raw tables and refactor
cnames <- names(read_excel(smfiles[1], sheet=1, range = cell_rows(3)))
vars <- rep(c('VWC_', 'T_', 'EC_'), 6)
pits <- sort(rep(c('A', 'B'), 9))
deps <- rep(sort(rep(c('10', '30', '60'),3)), 2)
vars <- paste0(vars, pits, deps)
cnames <- c('DateTime', vars, 'Batt_pct', 'Batt_vltg', 'Atm_press', 'Logger_temp')

smdata <- lapply(smfiles, read_excel, sheet=1, skip=3, col_names=cnames)

#Get site names and apply as names of list elements
sites <- unlist(lapply(str_split(smfiles, '/'), '[', 11))
names(smdata) <- sites

# Split list of dataframes by site and rbind those of same site
smdata.by.site <- split(smdata, names(smdata))
smdata.by.site <- lapply(smdata.by.site, rbindlist)

# Fix SG-SWR1 colnames, as the depths are different
colnames(smdata.by.site$`SG-SWR1`)[11:13] <- c("VWC_A80", "T_A80", "EC_A80" )

# Add a column to each resulting dataframe with the site name
for(i in 1:length(smdata.by.site)){
  nm = names(smdata.by.site)[[i]]
  smdata.by.site[[i]][smdata.by.site[[i]]==0] <- NA
  if('VWC_B10' %in% colnames(smdata.by.site[[i]])) {
    smdata.by.site[[i]]$VWC_mean10 = rowMeans(smdata.by.site[[i]][,c('VWC_A10', 'VWC_B10')], na.rm=T) # mean10
  } else {
    smdata.by.site[[i]]$VWC_mean10 = smdata.by.site[[i]]$VWC_A10
    }
    smdata.by.site[[i]]$VWC_mean30 = rowMeans(smdata.by.site[[i]][,c('VWC_A30', 'VWC_B30')], na.rm=T) # mean30
    smdata.by.site[[i]]$VWC_mean60 = rowMeans(smdata.by.site[[i]][,c('VWC_A60', 'VWC_B60')], na.rm=T) # mean60
    smdata.by.site[[i]]$Site = nm # name
}

# Ingest climate data
clim <- read.csv(climfiles[2])
precip <- clim[(clim$DATE>=as.Date('2021-06-01')) & (clim$DATE<=as.Date('2021-12-30')),]
precip <- precip[,c('DATE', 'PRCP')]
names(precip) <- c('DateTime', 'Precip')
p2 <- precip[rep(seq_len(nrow(precip)), each = 48), ]
p2$DateTime <- seq.POSIXt(ISOdate(2021,06,01), by='30 min', length.out=nrow(p2))
p2 <- p2[rep(seq_len(nrow(p2)), 10), ]
p2$Site <- sort(rep(unique(sites), 10224))

# Rbind everything to have all data together
sm <- rbindlist(smdata.by.site, fill=T)
sm.vwc <- select(sm, matches('DateTime|VWC_mean|Site'))
sm.vwc <- sm.vwc[!duplicated(sm.vwc)]

# Melt to get a tidy long dataframe
sm1.vwc <- melt(sm.vwc, id=c('DateTime', 'Site'))
#sm.p.long <- melt(sm.precip, id=c('DateTime', 'Site'))
sm.p.long <- sm1.vwc
# Merge with precip data
sm.p.long <- merge(sm1.vwc, p2, by=c('DateTime', 'Site'))

# Subset to date range
sm.sub <- sm.p.long[
  (sm.p.long$DateTime > as.POSIXct('2022-05-01')) 
  & sm.p.long$DateTime < as.POSIXct('2022-10-31'),]
sm.sub$DateTime <- as.POSIXct(sm.sub$DateTime)

# Take out problematic sites
sm.sub <- sm.sub[!sm.sub$Site %in% c('CC-CVS1', 'XX-CAR3'),]

# Calculate daily mean vwc
sm.sub <- sm.sub %>%
  mutate(day=format(DateTime, "%d"),
         month = format(DateTime, "%m"), 
         year = format(DateTime, "%Y"), 
         )

sm.sub.vwc.d <- sm.sub %>%
  group_by(Site, variable, day, month, year) %>%
  summarise(value = mean(value, na.rm=T)) %>%
  mutate(DateTime = as.POSIXct(make_date(year, month, day)))

# Calculate monthly use
sm.sub.mon.use <- sm.sub %>%
  group_by(Site, variable, day, month, year) %>%
  summarise(value = (max(value, na.rm=T)-min(value, na.rm=T))*60) %>%
  mutate(DateTime = as.POSIXct(make_date(year,month,day))) %>%
  #ungroup() %>%
  #group_by(Site, day, month, year) %>%
  #summarise(value=sum(value, na.rm=T)) %>%
  ungroup() %>%
  group_by(Site, variable, month, year) %>%
  summarise(value = mean(value, na.rm=T))

View(sm.sub.mon.use)

# Calculate HR
sm.sub.mon.hr <- sm.sub %>%
  group_by(Site, variable, day, month, year) %>%
  summarise(min_d0=min(value, na.rm=T),
            max_d0=max(value,na.rm=T)) %>%
  ungroup()
sm.sub.mon.hr$max_d1=lead(sm.sub.mon.hr$max_d0, 1)

sm.sub.mon.hr <- sm.sub.mon.hr %>%
  mutate(dd0 = max_d1-min_d0) %>%
  ungroup() %>%
  group_by(Site, month, year) %>%
  summarise(hr = dd0*60) %>%
  ungroup() %>%
  group_by(Site, month, year) %>%
  summarise(hr_m = mean(hr, na.rm=T))

View(sm.sub.mon.hr)

suntimes <- function(sd, ed, lat, lon){
  sunlight_times <- getSunlightTimes(as.Date(sd), lat=lat, lon=lon)
  return(sunlight_times)
}

suntimes(sm.sub$DateTime[1], sm.sub$DateTime[1], 38.916667, -106.933333)
View(sm.sub)
sunlight_times <- getSunlightTimes(as.Date(st), lat = lat, lon = lon)
sunset <- sunlight_times$sunset
sunrise <- sunlight_times$sunrise
sec_night <- sec_day <- 0
View(sm1.vwc.augoct)

# Plot with sites as facets
my_colors <- RColorBrewer::brewer.pal(8, "Blues")[c(3,5,7)]
ggplot(sm.sub.vwc.d, aes(x=DateTime)) +
  #geom_line(aes(y=Precip/100), color='grey30') + 
  geom_line(aes(y=value, color=variable), size = 0.6) +
  scale_color_manual(values = my_colors,
                     name = 'Depth',
                     labels = c('10 cm',
                                '30 cm', 
                                '60 cm')) +
  #scale_color_brewer(palette='Greens') +
  scale_y_continuous(
    name=bquote('Volumetric Soil Moisture ' (m^3 ~m^-3)),
    #sec.axis = sec_axis(~.*100, name=bquote('Precipitation' (mm ~d^-1)))
    ) +
  scale_x_datetime(date_breaks='15 days',
                   limits=c(as.POSIXct('2022-05-01'), NA)) +
  labs(title = 'Soil Volumetric Water Content at 10 Mixed Conifer Sites, May 2022 - Sep 2022',
       color = 'VWC at Depth') +
  xlab('Date') +
  facet_wrap(~Site, ncol=4) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        plot.title = element_text(size=20))


ggplot(sm.sub.mon.use, aes(x=month, y=value, fill=Site)) + 
  geom_col(position='dodge') + 
  scale_fill_manual(values = my_colors,
                    name = 'Depth',
                    labels = c('10 cm',
                               '30 cm', 
                               '60 cm')) +
  xlab('Month') +
  scale_y_continuous(name=bquote('Volumetric Soil Moisture ' (m^3 ~m^-3)),)
  

ggplot(sm.sub.mon.use, aes(x=month, y=value, fill=variable)) + 
  geom_col(position='dodge') +
  scale_fill_manual(values = my_colors,
                     name = 'Depth',
                     labels = c('10 cm',
                                '30 cm', 
                                '60 cm')) +
  xlab('Month') +
  scale_y_continuous(name=bquote('Soil Water Depletion ' (mm ~day^-1)),) +
  facet_wrap(~Site, ncol=4) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        plot.title = element_text(size=20))


ggplot(sm.sub.mon.hr, aes(x=month, y=hr_m, group=Site, color=Site)) + 
  geom_point() + 
  geom_smooth()
  scale_fill_manual(values = my_colors,
                    name = 'Depth',
                    labels = c('10 cm',
                               '30 cm', 
                               '60 cm')) +
  xlab('Month') +
  scale_y_continuous(name=bquote('Volumetric Soil Moisture ' (m^3 ~m^-3)),) #+
  facet_wrap(~Site, ncol=4) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        plot.title = element_text(size=20))
