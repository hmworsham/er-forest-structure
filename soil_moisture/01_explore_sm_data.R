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
  if('VWC_B10' %in% colnames(smdata.by.site[[i]])) {
    smdata.by.site[[i]]$VWC_mean10 = rowMeans(smdata.by.site[[i]][,c('VWC_A10', 'VWC_B10')]) # mean10
  } else {
    smdata.by.site[[i]]$VWC_mean10 = smdata.by.site[[i]]$VWC_A10
    }
    smdata.by.site[[i]]$VWC_mean30 = rowMeans(smdata.by.site[[i]][,c('VWC_A30', 'VWC_B30')]) # mean30
    smdata.by.site[[i]]$VWC_mean60 = rowMeans(smdata.by.site[[i]][,c('VWC_A60', 'VWC_B60')]) # mean60
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

# Subset to one month
sm1.vwc.augoct <- sm.p.long[
  (sm.p.long$DateTime > as.POSIXct('2021-10-15')) 
  & sm.p.long$DateTime < as.POSIXct('2022-09-15'),]
sm1.vwc.augoct$DateTime <- as.POSIXct(sm1.vwc.augoct$DateTime)

# Plot with sites as facets
my_colors <- RColorBrewer::brewer.pal(8, "Blues")[c(3,5,7)]
ggplot(sm1.vwc.augoct, aes(x=DateTime)) +
  #geom_line(aes(y=Precip/100), color='grey30') + 
  geom_line(aes(y=value, color=variable), size = 0.6) +
  scale_color_manual(values = my_colors,
                     labels = c('10 cm',
                                '30 cm', 
                                '60 cm')) +
  #scale_color_brewer(palette='Greens') +
  scale_y_continuous(
    name=bquote('Volumetric Soil Moisture' (m^3 ~m^-3)),
    #sec.axis = sec_axis(~.*100, name=bquote('Precipitation' (mm ~d^-1)))
    ) +
  scale_x_datetime(date_breaks='30 days',
                   limits=c(as.POSIXct('2021-10-15'), NA)) +
  labs(title = 'Soil Volumetric Water Content at 10 Mixed Conifer Sites, Oct 2021 - Sep 2022',
       color = 'VWC at Depth') +
  xlab('Date') +
  facet_wrap(~Site, ncol=5) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        plot.title = element_text(size=20))
