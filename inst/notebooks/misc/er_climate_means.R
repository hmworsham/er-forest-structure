
cbtmp <- read.csv('/Users/hmworsham/Repos/er/er-dendrochronology/data/intermediate/noaa_climate_means.csv')

library(dplyr)
cbtmp %>%
  filter(Date>as.Date('1980-10-01')) %>%
  summarise(mean=mean(Temp_C_AVG_ITP))

cbmax <- cbtmp %>%
  filter(Date>as.Date('1980-10-01')) %>%
  group_by(Month) %>%
  summarise(maxmon=mean(Temp_C_MAX_ITP))

mean(cbmax$maxmon)
mean(cbmin$minmon)
rcbmin <- cbtmp %>%
  filter(Date>as.Date('1980-10-01')) %>%
  group_by(Month) %>%
  summarise(minmon=mean(Temp_C_MIN_ITP))

  summarise(c_across(Temp_C_AVG_ITP, Temp_C_MAX_ITP, Temp_C_MIN_ITP, ~\(x) mean(x, na.rm=T)))

cbtmp
