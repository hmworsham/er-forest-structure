library(RSQLite)

con <- dbConnect(RSQLite::SQLite(), '~/Downloads/FIADB_CO.db')

dbListFields(con, 'COND')

counties <- dbReadTable(con, 'COUNTY')
cond <- dbReadTable(con, 'COND')

gunnison <- cond[(cond$STATECD=8) & (cond$COUNTYCD==51),]

length(unique(gunnison$PLOT))
       