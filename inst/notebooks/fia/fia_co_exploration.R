# FIA DATA EXPLORATION

# View the rFIA walkthrough on github: https://github.com/hunter-stanke/rFIA
# More details on the rFIA website: https://rfia.netlify.app

### Download and install packages ###

# NOTE: if prompted, hit ENTER to skip package updates
# devtools::install_github('hunter-stanke/rFIA')
library(rFIA)

### Download FIA data for the state of CO. ###

# You should only need to do this once, and it will download to the path you specify below
fia.path <- file.path('~', 'Desktop', 'FIA_CO')

# `getFIA()` times out after 60 seconds.
# Run options(timeout=3600) to prevent timeout for large downloads.
# Downloading all the CO data takes 5-10 minutes.
options(timeout=3600)
fia.co <- getFIA(states = 'CO', dir = fia.path, common=F)

### Read the downloaded data ###
fia.co <- readFIA(fia.path, common=F)
names(fia.co)

### Process and clean ###

# See all inventory years available
tpa.co <- tpa(fia.co)
head(tpa.co)

# Subset to estimates for most recent inventory year
# fia.co.mr <- clipFIA(fia.co, mostRecent = TRUE) ## subset the most recent data
# fia.co.tpa <- tpa(fia.co.mr)
# head(fia.co.tpa)

# Subset to specific counties
View(fia.co$TREE_GRM_BEGIN)

### Analyze ###

# Group estimates by species
fia.co.sp <- tpa(fia.co.mr, bySpecies = TRUE)
head(fia.co.sp, n = 5)

# Group estimates by size class
# NOTE: Default 2-inch size classes, but you can make your own using makeClasses()
fia.co.sc <- tpa(fia.co.mr, bySizeClass = TRUE)
head(fia.co.sc, n = 5)

# Group by species and size class, and plot the distribution
#  for the most recent inventory year
fia.co.spsc <- tpa(fia.co.mr, bySpecies = TRUE, bySizeClass = TRUE)

plotFIA(fia.co.spsc, BAA, grp = COMMON_NAME, x = sizeClass,
        plot.title = 'Size-class distributions of BAA by species',
        x.lab = 'Size Class (inches)', text.size = .75,
        n.max = 8) # Plots the top 8 species; increase or try n.max = -8 for bottom 8


### SCRATCH ###

# Another way to load locally downloaded data via SQL
# Seems like more work than just using `rFIA`, but a potentially
# more generalizable alternative

# library(RSQLite)
# con <- dbConnect(RSQLite::SQLite(), '/Volumes/GoogleDrive/My Drive/Research/FIA/FIADB_CO.db')
# dbListFields(con, 'COND')
# counties <- dbReadTable(con, 'COUNTY')
# cond <- dbReadTable(con, 'COND')
# gunnison <- cond[(cond$STATECD=8) & (cond$COUNTYCD==51),]
# length(unique(gunnison$PLOT))
# gunnison
