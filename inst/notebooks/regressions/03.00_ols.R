# Script for preparing datasets for modeling work

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

source(file.path('~', 'Repos', 'er', 'er-forest-structure', 'inst', 'notebooks', 'regressions', '01.00_stats_ingest_data.R'))

#############################
# Ordinary least squares
#############################
mod_lm <- lm(density ~
               elevation
             +slope
             +folded_aspect_205
             +tpi_1km
             +twi_100m
             +awc
             +om
             #+k
             +factor(geology)
             +aet
             +cwd,
             data=vars)

library(car)
avPlots(mod_lm)

