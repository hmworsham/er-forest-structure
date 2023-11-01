##########
# Table 1
##########

# Descriptions of variables
v.type <- c(rep('RE', 5), rep('EX', 15))
v.name <- c('Total number density', 'QMD',
            'Basal area', '90th percentile height', 'Height skew', 'Elevation',
            'Slope', 'Folded aspect', 'Heat load', 'TPI', 'TWI',
            'AET','CWD','SWE', '∆SWE', 'Available water capacity',
            'Organic matter', 'k', 'Total depth', 'Lithologic substrate')
v.desc <- c('Total number of ITC objects per grid cell',
            'Quadratic mean of stem diameters of objects per grid cell',
            'Sum of cross-sectional areas of stems per hectare',
            'Estimated maximum canopy height per grid cell',
            'Third moment of height distribution per grid cell',
            'Elevation above sea level',
            'dz/dxy computed in a 30 m window',
            'Index of cardinal aspect adjusted for higher incident radiation on SW slopes',
            'Potential heat load calculated according to Eq. 3 in McCune and Keon (2002)',
            'Index of hillslope position (summit, shoulder, backslope, footslope, and toeslope) computed in 1000 m window',
            'Terrain-driven ratio of upslope water supply to local drainage expressed as afunction of local slope and upslope contributing area per unit contour length, computed on a 100 m pixel scale',
            'Actual evapotranspiration: depth of water (mm) evaporated from the surface or transpired by plants per grid cell',
            'Climatic water deficit: difference between potential evapotranspiration (PET) and AET, where PET is the total depth of water that can be evaporated or transpired given prevailing atmospheric conditions',
            'Snow water equivalent derived by forcing iSnobal with 50 m snow depth data from eight Airborne Snow Observatory flights',
            'Relative velocity of snow disappearance; difference between winter SWE and summer SWE divided by days between flights averaged over three flight-years',
            'Amount of plant-available water that can be stored in a unit of soil depth',
            'Amount of decomposed plant and animal residue expressed as a weight percentage of the less than 2 mm soil material',
            'Amount of water that moves vertically through a unit area of soil per unit time under unit hydraulic gradient',
            'Sum of horizon depths in a soil component',
            'Distribution of rock formations'
)

v.cat <- c(rep('Forest structure', 5),
           rep('Topography', 6),
           rep('Climate', 4),
           rep('Soil', 4),
           'Geology'
)

v.unit <- c('stems ha'%p%supsc('-1'), 'cm', 'm'%p%supsc('2'), 'm', 'NA',
            'm', 'degrees', 'index', 'index', 'index', 'index',
            'm', 'm day'%p%supsc('-1'), 'mm', 'mm',
            'mm', '% mass', '\U00B5m sec'%p%supsc('-1'), 'cm',
            'NA')

v.src <- c(rep('NEON LiDAR', 5),
           rep('NEON LiDAR', 6),
           rep('BCM (Budo et al. 2017)', 2), rep('ASO', 2),
           rep('SSURGO', 4),
           'Colorado Geological Survey')

tbl1 <- data.frame(cbind('Type'=v.type, 'Category'=v.cat,
                            'Variable'=v.name, 'Description'=v.desc,
                            'Units'=v.unit, 'Source'=v.src))

###########
# Table 2
##########

i.meas <- c('Species', 'Stem height', 'DBH', 'Stem geolocation',
            'Crown illumination', 'Canopy position', 'Life status', 'Health status')
i.unit <- c(NA, 'm', 'cm', 'decimal degrees',
            'unitless index', 'unitless index', NA, NA)
i.method <- c('Visual identification', 'Nikon Forestry Pro II hypsometer or metric tape',
              'Diameter tape or calipers', 'decimal degrees', 'Trimble GEO-7X GNSS unit',
              'Visual determination', 'Visual determination', 'Visual inspection for death',
              'Visual inspection for infection, damage, decay, browning, or wilting')

tbl2 <- data.frame(cbind('Measurement'=i.meas, 'Units'=i.unit, 'Method'=i.method))


###########
# Table 3
##########
crit.height <- c('Z \U2264 10 m', '10 m \U003C Z \U2264 15 m',
                 '15 m \U003C Z \U2264 25 m', 'Z > 25 m')
crit.dxy <- c('∆Z \U003C 3 m', '∆Z \U003C 3 m', '∆Z \U003C 4 m', '∆Z \U003C 4 m' )
crit.dz <- c('∆XY \U003C 3 m', '∆XY \U003C 4 m', '∆XY \U003C 5 m', '∆XY \U003C 5 m')

tbl3 <- data.frame(cbind('Tree height (Z)'=crit.height,
                         '∆Z criterion'=crit.dxy,
                         '∆Y criterion'=crit.dz),
                   check.names=F)


##########
# Table 4
##########

ls.id <- c('\U03BB'%p%subsc('1'), '\U03BB'%p%subsc('2'),
           '\U03BB'%p%subsc('3'), '\U03BB'%p%subsc('4'),
           '\U03BB'%p%subsc('5'), '\U03BB'%p%subsc('6'),
           '\U03BB'%p%subsc('7'))
ls.param <- c('`start`', '`resolution`', '`window1`', '`buffer`',
              '`hardwood`', '`window2`', '`hmin`')
ls.desc <- c('Starting height above ground at which layer divisions begin',
             'Resolution of the CHM',
             'Window radius for the first local maximum filter for detecting tree tops',
             'Size of the buffer enforced around each point to create a polygonal cluster',
             'Logical switch, where False adds weight to clusters to account for mid-canopy density in conifer stands',
             'Window radius for the second local maximum filter for detecting tree tops',
             'Minimum height threshold, below which a new tree cannot be initiated')
ls.opt.val <- c(0.5, 0.5, 1.2, 0.5, F, 2.0, 1.3)

##########
# Table 5
##########

tbl5 <- data.frame(cbind('ID'=ls.id, 'Parameter'=ls.param, 'Description'=ls.desc,
                         'Optimal value'=ls.opt.val),
                   check.names=F)

##########
# Table 6
##########
gbm.perf.df <- read.csv(file.path(config$data$pro, 'gbm_perf_df.csv'),
                        check.names=F)
gam.perf.df <- read.csv(file.path(config$data$pro, 'gam_perf_df.csv'),
                                  check.names=F)

tbl6 <- gbm.perf.df %>%
  left_join(gam.perf.df, by='Response') %>%
  rename(`GBM CV error`='CV error',
         `GBM test error`='Test error.x',
         `GAM % TDE`='PDE',
         `GAM test error`='Test error.y'
         ) %>%
  mutate(across(`GBM CV error`:`GAM test error`, \(x) round(x,2)))

