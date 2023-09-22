# Correlation matrix
# Find correlations between variables

source(file.path('~', 'Repos', 'er-forest-structure', 'inst', 'notebooks', 'regressions', '01.00_stats_ingest_data.R'))

#############################
# Correlation matrix
#############################
corvars <- vars[,(!names(vars) %in% c('density', 'height', 'diam', 'ba', 'geology'))]
corvars <- na.omit(corvars)
corrmat <- cor(corvars)
par(mfcol=c(1,1))
corrplot(corrmat,
         method='number',
         type = 'upper',
         bg= 'grey90',
         tl.col = "black",
         diag=F,
         tl.pos='td')

pairs.panels(corrmat,
             smooth = F,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "kendall", # Correlation method (also "spearman" or "kendall")
             pch = '.',           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals
