### ITC optimization using LayerStacking

## Workspace setup
## ---------------------------------------------------------------------------------------------------

# Load config
config <- config::get(file=file.path('~',
                                     'Repos',
                                     'er-forest-structure',
                                     'config',
                                     'config.yml'))

# Load local helper functions and packages
source(file.path('~', 'Repos', 'er-forest-structure', 'inst', 'notebooks', 'LiDAR', '05.00_itc_traintest_loadup.R'))


## Define vectors of parameters on which to run algorithm
## ---------------------------------------------------------------------------------------------------

# Optimal parameters
start = 0.5
reso = 0.5
ws1 = 1
ws2 = 2
buf = 0.2

## Run optimization
## ---------------------------------------------------------------------------------------------------
testls <- lapply(lasplots, ls.init, start, reso, ws1, ws2, buf, hmin=1.8)

## Reformat results
## ---------------------------------------------------------------------------------------------------

# Unnest results of algorithm
testls <- unlist(testls, recursive=F)
testls <- lapply(testls, st_as_sf)

# Create vector of ITD run IDs
ls.runid <- expand.grid(names(lasplots), '_p', 1)
ls.runid <- ls.runid[order(ls.runid$Var1),]
ls.runid <- paste(ls.runid[,1],ls.runid[,2], ls.runid[,3], sep='')

# Add run IDs to unnested list of ITD outputs
names(testls) <- ls.runid

# # Filter out 0-length list members
# testls <- Filter(function(x) nrow(x) > 0 , testls)
#
# # Update run IDs after filtering
# ls.runid <- ls.runid[ls.runid %in% names(testls)]

## Bipartite matching
## ---------------------------------------------------------------------------------------------------

### Run matching
ls.match <- lapply(ls.runid,
                     FUN=bipart.match3,
                     lasset=testls,
                     obset=stems.in.plots#,
                     #plotdir=file.path('/global', 'scratch', 'users', 'worsham', 'itc_results', 'figs', 'ls_itc_figs'),
                     #mc.cores = getOption("mc.cores", length(workerNodes)-2)
                     )

# Reformat results
names(ls.match) <- names(lasplots)
ls.match <- data.frame(do.call('rbind', ls.match))
ls.match$site <- unlist(lapply(strsplit(rownames(ls.match), '\\.'), '[', 1))

a <- 0.8849
b <- 0.9102
ls.match$DBH_est <- nthroot(ls.match$Zpred/a, b)

med.ht <- median(ls.match$Zpred, na.rm=T)
p90.ht <- quantile(ls.match$Zpred, .9, na.rm=T)
qmd <- sqrt(mean(ls.match$DBH_est^2, na.rm=T))

ls.match.comp.l <- ls.match %>%
  group_by(site) %>%
  summarise(`Detected median height` = median(Zpred, na.rm=T),
            `Reference median height` = median(Zobs,na.rm=T)) %>%
  pivot_longer(cols=c(`Detected median height`, `Reference median height`))

ls.match.comp.l.90 <- ls.match %>%
  group_by(site) %>%
  summarise(`Detected 90th pctl height` = quantile(Zpred, .9, na.rm=T),
            `Reference 90th pctl height` = quantile(Zobs, .9, na.rm=T)) %>%
  pivot_longer(cols=c(`Detected 90th pctl height`, `Reference 90th pctl height`))


hcomp.colors <- brewer.pal(8, name='Blues')[c(4,8)]
ggplot(ls.match.comp.l.90, aes(x=site, y=value, fill=name)) +
  geom_col(position='dodge') +
  scale_fill_manual(values=hcomp.colors, name='Maximum height') +
  labs(x='Site', y='') +
  ggthemes::theme_calc()

ggplot(ls.match.comp.l, aes(x=site, y=value, fill=name)) +
  geom_col(position='dodge') +
  scale_fill_manual(values=hcomp.colors, name='Median height') +
  labs(x='Site', y='') +
  ggthemes::theme_calc()


# Plot kernel density
head(ls.match)

ls.match.l <- ls.match %>%
  mutate(pairid.site = paste(site, as.character(pair_id), sep='.')) %>%
  pivot_longer(cols=c(treeID, pred),
             names_to='source',
             values_to='treeID') %>%
  arrange(pair_id) %>%
  mutate(src = case_when(source=='pred' ~ 'Detected',
                         T ~ 'Observed')) %>%
  mutate(across(Zobs:Yobs, ~ ifelse(src=='Detected', NA, .)),
         across(Zpred:Ypred, ~ ifelse(src=='Observed', NA, .)),
         Z = coalesce(Zobs, Zpred),
         X = coalesce(Xobs, Xpred),
         Y = coalesce(Yobs, Ypred))

df.matched.plt.l <- ls.match.l %>%
    pivot_longer(cols=c(X,Y,Z),
                 names_to='dim')

kdens.colors <-brewer.pal(8, name='Blues')[c(4,8)]
skill.density <- ggplot(df.matched.plt.l, aes(x=value, group=src, color=factor(src))) +
  geom_density() +
  scale_color_manual(values=kdens.colors, name='Matched Trees') +
  labs(x='Dimensional value', y='Kernel density') +
  facet_wrap(~dim, nrow=3, scales='free') +
  ggthemes::theme_calc()

skill.density
