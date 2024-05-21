# Figure 4

#############################
# Set up working environment
#############################

# Load config
config <- config::get(file=file.path('config', 'config.yml'))

# Load local helper functions and packages
devtools::load_all()
load.pkgs(config$pkgs)

#############################
# Ingest data
#############################

# All detected trees within conifer forest mask
## NOTE: THE ZIPPED TAR FILE DOWNLOADED IN THIS CALL IS 4.1 GB.
## MAKE SURE STORAGE AND MEMORY RESOURCES ARE SUFFICIENT BEFORE EXECUTING.
## DOWNLOADING AND UNZIPPING WILL TAKE 5-10 MINUTES
download.file('https://drive.usercontent.google.com/download?id=1AxKmbo2HxKUcuxbk7HXhwitlq6prR_sc&confirm=true',
              destfile=file.path(tempdir(), 'trees.tar.gz'),
              method='wget')
untar(file.path(tempdir(), 'trees.tar.gz'), exdir=file.path(tempdir(), 'trees'))
alltrees <- read.csv(file.path(tempdir(), 'trees', 'trees_masked_5m.csv'))

# Optimal ITD results
download.file('https://drive.google.com/uc?export=download&id=1Vnv4UGjPsVZSW5deBQE_1qjXo2Vq3K5P&usp=drive_fs',
              destfile=file.path(tempdir(), 'optimal_itd.tar.gz'),
              method='wget')
untar(file.path(tempdir(), 'optimal_itd.tar.gz'), exdir=file.path(tempdir(), 'optimal_itd'))
opt.itd <- untar(file.path(tempdir(), 'optimal_itd.tar.gz'), list=T)

## Match data (tabular)
ls.match <- read.csv(file.path(tempdir(), 'optimal_itd', 'opt_matches.csv'))

# Field data
download.file('https://drive.google.com/uc?export=download&id=17V27lVbOqh3dIhHQ24tiJpmpJeurSGy9&usp=drive_fs',
              destfile=file.path(tempdir(), 'EastRiver_Census1_Data_Collated.csv'))
inv <- read.csv(file.path(tempdir(), 'EastRiver_Census1_Data_Collated.csv'))

##########################################################
# Summary stats on all detected trees
##########################################################

alltrees.corx <- alltrees %>%
  mutate(bin = cut(H, breaks=seq(0,max(H),1))) %>%
  group_by(bin) %>%
  summarise(n=n(),
            freq=n()/nrow(.))

lstrees.obs.corx <- ls.match %>%
  filter(src==0) %>%
  mutate(bin = cut(Zobs, breaks=seq(0,max(Zobs),1))) %>%
  group_by(bin) %>%
  summarise(n_obs=sum(!is.na(Zobs)),
            f_obs=sum(!is.na(Zobs))/nrow(.))

lstrees.det.corx <- ls.match %>%
  filter(src==1) %>%
  mutate(bin = cut(Zpred, breaks=seq(0,max(Zpred),1))) %>%
  group_by(bin) %>%
  summarise(n_det=sum(!is.na(Zpred)),
            f_det=sum(!is.na(Zpred))/nrow(.))

lstrees.corx <- full_join(lstrees.obs.corx, lstrees.det.corx, by='bin') %>%
  mutate(pct_diff=f_obs-f_det,
         pct_scale=1+pct_diff)

lstrees.corx.l <- lstrees.corx %>%
  pivot_longer(cols=c(f_obs, f_det))

alltrees.corx <- alltrees.corx %>%
  full_join(lstrees.corx, by='bin') %>%
  mutate(n_corr=n*pct_scale,
         f_corr=n_corr/23915758,
         hbin=1:nrow(.))

alltrees.corx.l <- alltrees.corx %>%
  pivot_longer(cols=c(n, n_corr)) %>%
  mutate(name=ifelse(name=='n', 'Pre-correction', 'Post-correction'))

alltrees.corx.plt <- ggplot(alltrees.corx.l, aes(x=hbin, y=value, group=name, fill=name)) +
  geom_col(width=.8, position=position_dodge(.8)) +
  scale_fill_manual(values=c('#E31A1C', '#FD8D3C'), name=NULL) +
  scale_x_continuous(limits=c(0,40)) +
  labs(x='Height (1 m bins)', y='Number of detected tree crown objects') +
  ggthemes::theme_calc(base_size=8) +
  theme(legend.position = c(1,1),
        legend.justification = c('right', 'top'),
        legend.box.background = element_rect(fill = "white", color = "black"))

#############################
# Write
#############################

cairo_pdf(file.path('inst', 'ms', 'figures', 'Fig4.pdf'),
          width=90/25.4, height=90/25.4, onefile=T,
          family='Arial', bg='white')

print(alltrees.corx.plt)

dev.off()
