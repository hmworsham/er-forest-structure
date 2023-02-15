
library(tidyverse)

setwd('~/Downloads')
scho_plots <- read.csv('Demography_plots_Scho_all_final.csv')
View(scho_plots)

scho_plots_tidy <- scho_plots %>%
#  extract(Comments, into = 'Status', remove = F) %>%
  mutate(Status = case_when(grepl('live', Comments, ignore.case = T) == T |
                              grepl('sappl', Comments, ignore.case = T) == T ~ 'Live',
                            grepl('dead', Comments, ignore.case = T) == T ~ 'Dead',
                            grepl('broken', Comments, ignore.case = T) == T ~ 'Broken'),
    Sapling = grepl('sapp', Comments, ignore.case = T)) %>%
  select(c(1:4,8,5:6,9,7)) %>%
  rename(Tree_ID = Tree_.,
         Tree_Tag = Tree_Tag..)

ggplot(scho_plots_tidy, aes(x = DBH_CM, y = Tree_Height_M, col = Status)) +
  geom_point(shape = 1) +
  scale_color_brewer(type = 'qual', palette = 7)
?scale_color_brewer
View(scho_plots_tidy)

rlang::last_error()
nchar(grep('Live', scho_plots$Comments, value = T))
?grep
