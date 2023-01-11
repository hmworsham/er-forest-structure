## This is script to run preliminary seasonal climate response analysis (seascorr) of potential tree-ring work
# and to guide decisions in preparation for field work. 
# Becky Brice, June 2021

rm(list=ls())
library(corrplot)
library(treeclim)
library(dplR)
library(dplyr)
library(burnr)
library(tidyr)

## Read in data. Tree = chronologies, ppt = precipitation data (CRU; 1901-2019), temp = temperature data (CRU; 1901-2019)
#tree <- read.csv("crns.csv",row.names=1)
tree <- read.csv("crns.csv")
precip <- read.csv("precip.csv")
temp <- read.csv("temp.csv") 

## IF NEEDED! Transpose and stack 13-column data to 3-column, Year - month - climate variable
p2 <- precip %>%
  pivot_longer(
    cols = starts_with("X"), names_to = "month", names_prefix ="X", values_to = "prec")
# write.csv(p2, file="precip")
t2 <- temp %>%
  pivot_longer(
    cols = starts_with("X"), names_to = "month", names_prefix ="X", values_to = "temp")

# add reorg precip data to the temparature dataframe for a climate dataframe
climate <- cbind(t2, p2)
# remove duplicate year and month columns
climate <- climate %>% 
  select(-4, -5) 

## subset each crn for individual crn runs in dcc and in seascorr
# example: chrono <- as.data.frame(tree$CO633)
# change row names (critical step)
# example: rownames(chrono) <- tree$year

## Run Seascorr in treeclim for columbine
sc<- seascorr(
  chrono,
  climate,
  var_names = NULL,
  timespan = NULL,
  complete = 9,
  season_lengths = c(1, 3, 6),
  primary = 1,
  secondary = 2,
  ci = 0.05
)
sc
plot(sc)