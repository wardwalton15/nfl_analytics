library(nflplotR)
library(tidyverse)
library(nflfastR)
library(gt)

install.packages("nflreadr")
library(nflreadr)

#which teams generated the most pressure in 2024?

pbp_2024 <- load_pbp(2024)

pfr <- load_pfr_advstats(2024)
summary(pfr)

