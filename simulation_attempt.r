library(tidyverse)
library(nflfastR)
library(nflreadr)
install.packages("nflseedR")
library(nflseedR)

set.seed(15)
sims <- simulate_nfl(
  nfl_season = 2025,
  fresh_season = TRUE,
  simulations = 100
)
