install.packages("nflseedR")
library(nflseedR)
library(tidyverse)
library(nflreadr)

#find each teams epa/play for 2026 so far
data <- load_pbp(2026) %>%
 filter(wp >= 0.05 & wp <= 0.95 & !is.na(posteam)) %>% 
  group_by(posteam) %>%
  summarise(epa_per_play = mean(epa, na.rm = TRUE)) %>%
  arrange(desc(epa_per_play))

data$posteam 

elo <- read_csv("nflelo.csv") %>% 
select(c(1,3))

head(elo)

joined <- data %>% 
  left_join(elo, by = c("posteam" = "Team")) %>%
  select(c(1,3))

head(joined)
final <- joined %>%
    rename(Team = posteam,
    elo = nfelo)

named_vector <- setNames(final$elo, final$Team)

print(named_vector)

games <- load_schedules(2026) %>%
  filter(game_type == "REG") %>%
  mutate(
    result = NA_integer_,
    away_score = NA_integer_,
    home_score = NA_integer_
  )

sims <- nfl_simulations(
  games = games,
  elo = named_vector,
  simulations = 50000,
  chunks = 20,
  verbosity = "NONE"
)

summary(sims)
