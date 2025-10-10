library(nflreadr)
library(nflfastR)
library(tidyverse)
library(nflplotR)
nflreadr::.clear_cache()
pbp <- load_pbp(2025)
view(pbp)

#red zone success rate by team
red_zone <- pbp %>% 
  filter(!is.na(epa),
         !is.na(posteam),
            !play_type %in% c("no_play", "extra_point"),
         yardline_100 <= 19) %>%
  group_by(posteam, game_id, fixed_drive) %>%
  summarise(td = any(pass_touchdown == 1  | rush_touchdown == 1), .groups = "drop") %>%
  group_by(posteam) %>%
  summarise(
    n = n(),                         # number of red zone drives
    td = sum(td),                     # drives ending in TD
    success_rate = td / n, 
    .groups = "drop"
  ) %>%
  arrange(desc(success_rate))
view(red_zone)
