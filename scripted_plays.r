library(nflreadr)
library(nflfastR)
library(tidyverse)
library(nflplotR)
nflreadr::.clear_cache()
pbp <- load_pbp(2025)
view(pbp)

colors <- nflfastR::teams_colors_logos

#team average epa per play on first 15 plays of game vs rest of game

first15 <- pbp %>% filter(!is.na(epa),
                          !is.na(posteam),
                          play_type %in% c("pass", "run"),
                          qtr <= 4) %>%
  group_by(game_id, posteam) %>%
  mutate(play_num = row_number()) %>%
  ungroup() %>%
  mutate(period = ifelse(play_num <= 15, "first_15", "Rest_of_game")) %>%
  group_by(posteam, period) %>%
  summarise(n = n(),
            epa = mean(epa),
            .groups = "drop") %>%
  arrange(posteam, period) %>%
  pivot_wider(names_from = period,
              values_from = c(n, epa)) %>%
  select(posteam,
         n_First_15_plays = n_first_15,
         epa_First_15_plays = epa_first_15,
         n_Rest_of_game = n_Rest_of_game,
         epa_Rest_of_game = epa_Rest_of_game) %>%
  arrange(desc(epa_First_15_plays))
view(first15)

##plot first 15 vs rest of game epa
first15 %>%
  ggplot(aes(x = epa_Rest_of_game, y = epa_First_15_plays)) +
  geom_hline(yintercept = 0, lty = 2, color = "grey50") +
  geom_vline(xintercept = 0, lty = 2, color = "grey50") +
  geom_abline(slope = 1, intercept = 0, lty = 2, color = "red") +
  geom_nfl_logos(aes(team_abbr = posteam), width = .08) +
  scale_x_continuous(name = "EPA/play - Rest of game") +
  scale_y_continuous(name = "EPA/play - First 15 plays") +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold", hjust = .05, size = 25),
        plot.caption = element_text(hjust = 0.5, size = 15), 
        axis.text.y = element_text(size = 15), 
        axis.text.x = element_text(size = 15)) +
  labs(title = "Scripted Plays: EPA/Play in First 15 Plays vs Rest of Game",
       caption = "Data: @nflfastR | Plot: Ward Walton")

