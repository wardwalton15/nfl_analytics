library(nflreadr)
library(nflfastR)
library(tidyverse)
library(nflplotR)
nflreadr::.clear_cache()
pbp <- load_pbp(2025)
library(gt)
##which qb scrambled the most
scrambles <- pbp %>%
  filter(qb_dropback == 1 & qb_scramble == 1) %>%
  group_by(rusher_player_name) %>%
  summarize(scrambles = n(),
            epa = mean(epa)) %>%
  arrange(desc(scrambles)) %>%
  ungroup()


dropbacks <- pbp %>%
  filter(qb_dropback == 1) %>%
  group_by(passer_player_name, passer_player_id) %>%
  summarize(dropbacks = n()) %>%
  arrange(desc(dropbacks)) %>%
  ungroup()

dropbacks <- scrambles %>%
  left_join(dropbacks, by = c("rusher_player_name" = "passer_player_name")) %>%
  mutate(scramble_rate = scrambles / (dropbacks), 
         total_plays = scrambles + dropbacks)

view(dropbacks)

#scramble rate vs epa
dropbacks %>%
  filter(total_plays >= 25) %>%
  ggplot(aes(x = scramble_rate, y = epa, label = rusher_player_name)) +
  geom_nfl_headshots(aes(player_gsis = passer_player_id), width = .11) +
  geom_text(nudge_y = -0.14, size = 7) +
  theme_minimal() +
  labs(title = "Quarterback Scramble Rate vs Scramble EPA/Play",
       subtitle = "Minimum 20 Dropbacks",
       x = "Scramble Rate",
       y = "EPA per Scramble",
       caption = "Data: nflfastR | Plot: Ward Walton") +
       theme(
    plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(hjust = 0.5, size = 12)
  )

## dropback epa/play vs air yards/attempt
scrambles <- pbp %>%
  filter(qb_dropback == 1 & qb_scramble == 1) %>%
  group_by(rusher_player_name) %>%
  summarize(scrambles = n(),
            epa = mean(epa)) %>%
  arrange(desc(scrambles)) %>%
  ungroup()


dropbacks2 <- pbp %>%
  filter(qb_dropback == 1 & !is.na(passer_player_id)) %>%
  group_by(passer_player_name) %>%
  summarize(dropbacks = n(),
            id = first(id),
            epa = mean(epa, na.rm = TRUE),
            air_yards = mean(air_yards, na.rm = TRUE)) %>%
  arrange(desc(dropbacks)) %>%
  ungroup()

dropbacks3 <- scrambles %>%
  left_join(dropbacks2, by = c("rusher_player_name" = "passer_player_name")) %>%
  mutate(scramble_rate = scrambles / (dropbacks), 
         total_plays = scrambles + dropbacks)


view(dropbacks3)


dropbacks3 %>%
  filter(total_plays >= 60) %>% 
  ggplot(aes(x = air_yards, y = epa.y, label = rusher_player_name)) +
  geom_nfl_headshots(aes(player_gsis = id), width = .11) +
  geom_text(nudge_y = -0.04, size = 7) + 
  theme_minimal() +
  labs(title = "Quarterback Air Yards/Attempt vs EPA/Play",
       subtitle = "Minimum 60 Dropbacks",
       x = "Average Air Yards per Attempt",
       y = "EPA per Dropback",
       caption = "Data: nflfastR | Plot: Ward Walton") +
       theme(
    plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(hjust = 0.5, size = 12),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20)
  )

##horizontal boxplot of epa/dropback by qb

view(dropbacks)
dropbacks %>%
  filter(dropbacks >= 60) %>%
  ggplot(aes(x = reorder(passer_player_name, epa), y = epa)) +
  geom_boxplot(fill = "lightblue") +
  geom_nfl_headshots(aes(player_gsis = passer_player_id), y = -0.15, width = 0.1) +
  coord_cartesian(ylim = c(-0.2, 0.2)) +
  theme_minimal() +
  labs(title = "Quarterback EPA per Dropback",
       subtitle = "Minimum 60 Dropbacks",
       x = "Quarterback",
       y = "EPA per Dropback",
       caption = "Data: nflfastR | Plot: Ward Walton") +
       theme(
    plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(hjust = 0.5, size = 12),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 20),
    axis.text.x = element_text(size = 15)
  )
