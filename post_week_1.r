library(nflfastR)
library(tidyverse)
library(nflreadr)



#favorite cover % by spread (for spread_line a positive value means the home team was favored)
games <- load_pbp(seasons = c(1999:2025)) %>%
group_by (game_id) %>%
  slice_max(play_id, n = 1) %>%
  ungroup() %>%
  select(game_id, season, week, home_team, away_team, home_score, away_score, spread_line) %>%
  filter(!is.na(spread_line)) %>%
  mutate(
    favorite = ifelse(spread_line < 0, away_team, home_team),
    underdog = ifelse(spread_line < 0, home_team, away_team),
    favorite_score = ifelse(spread_line < 0, away_score, home_score),
    underdog_score = ifelse(spread_line < 0, home_score, away_score),
    spread = abs(spread_line),
    favorite_covers = (favorite_score - underdog_score ) > spread
  )

view(games)

#favorite cover rate by spread
games %>%
  group_by(spread) %>%
  summarize(
    games = n(),
    favorite_covers = sum(favorite_covers),
    cover_pct = favorite_covers / games
  ) %>%
  filter(games >= 10) %>%
  ggplot(aes(x = spread, y = cover_pct)) +
  geom_point(aes(size = games)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Favorite Cover Percentage by Point Spread",
    subtitle = "Data from 2024 and 2025 NFL Seasons",
    x = "Point Spread",
    y = "Favorite Cover Percentage",
    size = "Number of Games",
    caption = "Data: @nflfastR"
  ) +
  theme_minimal()

#histogram plot of spread
games %>%
  ggplot(aes(x = spread)) +
  geom_histogram(fill = "blue",binwidth = 0.5, alpha = 0.5) +
  labs(
    title = "Density Plot of Point Spreads",
    subtitle = "Data from 2024 and 2025 NFL Seasons",
    x = "Point Spread",
    y = "Density",
    caption = "Data: @nflfastR"
  ) +
  theme_minimal()
