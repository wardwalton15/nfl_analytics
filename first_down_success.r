install.packages("tidyverse")
install.packages("nflfastR")
install.packages("gt")
install.packages("nflplotR")

library(nflplotR)
library(tidyverse)
library(nflfastR)
library(gt)

#which team in 2024 passed the most on first down? (when game was in a competitive state)
pbp_2024 <- load_pbp(2024)
colors <- nflfastR::teams_colors_logos

pbp_2024 <- pbp_2024 %>%
  left_join(colors, by = c("posteam" = "team_abbr"))

pbp_2024 %>%
  filter(
    down == 1,
    rush == 1 | pass == 1,
    between(wp, .1, .9)
  ) %>%
  group_by(posteam) %>%
  summarise(
    logo = first(team_logo_espn),
    p = sum(pass == 1, na.rm = TRUE),
    run = sum(rush == 1, na.rm = TRUE),
    pass_epa = mean(epa[pass == 1], na.rm = TRUE),
    run_epa = mean(epa[rush == 1], na.rm = TRUE),
    epa = mean(epa, na.rm = TRUE),
    plays = n(),
    .groups = "drop"
  ) %>%
  mutate(pass_pct = p / (p + run)) %>%
  ggplot(aes(x = pass_pct, y = epa)) +
  geom_nfl_logos(aes(team_abbr = posteam, width = 0.075)) +
labs(
  title = "First Down EPA vs Pass Rate in 2024 (when Win Probability between 10% and 90%)",
  caption = "Data: @nflfastR | Plot: Ward Walton",
  xlab = "Percentage of First Down Plays that were Passes",
  ylab = "Average EPA per Play"
)

#team first down epa vs third down epa
pbp_2024 %>%
  filter(
    down == 1 | down == 3,
    rush == 1 | pass == 1,
    between(wp, .1, .9)
  ) %>%
  group_by(posteam, down) %>%
  summarise(
    logo = first(team_logo_espn),
    p = sum(pass == 1, na.rm = TRUE),
    run = sum(rush == 1, na.rm = TRUE),
    epa = mean(epa, na.rm = TRUE),
    plays = n(),
    .groups = "drop"
  ) %>%
  mutate(pass_pct = p / (p + run)) %>%
  select(posteam, down, epa) %>%
  pivot_wider(names_from = down, values_from = epa, names_prefix = "down_") %>%
  filter(!is.na(down_1) & !is.na(down_3)) %>%
  ggplot(aes(x = down_1, y = down_3)) +
  geom_nfl_logos(aes(team_abbr = posteam, width = 0.075)) +
  labs(
    title = "First Down EPA vs Third Down EPA in 2024 (when Win Probability between 10% and 90%)",
    caption = "Data: @nflfastR | Plot: Ward Walton",
    xlab = "Average EPA per Play on First Down",
    ylab = "Average EPA per Play on Third Down"
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")

#most efficient third down targets in 2024 (min 25 targets)
pbp_2024 %>%
  filter(down == 3 & (rush == 1 | pass == 1) & !is.na(receiver_player_name)) %>% 
  group_by(receiver_player_name, posteam) %>%
  summarise(
    targets = sum(pass == 1, na.rm = TRUE),
    receptions = sum(complete_pass == 1, na.rm = TRUE),
    first_downs = sum(first_down_pass == 1, na.rm = TRUE) + sum(first_down_rush == 1, na.rm = TRUE),
    first_down_pct = round(100*(first_downs / targets), 1),
    .groups = "drop"
  ) %>% arrange(desc(first_down_pct)) %>%
  filter(targets >= 25) %>%
  slice_head(n = 10) %>%
  gt() %>%
  tab_header(
    title = "Top Third Down Targets in 2024",
    subtitle = "By Pct of Targets that Resulted in a First Down (Min 25 Third Down Targets)"
  ) %>% 
  cols_label(receiver_player_name = "Player",
    posteam = "Team",
    targets = "Targets",
    receptions = "Receptions",
    first_downs = "First Downs",
    first_down_pct = "First Down %",
  ) %>%
  gt_nfl_logos(columns = "posteam")

#yards per target
pbp_2024 %>%
  filter(pass == 1 & !is.na(receiver_player_name)) %>%
  group_by(receiver_player_name, posteam) %>%
  summarise(
    routes = n(),
    yards = sum(yards_gained, na.rm = TRUE),
    ypr = round(yards / routes, 2),
    .groups = "drop"
  ) %>%
  arrange(desc(ypr)) %>%
  filter(routes >= 50) %>%
  slice_head(n = 10) %>%
  gt() %>%
  tab_header(
    title = "Top Yards Per Target in 2024",
    subtitle = "Min 50 Targets"
  ) %>%
  cols_label(
    receiver_player_name = "Player",
    posteam = "Team",
    routes = "Targets",
    yards = "Yards",
    ypr = "Yards Per Target"
  ) %>%
  gt_nfl_logos(columns = "posteam")




