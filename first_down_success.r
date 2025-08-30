install.packages("tidyverse")
install.packages("nflfastR")
install.packages("gt")
install.packages("nflplotR")

library(nflplotR)
library(tidyverse)
library(nflfastR)
library(gt)

pbp_2024 <- load_pbp(2024)
colors <- nflfastR::teams_colors_logos

pbp_2024 <- pbp_2024 %>%
  left_join(colors, by = c("posteam" = "team_abbr"))
# pass rate by down
pbp_2024 %>%
  filter((rush == 1 | pass == 1 & !is.na(down))) %>%
  group_by(down) %>%
  summarise(
    plays = n(),
    pass_plays = sum(pass == 1, na.rm = TRUE),
    epa = mean(epa, na.rm = TRUE),
    pass_pct = round(100 * mean(pass == 1, na.rm = TRUE), 1),
    .groups = "drop" 
  ) 



#pass_pct by win probability 
pbp_2024 %>%
  filter((rush == 1 | pass == 1)) %>%
  group_by(wp_bin = cut(wp, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)) %>%
  summarise(
    plays = n(),
    pass_plays = sum(pass == 1, na.rm = TRUE),
    pass_pct = round(100 * mean(pass == 1, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = wp_bin,
   y = pass_pct,
    group = 1)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Pass Rate by Win Probability in 2024",
    caption = "Data: @nflfastR | Plot: Ward Walton",
    x = "Win Probability Bin",
    y = "Pass Rate (%)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#distribution of yards gained- pass vs run(violin plot)
pbp_2024 %>%
  filter((rush == 1 | pass == 1)) %>%
  mutate(play_type = factor(pass, labels = c("Run", "Pass"))) %>%
  ggplot(aes(x = play_type, y = yards_gained, fill = play_type)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, position = position_dodge(0.9), outlier.shape = NA) +
  coord_cartesian(ylim = c(-10, 30)) +
  labs(
    title = "Distribution of Yards Gained in 2024",
    caption = "Data: @nflfastR | Plot: Ward Walton",
    x = "Play Type",
    y = "Yards Gained"
  ) +
  theme(legend.position = "none")


  #pct of time you gain 5+ yards by pass vs run
pbp_2024 %>%
  filter((rush == 1 | pass == 1)) %>%
  mutate(play_type = factor(pass, labels = c("Run", "Pass"))) %>%
  group_by(play_type) %>%
  summarise(
    plays = n(),
    gain_5_plus = sum(yards_gained >= 5, na.rm = TRUE),
    pct_5_plus = round(100 * mean(yards_gained >= 5, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  gt() %>%
  tab_header(
    title = "Percentage Gaining 5+ Yards in 2024",
    subtitle = "By Play Type"
  ) %>%
  cols_label(
    play_type = "Play Type",
    plays = "Total Plays",
    gain_5_plus = "Plays Gaining 5+",
    pct_5_plus = "Pct Gaining 5+"
  )


  #% of pass plays with completion - 54.2%
pbp_2024 %>%
  filter(pass == 1) %>%
  summarise(
    attempts = n(),
    completions = sum(complete_pass == 1, na.rm = TRUE),
    comp_pct = round(100 * mean(complete_pass == 1, na.rm = TRUE), 1)
  )



#which team in 2024 passed the most on first down? (when game was in a competitive state)

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
  title = "First Down EPA vs Pass Rate in 2024 (When Win Probability between 10% and 90%)",
  caption = "Data: @nflfastR | Plot: Ward Walton",
  x = "Percentage of First Down Plays that were Passes",
  y = "Average First Down EPA per Play"
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
    title = "First Down EPA vs Third Down EPA in 2024 (When Win Probability between 10% and 90%)",
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

#which qbs get sacked on the highest pct of dropbacks (min 50 dropbacks)
pbp_2024 %>%
  filter(pass == 1 & !is.na(passer_player_name)) %>%
  group_by(passer_player_name, posteam) %>%
  summarise(
    dropbacks = n(),
    sacks = sum(sack == 1, na.rm = TRUE),
    sack_pct = round(100 * mean(sack == 1, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  filter(dropbacks >= 100) %>%
  arrange(desc(sack_pct)) %>%
  slice_head(n = 10) %>%
  gt() %>%
  tab_header(
    title = "QBs with Highest Sack Percentage in 2024",
    subtitle = "Min 100 Dropbacks"
  ) %>%
  cols_label(
    passer_player_name = "Player",
    posteam = "Team",
    dropbacks = "Dropbacks",
    sacks = "Sacks",
    sack_pct = "Sack Percentage"
  ) %>%
  gt_nfl_logos(columns = "posteam")

