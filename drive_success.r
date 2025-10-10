library(nflreadr)
library(nflfastR)
library(tidyverse)
library(nflplotR)
nflreadr::.clear_cache()
pbp <- load_pbp(2025)
colors <- nflfastR::teams_colors_logos

unique(pbp$fixed_drive_result)

drive_summary <- pbp %>% filter(!is.na(posteam) & fixed_drive_result != "End of half") %>%
  group_by(game_id, posteam, drive) %>%
  summarise(drive_result = first(fixed_drive_result), .groups = "drop") %>%
  mutate(result_group = case_when(
    drive_result %in% c("Field goal", "Missed field goal") ~ "FG_att",
    drive_result == "Touchdown"                            ~ "TD",
    drive_result == "Punt"                                 ~ "Punt",
    TRUE                                                   ~ "Turnover"
  )) %>%
  group_by(posteam, result_group) %>%
  summarise(n_drives = n(), .groups = "drop_last") %>%
  mutate(total_drives = sum(n_drives),
         pct = n_drives / total_drives) %>%
  ungroup() %>%
  select(posteam, result_group, pct) %>%
  pivot_wider(
    names_from = result_group,
    values_from = pct,
    values_fill = 0
  ) %>%
  rename(
    td_pct        = TD,
    fg_att_pct    = FG_att,
    punt_pct      = Punt,
    turnover_pct  = Turnover
  ) %>%
  select(posteam, td_pct, fg_att_pct, punt_pct, turnover_pct)

view(drive_summary)


plot_data <- drive_summary %>%
  pivot_longer(
    cols = c(td_pct, fg_att_pct, punt_pct, turnover_pct),
    names_to = "result",
    values_to = "pct"
  ) %>%
  mutate(
    result = recode(result,
      "td_pct"       = "Touchdown",
      "fg_att_pct"   = "FG Attempt",
      "punt_pct"     = "Punt",
      "turnover_pct" = "Turnover"
    ),
    # enforce stacking order
    result = factor(result, levels = c("Turnover", "Punt", "FG Attempt", "Touchdown"))
  ) %>%
  complete(posteam, result, fill = list(pct = 0))

# find team order by td_pct
team_order <- drive_summary %>%
  arrange((td_pct)) %>%
  pull(posteam)

plot_data <- plot_data %>%
  mutate(posteam = factor(posteam, levels = team_order))

# logo positions (top of bars)
logo_positions <- plot_data %>%
  group_by(posteam) %>%
  summarise(y = sum(pct)/2, .groups = "drop")

ggplot(plot_data, aes(x = posteam, y = pct, fill = result)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.85) +
  geom_nfl_logos(
    data = logo_positions,
    aes(x = posteam, y = y, team_abbr = posteam),
    inherit.aes = FALSE,
    width = 0.05
  ) +
  scale_fill_manual(
    name = "",
    values = c(
      "Touchdown" = "#4CAF50",   # green
      "FG Attempt" = "#FFC107",  # amber
      "Punt" = "#2196F3",        # blue
      "Turnover" = "#F44336"     # red
    )
  ) +
  coord_flip() +
  labs(
    title = "NFL Drive Outcomes by Team",
    subtitle = "Percentage of drives ending in TD, FG attempt, Punt, or Turnover",
    y = "Share of Drives",
    x = NULL,
    caption = "Data: nflfastR | Plot: Ward Walton"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 16),
    plot.caption = element_text(hjust = 0.5, size = 12),
    legend.position = "top"
  ) + theme(axis.text.y = element_blank())

#defense
drive_summary <- pbp %>% filter(!is.na(defteam) & fixed_drive_result != "End of half") %>%
  group_by(game_id, defteam, drive) %>%
  summarise(drive_result = first(fixed_drive_result), .groups = "drop") %>%
  mutate(result_group = case_when(
    drive_result %in% c("Field goal", "Missed field goal") ~ "FG_att",
    drive_result == "Touchdown"                            ~ "TD",
    drive_result == "Punt"                                 ~ "Punt",
    TRUE                                                   ~ "Turnover"
  )) %>%
  group_by(defteam, result_group) %>%
  summarise(n_drives = n(), .groups = "drop_last") %>%
  mutate(total_drives = sum(n_drives),
         pct = n_drives / total_drives) %>%
  ungroup() %>%
  select(defteam, result_group, pct) %>%
  pivot_wider(
    names_from = result_group,
    values_from = pct,
    values_fill = 0
  ) %>%
  rename(
    td_pct        = TD,
    fg_att_pct    = FG_att,
    punt_pct      = Punt,
    turnover_pct  = Turnover
  ) %>%
  select(defteam, td_pct, fg_att_pct, punt_pct, turnover_pct)

view(drive_summary)


plot_data <- drive_summary %>%
  pivot_longer(
    cols = c(td_pct, fg_att_pct, punt_pct, turnover_pct),
    names_to = "result",
    values_to = "pct"
  ) %>%
  mutate(
    result = recode(result,
      "td_pct"       = "Touchdown",
      "fg_att_pct"   = "FG Attempt",
      "punt_pct"     = "Punt",
      "turnover_pct" = "Turnover"
    ),
    # enforce stacking order
    result = factor(result, levels = c("Turnover", "Punt", "FG Attempt", "Touchdown"))
  ) %>%
  complete(defteam, result, fill = list(pct = 0))

# find team order by td_pct
team_order <- drive_summary %>%
  arrange(desc(td_pct)) %>%
  pull(defteam)

plot_data <- plot_data %>%
  mutate(defteam = factor(defteam, levels = team_order))

# logo positions (top of bars)
logo_positions <- plot_data %>%
  group_by(defteam) %>%
  summarise(y = sum(pct)/2, .groups = "drop")

ggplot(plot_data, aes(x = defteam, y = pct, fill = result)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.85) +
  geom_nfl_logos(
    data = logo_positions,
    aes(x = defteam, y = y, team_abbr = defteam),
    inherit.aes = FALSE,
    width = 0.05
  ) +
  scale_fill_manual(
    name = "",
    values = c(
      "Touchdown" = "#4CAF50",   # green
      "FG Attempt" = "#FFC107",  # amber
      "Punt" = "#2196F3",        # blue
      "Turnover" = "#F44336"     # red
    )
  ) +
  coord_flip() +
  labs(
    title = "NFL Drive Outcomes by Team Defense",
    subtitle = "Percentage of opponent drives ending in TD, FG attempt, Punt, or Turnover",
    y = "Share of Drives",
    x = NULL,
    caption = "Data: nflfastR | Plot: Ward Walton"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 16),
    plot.caption = element_text(hjust = 0.5, size = 12),
    legend.position = "top"
  ) + theme(axis.text.y = element_blank())

###three and out rate by team
three_and_out <- pbp %>% filter(!is.na(posteam) & fixed_drive_result != "End of half") %>%
  group_by(game_id, posteam, drive) %>%
  summarise(drive_result = first(fixed_drive_result),
  fd = sum(first_down, na.rm = TRUE), .groups = "drop") %>%
  mutate(three_and_out = ifelse(drive_result == "Punt" & (fd == 0) == 1, 1, 0)) %>%
  group_by(posteam) %>%
  summarise(n_drives = n(),
            n_three_and_out = sum(three_and_out),
            three_and_out_pct = n_three_and_out / n_drives,
            .groups = "drop") %>%
  arrange(three_and_out_pct)
view(three_and_out)

#plot three and out
ggplot(three_and_out, aes(x = reorder(posteam, three_and_out_pct), y = three_and_out_pct)) +
  geom_bar(stat = "identity", fill = "#1d428a", alpha = 0.85) +
  geom_nfl_logos(
    data = three_and_out %>%
      mutate(y = three_and_out_pct / 2),
    aes(x = posteam, y = y, team_abbr = posteam),
    inherit.aes = FALSE,
    width = 0.05
  ) +
  coord_flip() +
  labs(
    title = "NFL Three-and-Out Rate by Team",
    subtitle = "Percentage of drives ending in a three-and-out (no first downs and a punt)",
    y = "Three-and-Out Rate",
    x = NULL,
    caption = "Data: nflfastR | Plot: Ward Walton"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 16),
    plot.caption = element_text(hjust = 0.5, size = 12)
  ) + theme(axis.text.y = element_blank())

### red zone drive summary
rz_drive_summary <- pbp %>% 
  filter(!is.na(posteam) & fixed_drive_result != "End of half" & yardline_100 <= 20 & !play_type %in% c("extra_point") & week <= 4) %>%
  group_by(game_id, posteam, drive) %>%
  summarise(drive_result = first(fixed_drive_result), .groups = "drop") %>%
  mutate(result_group = case_when(
    drive_result %in% c("Field goal", "Missed field goal") ~ "FG_att",
    drive_result == "Touchdown"                            ~ "TD",
    TRUE                                                   ~ "Turnover"
  )) %>%
  group_by(posteam, result_group) %>%
  summarise(n_drives = n(), .groups = "drop_last") %>%
  mutate(total_drives = sum(n_drives)) %>%
  ungroup() %>%
  select(posteam, result_group, n_drives) %>%
  pivot_wider(
    names_from = result_group,
    values_from = n_drives,
    values_fill = 0
  ) %>%
  rename(
    td        = TD,
    fg_att    = FG_att,
    turnover  = Turnover
  ) %>%
  select(posteam, td, fg_att, turnover)

view(rz_drive_summary)


plot_data <- rz_drive_summary %>%
  pivot_longer(
    cols = c(td, fg_att, turnover),
    names_to = "result",
    values_to = "count"
  ) %>%
  mutate(
    result = recode(result,
      "td"       = "Touchdown",
      "fg_att"   = "FG Attempt",
      "turnover" = "Turnover"
    ),
    result = factor(result, levels = c("Turnover", "FG Attempt", "Touchdown"))
  ) %>%
  complete(posteam, result, fill = list(count = 0))

team_order <- rz_drive_summary %>%
  arrange(td) %>%
  pull(posteam)

plot_data <- plot_data %>%
  mutate(posteam = factor(posteam, levels = team_order))


# logo positions (top of bars)
logo_positions <- plot_data %>%
  group_by(posteam) %>%
  summarise(y = 1, .groups = "drop")

view(plot_data)

ggplot(plot_data, aes(x = posteam, y = count, fill = result)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.85) +
  geom_nfl_logos(
    data = logo_positions,
    aes(x = posteam, y = y, team_abbr = posteam),
    inherit.aes = FALSE,
    width = 0.05
  ) +
  coord_flip() +
  scale_fill_manual(
    name = "",
    values = c(
      "Touchdown" = "#4CAF50",
      "Turnover" = "#F44336",
      "FG Attempt" = "#FFC107"
    )
  ) +
  labs(
    title = "Red Zone Outcomes by Team",
    subtitle = "Number of red zone drives ending in TD, FG attempt, or Turnover",
    y = "Number of Red Zone Drives",
    x = NULL,
    caption = "Data: nflfastR | Plot: Ward Walton"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 16),
    plot.caption = element_text(hjust = 0.5, size = 12),
    legend.position = "top"
  ) +
  theme(axis.text.y = element_blank())
