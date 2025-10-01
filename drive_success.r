library(nflreadr)
library(nflfastR)
library(tidyverse)
library(nflplotR)
nflreadr::.clear_cache()
pbp <- load_pbp(2025)
colors <- nflfastR::teams_colors_logos

unique(pbp$fixed_drive_result)

drive_summary <- pbp %>% filter(!is.na(posteam)) %>%
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
