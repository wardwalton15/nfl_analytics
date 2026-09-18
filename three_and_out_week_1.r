# Run: Rscript three_and_out_week_1.r [season=2026] [optional PBP .rds]
.libPaths(c('/private/tmp/nfl-week1-library', '/private/tmp/nfl-chart-r-library', .libPaths()))
library(dplyr)
library(ggplot2)
args <- commandArgs(trailingOnly = TRUE)
year <- if (length(args)) as.integer(args[1]) else 2026L
pbp <- if (length(args) >= 2) readRDS(args[2]) else nflreadr::load_pbp(year)
week1 <- pbp |> filter(season == year, season_type == 'REG', week == 1,
                       !coalesce(play_deleted == 1, FALSE))
stopifnot(n_distinct(week1$game_id) == 16L,
          !anyDuplicated(week1[c('game_id', 'play_id')]))
# Use corrected drive IDs; first_down includes first downs awarded by penalty.
# Follow the requested definition without imposing an exact play-count restriction.
drives <- week1 |> filter(!is.na(posteam), !is.na(fixed_drive)) |>
  group_by(game_id, posteam, fixed_drive) |>
  summarise(first_downs = sum(first_down, na.rm = TRUE),
            result = first(na.omit(fixed_drive_result), default = NA_character_),
            result_count = n_distinct(fixed_drive_result, na.rm = TRUE),
            punt_attempts = sum(punt_attempt, na.rm = TRUE), .groups = 'drop') |>
  mutate(three_and_out = coalesce(first_downs == 0 & result == 'Punt', FALSE))
stopifnot(all(drives$result_count <= 1),
          all(drives$punt_attempts[drives$three_and_out] >= 1))
ranking <- drives |> group_by(team = posteam) |>
  summarise(three_and_outs = sum(three_and_out), .groups = 'drop') |>
  arrange(desc(three_and_outs), team) |>
  mutate(rank = min_rank(desc(three_and_outs)))
stopifnot(nrow(ranking) == 32L)
# Prefer the largest complete tie group that keeps the chart between 5 and 10 teams.
cutoffs <- sort(unique(ranking$three_and_outs), decreasing = TRUE)
sizes <- vapply(cutoffs, function(x) sum(ranking$three_and_outs >= x), integer(1))
valid <- which(sizes >= 5 & sizes <= 10)
if (!length(valid)) stop('No tie-preserving cutoff between 5 and 10 teams.')
top <- ranking |> filter(three_and_outs >= cutoffs[max(valid)])
teams <- nflreadr::load_teams() |> select(team = team_abbr, team_name, team_color)
top <- top |> left_join(teams, by = 'team')
stopifnot(!anyNA(top$team_name))
top$team_name <- factor(top$team_name, levels = rev(top$team_name))
chart <- ggplot(top, aes(three_and_outs, team_name)) +
  geom_col(aes(fill = team_color), width = .62) +
  geom_text(aes(label = three_and_outs), hjust = -.55, size = 6, fontface = 'bold', color = '#242320') +
  scale_fill_identity() +
  scale_x_continuous(breaks = 0:max(top$three_and_outs), limits = c(0, max(top$three_and_outs) + .65), expand = c(0, 0)) +
  labs(title = 'MOST THREE-AND-OUTS',
       subtitle = paste0(year, ' NFL Week 1  |  Top ', nrow(top), ' offenses'),
       x = 'Drives with zero first downs ending in a punt', y = NULL,
       caption = 'Includes first downs by penalty. Tied teams ordered alphabetically.\nSource: nflverse / nflreadr  |  Plot: Ward Walton') +
  theme_minimal(base_size = 14) +
  theme(plot.background = element_rect(fill = '#F5F0E6', color = NA),
        panel.background = element_rect(fill = '#F5F0E6', color = NA),
        panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = '#DDD8CE'),
        axis.text.y = element_text(size = 15, color = '#242320'),
        axis.text.x = element_text(color = '#55534D'),
        axis.title.x = element_text(margin = margin(t = 15)),
        plot.title = element_text(size = 27, face = 'bold', color = '#242320'),
        plot.subtitle = element_text(size = 16, margin = margin(b = 22)),
        plot.caption = element_text(hjust = 0, size = 10, lineheight = 1.3, margin = margin(t = 20)),
        plot.title.position = 'plot', plot.caption.position = 'plot',
        plot.margin = margin(24, 30, 20, 24))
out <- paste0('outputs/three_and_out_week_1_', year)
dir.create(out, recursive = TRUE, showWarnings = FALSE)
write.csv(drives, file.path(out, 'drive_audit.csv'), row.names = FALSE)
write.csv(ranking, file.path(out, 'team_summary.csv'), row.names = FALSE)
ggsave(file.path(out, 'three_and_outs.png'), chart, width = 10, height = 6.4, dpi = 180)
print(top |> select(rank, team, three_and_outs))
