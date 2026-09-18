# Run: Rscript highest_scoring_week_1.r [optional cached schedules .rds]
.libPaths(c('/private/tmp/nfl-week1-library', '/private/tmp/nfl-gt-library', .libPaths()))
library(dplyr)
library(ggplot2)
args <- commandArgs(trailingOnly = TRUE)
schedules <- if (length(args)) readRDS(args[1]) else nflreadr::load_schedules(TRUE)
stopifnot(nrow(schedules) > 0)
games <- schedules |> filter(game_type == 'REG', week == 1, season <= 2026) |>
  select(game_id, season, gameday, home_team, away_team, home_score, away_score)
stopifnot(!anyDuplicated(games$game_id))
summary <- games |> group_by(season) |>
  summarise(games = n(), complete = all(!is.na(home_score) & !is.na(away_score)),
            points = sum(home_score + away_score), .groups = 'drop') |>
  filter(complete) |> arrange(desc(points), season) |>
  mutate(rank = min_rank(desc(points)), points_per_game = points / games)
stopifnot(2026 %in% summary$season, all(games$home_score >= 0, na.rm = TRUE),
          all(games$away_score >= 0, na.rm = TRUE))
top <- summary |> slice_head(n = 5) |>
  mutate(year = factor(season, levels = rev(season)),
         label = paste0(format(points, big.mark = ','), ' points'),
         rank_label = ifelse(duplicated(rank) | duplicated(rank, fromLast = TRUE),
                             paste0('T', rank), as.character(rank)))
out <- 'outputs/highest_scoring_week_1'
dir.create(out, recursive = TRUE, showWarnings = FALSE)
write.csv(games, file.path(out, 'game_audit.csv'), row.names = FALSE)
write.csv(summary, file.path(out, 'season_totals.csv'), row.names = FALSE)
write.csv(top |> select(rank, season, points, games, points_per_game),
          file.path(out, 'top_5.csv'), row.names = FALSE)
p <- ggplot(top, aes(points, year)) +
  geom_col(aes(fill = season == 2026), width = .62) +
  geom_text(aes(label = label), hjust = 1.12, color = '#FFFFFF',
            size = 5.2, fontface = 'bold') +
  geom_text(aes(x = 825, label = rank_label), color = '#6F675C', size = 4.5) +
  scale_fill_manual(values = c('FALSE' = '#475764', 'TRUE' = '#B44A16'), guide = 'none') +
  scale_x_continuous(limits = c(0, 860), breaks = seq(0, 800, 200), expand = c(0, 0)) +
  labs(title = 'HIGHEST-SCORING NFL WEEK 1s',
       subtitle = sprintf('2026 ties 2012 for No. 1: 791 total points\nTop five seasons | %s–2026 available data', min(summary$season)),
       x = 'Total points scored', y = NULL,
       caption = 'Regular-season Week 1 • Both teams’ final scores, counted once per game.\nAll five seasons shown had 16 games. T1 denotes a tie for first.\nSource: nflverse / nflreadr schedule data | Ward Walton') +
  theme_minimal(base_size = 14, base_family = 'Arial') +
  theme(plot.background = element_rect(fill = '#F5F0E6', color = NA),
        panel.background = element_rect(fill = '#F5F0E6', color = NA),
        panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = '#E1D8C9', linewidth = .4),
        plot.title = element_text(size = 25, face = 'bold', color = '#242320', margin = margin(b = 10)),
        plot.subtitle = element_text(size = 14, color = '#B44A16', lineheight = 1.4, margin = margin(b = 22)),
        axis.text.y = element_text(size = 17, face = 'bold', color = '#242320'),
        axis.text.x = element_text(color = '#6F675C'),
        axis.title.x = element_text(size = 12, margin = margin(t = 12)),
        plot.caption = element_text(hjust = 0, size = 10.5, lineheight = 1.4, margin = margin(t = 20)),
        plot.margin = margin(25, 28, 22, 25))
ggsave(file.path(out, 'highest_scoring_week_1.png'), p, width = 11, height = 7,
       dpi = 220, device = ragg::agg_png)
print(top |> select(rank, season, points, games, points_per_game))
