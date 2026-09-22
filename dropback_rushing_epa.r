# Run: Rscript dropback_rushing_epa.r [season=2026] [through_week=2] [PBP .rds]
.libPaths(c('/private/tmp/nfl-chart-r-library', .libPaths()))
suppressPackageStartupMessages({library(dplyr); library(ggplot2)})
args <- commandArgs(trailingOnly = TRUE)
season_id <- if (length(args)) as.integer(args[1]) else 2026L
through_week <- if (length(args) >= 2) as.integer(args[2]) else 2L
stopifnot(!is.na(season_id), !is.na(through_week), through_week %in% 1:18)
out <- sprintf('outputs/dropback_rushing_epa_%s_through_week_%s', season_id, through_week)
source <- if (length(args) >= 3) args[3] else sprintf(
  'outputs/epa_success_%s_through_week_%s/source_pbp.rds', season_id, through_week)
pbp <- if (file.exists(source)) readRDS(source) else nflreadr::load_pbp(season_id)
selected <- pbp |> filter(season == season_id, season_type == 'REG', between(week, 1L, through_week))
if (!nrow(selected) || max(selected$week) != through_week) stop('Requested weeks are unavailable.')
plays <- selected |> filter(play_type %in% c('run', 'pass'), !is.na(posteam),
  !coalesce(play_deleted == 1, FALSE), coalesce(qb_kneel, 0) == 0,
  coalesce(qb_spike, 0) == 0, coalesce(two_point_attempt, 0) == 0) |>
  mutate(dropback = qb_dropback == 1)
stopifnot(!anyNA(plays$dropback), !anyDuplicated(plays[c('game_id', 'play_id')]))
teams <- plays |> group_by(team = posteam) |> summarise(
  plays = n(), games = n_distinct(game_id), dropbacks = sum(dropback),
  dropback_rate = mean(dropback), designed_runs = sum(!dropback),
  runs_with_epa = sum(!dropback & is.finite(epa)),
  rushing_epa_per_play = mean(epa[!dropback & is.finite(epa)]), .groups = 'drop')
stopifnot(nrow(teams) == 32, all(is.finite(teams$rushing_epa_per_play)),
  sum(teams$plays) == nrow(plays), all(teams$dropback_rate >= 0 & teams$dropback_rate <= 1))
logo_dir <- file.path(out, 'logos')
dir.create(logo_dir, recursive = TRUE, showWarnings = FALSE)
for (tm in teams$team) {
  dest <- file.path(logo_dir, paste0(tm, '.png'))
  existing <- file.path('outputs/epa_success_2026_through_week_2/logos', paste0(tm, '.png'))
  if (!file.exists(dest) && file.exists(existing)) file.copy(existing, dest)
  if (!file.exists(dest)) {
    meta <- nflreadr::load_teams()
    download.file(meta$team_logo_espn[match(tm, meta$team_abbr)], dest, mode = 'wb', quiet = TRUE)
  }
}
teams$logo <- file.path(logo_dir, paste0(teams$team, '.png'))
bg <- '#F7F3EA'
xr <- range(teams$dropback_rate) + c(-.025, .025)
yr <- range(teams$rushing_epa_per_play)
span <- max(diff(yr), .1)
yr <- yr + c(-.10, .10) * span
# Separate nearby logos while preserving exact coordinates with small dots and leaders.
anchor <- cbind((teams$dropback_rate - xr[1]) / diff(xr),
                (teams$rushing_epa_per_play - yr[1]) / diff(yr))
pos <- anchor
for (iter in 1:1000) {
  pos <- pos + .008 * (anchor - pos)
  for (i in 1:(nrow(teams) - 1)) for (j in (i + 1):nrow(teams)) {
    delta <- pos[j, ] - pos[i, ]; distance <- sqrt(sum(delta^2))
    if (distance < .058) {
      direction <- if (distance < 1e-10) c(1, 0) else delta / distance
      push <- direction * (.058 - distance) / 2
      pos[i, ] <- pos[i, ] - push; pos[j, ] <- pos[j, ] + push
    }
  }
  pos[] <- pmax(.04, pmin(.96, pos))
}
teams$logo_x <- xr[1] + pos[, 1] * diff(xr)
teams$logo_y <- yr[1] + pos[, 2] * diff(yr)
lac <- teams |> filter(team == 'LAC')
stopifnot(nrow(lac) == 1)
p <- ggplot(teams, aes(dropback_rate, rushing_epa_per_play)) +
  geom_vline(xintercept = mean(plays$dropback), linetype = 'dashed', color = '#AAA69B') +
  geom_hline(yintercept = mean(plays$epa[!plays$dropback & is.finite(plays$epa)]),
    linetype = 'dashed', color = '#AAA69B') +
  geom_segment(aes(xend = logo_x, yend = logo_y), color = '#96978E', linewidth = .4) +
  geom_point(size = 1, color = '#777E82') +
  ggpath::geom_from_path(data = filter(teams, team != 'LAC'),
    aes(x = logo_x, y = logo_y, path = logo), width = .048) +
  ggpath::geom_from_path(data = lac,
    aes(x = logo_x, y = logo_y, path = logo), width = .064) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_y_continuous(labels = scales::label_number(accuracy = .01)) +
  coord_cartesian(xlim = xr, ylim = yr, expand = FALSE) +
  labs(title = 'DROPBACK RATE vs. RUSHING EPA',
    subtitle = sprintf('%s NFL · Weeks 1–%s · Los Angeles Chargers highlighted', season_id, through_week),
    x = 'DROPBACK RATE', y = 'RUSHING EPA / PLAY',
    caption = paste('Dropback rate = dropbacks / (dropbacks + designed runs). Dropbacks include sacks and scrambles.',
      'Rushing EPA uses designed runs with finite EPA. Excludes kneels, spikes, deleted plays and two-point tries.',
      'Dashed lines = play-weighted league averages. Dots mark exact values where logos are displaced.',
      'Source: nflverse | Ward Walton', sep = '\n')) +
  theme_minimal(base_family = 'Arial', base_size = 13) +
  theme(plot.background = element_rect(fill = bg, color = NA),
    panel.grid.minor = element_blank(), panel.grid.major = element_line(color = '#E3E0D7', linewidth = .3),
    plot.title = element_text(size = 26, face = 'bold', color = '#242922'),
    plot.subtitle = element_text(size = 12, margin = margin(b = 18)),
    plot.caption = element_text(hjust = 0, size = 9, lineheight = 1.4, margin = margin(t = 16)),
    axis.title = element_text(face = 'bold', size = 12),
    plot.margin = margin(24, 26, 20, 24), panel.border = element_rect(fill = NA, color = '#C9C7BD'))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
write.csv(teams, file.path(out, 'team_summary.csv'), row.names = FALSE)
ggsave(file.path(out, 'dropback_rushing_epa.png'), p, width = 11, height = 9, dpi = 180, device = ragg::agg_png)
print(lac)
message('Saved to ', out)
