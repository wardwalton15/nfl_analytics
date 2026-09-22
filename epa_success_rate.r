# Run: Rscript epa_success_rate.r [season=2026] [through_week=2] [PBP .rds]
.libPaths(c('/private/tmp/nfl-chart-r-library', .libPaths()))
suppressPackageStartupMessages({library(dplyr); library(ggplot2)})
args <- commandArgs(trailingOnly = TRUE)
season_id <- if (length(args)) as.integer(args[1]) else 2026L
through_week <- if (length(args) >= 2) as.integer(args[2]) else 2L
stopifnot(!is.na(season_id), !is.na(through_week), through_week %in% 1:18)
out <- sprintf('outputs/epa_success_%s_through_week_%s', season_id, through_week)
dir.create(out, recursive = TRUE, showWarnings = FALSE)
cache <- file.path(out, 'source_pbp.rds')
source <- if (length(args) >= 3) args[3] else cache
pbp <- if (file.exists(source)) readRDS(source) else nflreadr::load_pbp(season_id)
selected <- pbp |> filter(season == season_id, season_type == 'REG', between(week, 1L, through_week))
if (!nrow(selected) || max(selected$week) != through_week) stop('Requested weeks are unavailable.')
plays <- selected |> filter(play_type %in% c('run', 'pass'), is.finite(epa), !is.na(posteam),
  !coalesce(play_deleted == 1, FALSE), coalesce(qb_kneel, 0) == 0,
  coalesce(qb_spike, 0) == 0, coalesce(two_point_attempt, 0) == 0)
stopifnot(!anyDuplicated(plays[c('game_id', 'play_id')]))
teams <- plays |> group_by(team = posteam) |> summarise(plays = n(), games = n_distinct(game_id),
  epa_per_play = mean(epa), success_rate = mean(epa > 0), .groups = 'drop') |> arrange(desc(epa_per_play))
stopifnot(nrow(teams) == 32, sum(teams$plays) == nrow(plays), all(is.finite(teams$success_rate)))
# Before bye weeks, require one game per team in each requested week.
if (through_week <= 4) stopifnot(all(teams$games == through_week))
saveRDS(selected, cache)
write.csv(teams, file.path(out, 'team_summary.csv'), row.names = FALSE)
write.csv(plays |> select(game_id, play_id, week, posteam, play_type, epa), file.path(out, 'play_audit.csv'), row.names = FALSE)
logo_dir <- file.path(out, 'logos'); dir.create(logo_dir, showWarnings = FALSE)
for (tm in teams$team) {
  dest <- file.path(logo_dir, paste0(tm, '.png'))
  existing <- file.path('outputs/epa_team_tiers_2026_week_1/logos', paste0(tm, '.png'))
  if (!file.exists(dest) && file.exists(existing)) file.copy(existing, dest)
  if (!file.exists(dest)) {
    meta <- nflreadr::load_teams()
    download.file(meta$team_logo_espn[match(tm, meta$team_abbr)], dest, mode = 'wb', quiet = TRUE)
  }
}
teams$logo <- file.path(logo_dir, paste0(teams$team, '.png'))
teams$highlight <- teams$team %in% c('BUF', 'SF', 'BAL')
teams$color <- case_when(teams$team == 'BUF' ~ '#0055A4',
                         teams$team == 'BAL' ~ '#241773', TRUE ~ '#AA182C')
xlim <- range(teams$epa_per_play) + c(-.065, .065)
ylim <- range(teams$success_rate) + c(-.035, .035)
# Resolve logo collisions in normalized panel coordinates; dots retain exact values.
anchor <- cbind((teams$epa_per_play - xlim[1]) / diff(xlim), (teams$success_rate - ylim[1]) / diff(ylim))
pos <- anchor
for (iter in 1:1000) {
  pos <- pos + .008 * (anchor - pos)
  for (i in 1:31) for (j in (i + 1):32) {
    delta <- pos[j, ] - pos[i, ]; distance <- sqrt(sum(delta^2))
    if (distance < .082) {
      direction <- if (distance < 1e-10) c(1, 0) else delta / distance
      push <- direction * (.082 - distance) / 2
      pos[i, ] <- pos[i, ] - push; pos[j, ] <- pos[j, ] + push
    }
  }
  pos[] <- pmax(.045, pmin(.955, pos))
}
teams$logo_x <- xlim[1] + pos[, 1] * diff(xlim)
teams$logo_y <- ylim[1] + pos[, 2] * diff(ylim)
write.csv(teams, file.path(out, 'logo_positions.csv'), row.names = FALSE)
league_epa <- mean(plays$epa); league_success <- mean(plays$epa > 0)
hi <- teams |> filter(highlight)
p <- ggplot(teams, aes(epa_per_play, success_rate)) +
  annotate('rect', xmin = league_epa, xmax = Inf, ymin = league_success, ymax = Inf, fill = '#E4EDDF', alpha = .8) +
  geom_vline(xintercept = league_epa, linetype = 'dashed', color = '#9B998E', linewidth = .45) +
  geom_hline(yintercept = league_success, linetype = 'dashed', color = '#9B998E', linewidth = .45) +
  geom_segment(aes(xend = logo_x, yend = logo_y), color = '#96978E', linewidth = .4) +
  geom_point(size = 1.25, color = '#77796F') +
  geom_point(data = hi, aes(logo_x, logo_y, color = color), shape = 21, fill = 'white', size = 15, stroke = 1.4) +
  scale_color_identity() +
  ggpath::geom_from_path(aes(logo_x, logo_y, path = logo), width = .057) +
  scale_x_continuous(labels = scales::label_number(accuracy = .01)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
  labs(title = 'NFL OFFENSIVE EFFICIENCY',
    subtitle = sprintf('%s · Weeks 1–%s · %s games', season_id, through_week, n_distinct(plays$game_id)),
    x = 'EPA / PLAY  →', y = 'SUCCESS RATE  →',
    caption = paste('Success = EPA > 0. Dashed lines = play-weighted league averages. Shading = above average in both.',
      'Run/pass plays, including sacks and scrambles; excludes kneels, spikes and two-point tries.',
      'Dots show exact values; logos displaced where needed.  Source: nflverse  |  Ward Walton', sep = '\n')) +
  theme_minimal(base_family = 'Arial', base_size = 13) +
  theme(plot.background = element_rect(fill = '#F7F3EA', color = NA),
    panel.grid.minor = element_blank(), panel.grid.major = element_line(color = '#E3E0D7', linewidth = .3),
    plot.title = element_text(size = 27, face = 'bold', color = '#242922'),
    plot.subtitle = element_text(size = 12, lineheight = 1.55, margin = margin(b = 17)),
    plot.caption = element_text(hjust = 0, size = 9, lineheight = 1.4, margin = margin(t = 16)),
    axis.title = element_text(face = 'bold', size = 12), plot.margin = margin(24, 26, 20, 24),
    panel.border = element_rect(fill = NA, color = '#C9C7BD'))
ggsave(file.path(out, 'epa_success_rate.png'), p, width = 11, height = 10, dpi = 180, device = ragg::agg_png)
writeLines(c(sprintf('# Offensive efficiency: %s through Week %s', season_id, through_week),
  sprintf('32 teams; %s games; %s eligible plays.', n_distinct(plays$game_id), nrow(plays)),
  'EPA/play = mean EPA; success rate = fraction with EPA > 0, using the same eligible plays.',
  'Regular season run/pass plays including sacks/scrambles; exclude deleted plays, kneels, spikes, two-point attempts and nonfinite EPA.',
  'Reference lines are league averages weighted by plays. Highlight rings identify BUF (blue), SF (red) and BAL (purple).',
  'Logos are displaced to reduce overlap; dots and leader lines preserve exact coordinates.',
  'Source: nflverse play-by-play via nflreadr. Saved source_pbp.rds supports offline reproduction.',
  sprintf('Recreate: Rscript epa_success_rate.r %s %s', season_id, through_week)), file.path(out, 'README.md'))
print(hi |> select(team, plays, games, epa_per_play, success_rate))
message('Saved to ', out)
