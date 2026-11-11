# Run: Rscript under_center_rate.r [season=2026] [through_week=latest] [optional PBP .rds]
# Under-center share of offensive plays vs EPA/play on under-center plays, by team.
# Packages: nflreadr, dplyr, ggplot2, ggpath (and ggpath dependencies).
.libPaths(c('/private/tmp/nfl-chart-r-library', .libPaths()))
suppressPackageStartupMessages({library(dplyr); library(ggplot2)})
args <- commandArgs(trailingOnly = TRUE)
season_id <- if (length(args) >= 1) as.integer(args[1]) else 2026L
pbp <- if (length(args) >= 3) readRDS(args[3]) else nflreadr::load_pbp(season_id)
source_time <- attr(pbp, 'nflverse_timestamp')
regular <- pbp |> filter(season == season_id, season_type == 'REG', !coalesce(play_deleted == 1, FALSE))
stopifnot(nrow(regular) > 0)
week_id <- if (length(args) >= 2) as.integer(args[2]) else max(regular$week)
stopifnot(!is.na(season_id), week_id %in% 1:18)
regular <- regular |> filter(week <= week_id)

# Flag a week still in progress by comparing played games with the schedule.
scheduled <- nflreadr::load_schedules(season_id) |> filter(game_type == 'REG', week == week_id) |> nrow()
played <- n_distinct(regular$game_id[regular$week == week_id])
partial <- played < scheduled
out_dir <- sprintf('outputs/under_center_%s_through_week_%s%s', season_id, week_id, if (partial) '_partial' else '')
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(regular, file.path(out_dir, 'source_pbp.rds'))

# wp is the possession team's pre-play win probability (inclusive bounds).
# Non-shotgun is the PBP proxy for under center (includes pistol); this is not charted formation data.
# Rate uses all eligible plays with known formation, even if EPA is missing.
plays <- regular |>
  filter(play_type %in% c('run', 'pass'), !is.na(posteam),
         between(wp, .05, .95), coalesce(qb_kneel, 0) == 0,
         coalesce(qb_spike, 0) == 0, coalesce(two_point_attempt, 0) == 0)
missing_formation <- sum(!plays$shotgun %in% c(0, 1))
plays <- plays |>
  filter(shotgun %in% c(0, 1)) |>
  mutate(under_center = shotgun == 0)
stopifnot(!anyDuplicated(plays[c('game_id', 'play_id')]))
uc_epa <- function(d) mean(d$epa[d$under_center & is.finite(d$epa)])
team_summary <- plays |>
  group_by(team = posteam) |>
  summarise(games = n_distinct(game_id),
            eligible_plays = n(), under_center_plays = sum(under_center),
            under_center_rate = mean(under_center),
            under_center_epa_plays = sum(under_center & is.finite(epa)),
            under_center_epa = mean(epa[under_center & is.finite(epa)]),
            under_center_pass_rate = mean(qb_dropback[under_center] == 1),
            shotgun_epa = mean(epa[!under_center & is.finite(epa)]),
            .groups = 'drop') |>
  arrange(desc(under_center_rate))
stopifnot(nrow(team_summary) == 32, all(is.finite(team_summary$under_center_epa)),
          sum(team_summary$eligible_plays) == nrow(plays))
league_rate <- mean(plays$under_center)
league_epa <- uc_epa(plays)
fit <- cor.test(team_summary$under_center_rate, team_summary$under_center_epa)
write.csv(team_summary, file.path(out_dir, 'team_summary.csv'), row.names = FALSE)
write.csv(plays |> select(game_id, play_id, week, posteam, play_type, qb_dropback, wp, shotgun,
                          under_center, epa, desc),
          file.path(out_dir, 'play_audit.csv'), row.names = FALSE)
write.csv(data.frame(eligible_plays = nrow(plays),
                     excluded_missing_formation = missing_formation,
                     under_center_rate = league_rate, under_center_epa = league_epa,
                     team_correlation = unname(fit$estimate), p_value = fit$p.value),
          file.path(out_dir, 'league_summary.csv'), row.names = FALSE)

# Reuse existing logos, then download missing ones from nflverse team metadata.
logo_dir <- file.path(out_dir, 'logos')
dir.create(logo_dir, showWarnings = FALSE)
for (tm in team_summary$team) {
  dest <- file.path(logo_dir, paste0(tm, '.png'))
  existing <- file.path('outputs/first_down_drives_2025/logos', paste0(tm, '.png'))
  if (!file.exists(dest) && file.exists(existing)) file.copy(existing, dest)
  if (!file.exists(dest)) {
    meta <- nflreadr::load_teams()
    download.file(meta$team_logo_espn[match(tm, meta$team_abbr)], dest, mode = 'wb', quiet = TRUE)
  }
}
plot_data <- team_summary |> mutate(logo = file.path(logo_dir, paste0(team, '.png')))
stopifnot(all(file.exists(plot_data$logo)))

# Axes have different spreads, so collisions are resolved in range-normalized units.
pad <- function(r) r + c(-1, 1) * diff(r) * .08
xl <- pad(range(plot_data$under_center_rate)); yl <- pad(range(plot_data$under_center_epa))
anchor <- cbind((plot_data$under_center_rate - xl[1]) / diff(xl),
                (plot_data$under_center_epa - yl[1]) / diff(yl))
pos <- anchor
min_distance <- .06
for (iteration in 1:800) {
  pos <- pos + .012 * (anchor - pos)
  for (i in 1:(nrow(pos) - 1)) for (j in (i + 1):nrow(pos)) {
    delta <- pos[j, ] - pos[i, ]; distance <- sqrt(sum(delta^2))
    if (distance < min_distance) {
      direction <- if (distance < 1e-10) c(1, 0) else delta / distance
      push <- direction * (min_distance - distance) * .5
      pos[i, ] <- pos[i, ] - push; pos[j, ] <- pos[j, ] + push
    }
  }
  pos <- pmin(pmax(pos, min_distance / 2), 1 - min_distance / 2)
}
plot_data$logo_x <- xl[1] + pos[, 1] * diff(xl); plot_data$logo_y <- yl[1] + pos[, 2] * diff(yl)
plot_data$moved <- sqrt(rowSums((pos - anchor)^2)) > .006
write.csv(plot_data |> select(team, under_center_rate, under_center_epa, logo_x, logo_y, moved),
          file.path(out_dir, 'logo_positions.csv'), row.names = FALSE)

week_label <- sprintf('Weeks 1–%s%s', week_id,
                      if (partial) sprintf(' (week %s: %s of %s games)', week_id, played, scheduled) else '')
under_center_plot <- ggplot(plot_data) +
  geom_vline(xintercept = league_rate, linetype = 'dashed', color = '#B7AC9A') +
  geom_hline(yintercept = league_epa, linetype = 'dashed', color = '#B7AC9A') +
  geom_segment(data = filter(plot_data, moved),
               aes(under_center_rate, under_center_epa, xend = logo_x, yend = logo_y),
               color = '#686158', linewidth = .4) +
  geom_point(data = filter(plot_data, moved), aes(under_center_rate, under_center_epa),
             color = '#686158', size = 1.1) +
  ggpath::geom_from_path(aes(x = logo_x, y = logo_y, path = logo), width = .045) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1), breaks = seq(0, 1, .05)) +
  scale_y_continuous(labels = scales::label_number(accuracy = .01, style_negative = 'minus')) +
  coord_cartesian(xlim = xl, ylim = yl, expand = FALSE) +
  labs(title = 'UNDER CENTER: USAGE VS. EFFICIENCY',
       subtitle = sprintf('%s NFL regular season, %s | Pre-play win probability: 5–95%%',
                          season_id, week_label),
       x = 'Under-center share of offensive plays',
       y = 'EPA/play from under center',
       caption = sprintf(paste0('Dashed lines: play-weighted NFL averages (%.1f%% usage, %+.3f EPA/play). ',
         'Team-level correlation r = %.2f.\n',
         'Under center = shotgun == 0 (pistol counts as under center). Runs + passes include sacks and scrambles.\n',
         'Excludes kneels, spikes, no-plays and two-point attempts. Both axes use the same WP filter.\n',
         'Logos nudged apart where they overlap; leader lines mark actual coordinates.\n',
         'Source: nflverse / nflreadr | Plot: Ward Walton'), 100 * league_rate, league_epa, fit$estimate)) +
  theme_minimal(base_size = 13, base_family = 'Arial') +
  theme(plot.background = element_rect(fill = '#F5F0E6', color = NA),
        panel.background = element_rect(fill = '#F5F0E6', color = NA),
        panel.grid.minor = element_blank(), panel.grid.major = element_line(color = '#E4DDD1'),
        plot.title = element_text(face = 'bold', size = 22, color = '#242320'),
        plot.subtitle = element_text(color = '#686158', margin = margin(b = 18)),
        plot.caption = element_text(hjust = 0, color = '#686158', size = 10, lineheight = 1.25),
        axis.title = element_text(face = 'bold'), plot.margin = margin(20, 25, 15, 20))
png_name <- sprintf('under_center_%s_through_week_%s.png', season_id, week_id)
ggsave(file.path(out_dir, png_name), under_center_plot, width = 12, height = 9, dpi = 180)

writeLines(c(
  sprintf('# %s under-center usage vs EPA/play, REG %s', season_id, week_label),
  sprintf('Coverage: %s games, %s teams, %s eligible plays (%s dropped for missing formation).',
          n_distinct(plays$game_id), nrow(team_summary), nrow(plays), missing_formation),
  paste('nflverse source timestamp:', paste(source_time, collapse = ' ')),
  'Under center = shotgun == 0 in nflverse PBP (pistol and under-center both count). Not charted formation data.',
  'Plays: runs + passes (incl. sacks, scrambles); pre-play WP 5-95%; no kneels, spikes, 2-pt tries, no-plays.',
  sprintf('League: %.1f%% under center, %+.3f EPA/play under center. Team correlation r = %.2f (p = %.3f).',
          100 * league_rate, league_epa, fit$estimate, fit$p.value),
  'source_pbp.rds stores these games so a rerun with it reproduces the chart without downloading.',
  sprintf('Recreate: Rscript under_center_rate.r %s %s %s', season_id, week_id, file.path(out_dir, 'source_pbp.rds'))
), file.path(out_dir, 'README.md'))
options(width = 200)
print(team_summary |> select(team, games, eligible_plays, under_center_rate, under_center_epa,
                             under_center_epa_plays, under_center_pass_rate, shotgun_epa), n = 32)
message(sprintf('League: %.1f%% under center, %+.3f EPA/play | team r = %.2f (p = %.3f)',
                100 * league_rate, league_epa, fit$estimate, fit$p.value))
message('Saved plot and audit data to ', out_dir)
