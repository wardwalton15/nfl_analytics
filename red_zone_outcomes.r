# Run: Rscript red_zone_outcomes.r [season=2026] [through_week=latest] [optional PBP .rds]
# How each team's red zone trips ended: TD, FG attempt, turnover, downs, or end of half.
# Packages: nflreadr, dplyr, tidyr, ggplot2, ggpath, scales.
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(ggplot2)})
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
out_dir <- sprintf('outputs/red_zone_outcomes_%s_through_week_%s%s', season_id, week_id, if (partial) '_partial' else '')
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(regular, file.path(out_dir, 'source_pbp.rds'))

# A red zone trip = an offensive drive with at least one scrimmage snap at the opponent's 20 or closer.
# Snaps wiped out by penalty (no_play) still count: the offense was snapping from there.
# PATs and two-point tries are excluded so a long TD drive is not counted as a trip.
outcome_levels <- c('Touchdown', 'FG attempt', 'Turnover', 'Downs', 'End of half')
trips <- regular |>
  filter(!is.na(posteam), !is.na(fixed_drive), yardline_100 <= 20,
         play_type %in% c('run', 'pass', 'field_goal', 'punt', 'qb_kneel', 'qb_spike', 'no_play'),
         coalesce(two_point_attempt, 0) == 0) |>
  group_by(game_id, week, team = posteam, fixed_drive) |>
  summarise(drive_result = first(fixed_drive_result), closest = min(yardline_100), .groups = 'drop') |>
  mutate(outcome = case_when(
    drive_result == 'Touchdown' ~ 'Touchdown',
    drive_result %in% c('Field goal', 'Missed field goal') ~ 'FG attempt',
    drive_result %in% c('Turnover', 'Opp touchdown') ~ 'Turnover',
    drive_result == 'Turnover on downs' ~ 'Downs',
    TRUE ~ 'End of half'),   # end of half/game, plus any rare punt or safety
    outcome = factor(outcome, levels = outcome_levels))
stopifnot(!anyDuplicated(trips[c('game_id', 'team', 'fixed_drive')]))

team_summary <- trips |>
  count(team, outcome, .drop = FALSE) |>
  pivot_wider(names_from = outcome, values_from = n, values_fill = 0) |>
  rename(td = Touchdown, fg_att = `FG attempt`, turnover = Turnover, downs = Downs, end_half = `End of half`) |>
  left_join(regular |> filter(!is.na(posteam)) |> distinct(team = posteam, game_id) |> count(team, name = 'games'),
            by = 'team') |>
  mutate(trips = td + fg_att + turnover + downs + end_half,
         td_rate = td / trips, trips_per_game = trips / games) |>
  arrange(desc(td), desc(td_rate), team)
stopifnot(nrow(team_summary) == 32, sum(team_summary$trips) == nrow(trips))
league_td_rate <- sum(team_summary$td) / sum(team_summary$trips)
write.csv(team_summary, file.path(out_dir, 'team_summary.csv'), row.names = FALSE)
write.csv(trips, file.path(out_dir, 'trip_audit.csv'), row.names = FALSE)

# Reuse existing logos, then download missing ones from nflverse team metadata.
logo_dir <- file.path(out_dir, 'logos')
dir.create(logo_dir, showWarnings = FALSE)
for (tm in team_summary$team) {
  dest <- file.path(logo_dir, paste0(tm, '.png'))
  existing <- file.path('outputs/under_center_2026_through_week_9/logos', paste0(tm, '.png'))
  if (!file.exists(dest) && file.exists(existing)) file.copy(existing, dest)
  if (!file.exists(dest)) {
    meta <- nflreadr::load_teams()
    download.file(meta$team_logo_espn[match(tm, meta$team_abbr)], dest, mode = 'wb', quiet = TRUE)
  }
}

team_levels <- rev(team_summary$team)   # most TDs at the top
bars <- team_summary |>
  select(team, Touchdown = td, `FG attempt` = fg_att, Turnover = turnover, Downs = downs, `End of half` = end_half) |>
  pivot_longer(-team, names_to = 'outcome', values_to = 'n') |>
  mutate(outcome = factor(outcome, levels = rev(outcome_levels)), team = factor(team, levels = team_levels))
labels <- team_summary |>
  mutate(team = factor(team, levels = team_levels), logo = file.path(logo_dir, paste0(team, '.png')),
         label = sprintf('%d%%', round(100 * td_rate)), sub = sprintf('%d trips', trips))
stopifnot(all(file.exists(labels$logo)))

fills <- c('Touchdown' = '#2E8B57', 'FG attempt' = '#E9B949', 'Turnover' = '#C8423B',
           'Downs' = '#E8836B', 'End of half' = '#C9C1B3')
max_trips <- max(team_summary$trips)
x_max <- ceiling((max_trips + 4.2) / 5) * 5
week_label <- sprintf('Weeks 1–%s%s', week_id,
                      if (partial) sprintf(' (week %s: %s of %s games)', week_id, played, scheduled) else '')

rz_plot <- ggplot(bars, aes(x = n, y = team, fill = outcome)) +
  geom_col(width = .78, color = '#F5F0E6', linewidth = .35) +
  geom_text(data = filter(bars, outcome == 'Touchdown', n > 0), aes(x = n / 2, label = n),
            color = 'white', fontface = 'bold', size = 3.3, family = 'Arial') +
  ggpath::geom_from_path(data = labels, aes(x = -1.15, y = team, path = logo),
                         width = .032, inherit.aes = FALSE) +
  geom_text(data = labels, aes(x = trips + .5, y = team, label = label), inherit.aes = FALSE,
            hjust = 0, fontface = 'bold', size = 3.6, color = '#242320', family = 'Arial') +
  geom_text(data = labels, aes(x = trips + 2.35, y = team, label = sub), inherit.aes = FALSE,
            hjust = 0, size = 3, color = '#8A8276', family = 'Arial') +
  annotate('text', x = max_trips + .5, y = 32.95, label = 'TD RATE', hjust = 0, vjust = 0,
           size = 2.9, fontface = 'bold', color = '#686158', family = 'Arial') +
  scale_fill_manual(values = fills, breaks = outcome_levels, name = NULL) +
  scale_x_continuous(breaks = seq(0, x_max, 5), expand = expansion(mult = 0)) +
  scale_y_discrete(expand = expansion(add = c(.7, 1.1))) +
  coord_cartesian(xlim = c(-2.2, x_max), clip = 'off') +
  labs(title = 'RED ZONE OUTCOMES BY TEAM',
       subtitle = sprintf('%s NFL regular season, %s  |  How every red zone trip ended, sorted by touchdowns',
                          season_id, week_label),
       x = 'Red zone trips', y = NULL,
       caption = sprintf(paste0('Red zone trip: a drive with at least one snap at the opponent\'s 20 or closer (PATs and two-point tries excluded). ',
         'League TD rate: %.1f%%.\n',
         'Turnover = interception or lost fumble (including returns for TDs). End of half = ran out of clock before halftime or game end.\n',
         'Source: nflverse / nflreadr  |  Plot: Ward Walton'), 100 * league_td_rate)) +
  theme_minimal(base_size = 13, base_family = 'Arial') +
  theme(plot.background = element_rect(fill = '#F5F0E6', color = NA),
        panel.background = element_rect(fill = '#F5F0E6', color = NA),
        panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = '#E4DDD1'),
        axis.text.y = element_blank(), axis.text.x = element_text(color = '#686158'),
        axis.title.x = element_text(face = 'bold', color = '#242320', margin = margin(t = 8)),
        legend.position = 'top', legend.justification = 'left', legend.direction = 'horizontal',
        legend.location = 'plot', legend.key.size = unit(.9, 'lines'),
        legend.text = element_text(color = '#242320', size = 11, margin = margin(r = 10, l = 4)),
        legend.margin = margin(0, 0, 4, 0),
        plot.title = element_text(face = 'bold', size = 24, color = '#242320'),
        plot.title.position = 'plot', plot.caption.position = 'plot',
        plot.subtitle = element_text(color = '#686158', margin = margin(b = 12)),
        plot.caption = element_text(hjust = 0, color = '#686158', size = 9.5, lineheight = 1.25, margin = margin(t = 12)),
        plot.margin = margin(22, 25, 15, 20))
png_name <- sprintf('red_zone_outcomes_%s_through_week_%s.png', season_id, week_id)
ggsave(file.path(out_dir, png_name), rz_plot, width = 11, height = 13, dpi = 180)

writeLines(c(
  sprintf('# %s red zone outcomes, REG %s', season_id, week_label),
  sprintf('Coverage: %s games, %s teams, %s red zone trips. League TD rate %.1f%%.',
          n_distinct(regular$game_id), nrow(team_summary), nrow(trips), 100 * league_td_rate),
  paste('nflverse source timestamp:', paste(source_time, collapse = ' ')),
  'Trip: offensive drive (fixed_drive) with any run/pass/FG/punt/kneel/spike/no_play snap at yardline_100 <= 20; PATs and 2-pt tries excluded.',
  'Outcome from fixed_drive_result: Touchdown; FG attempt = made + missed FG; Turnover = INT/lost fumble incl. Opp touchdown;',
  'Downs = turnover on downs; End of half = clock expired (plus any rare punt/safety). Every trip is in trip_audit.csv.',
  'Sorted by TD count, ties by TD rate. Totals are not per game: check the games column for bye weeks.',
  sprintf('Recreate: Rscript red_zone_outcomes.r %s %s %s', season_id, week_id, file.path(out_dir, 'source_pbp.rds'))
), file.path(out_dir, 'README.md'))
options(width = 200)
print(team_summary, n = 32)
message(sprintf('League red zone TD rate: %.1f%% on %d trips', 100 * league_td_rate, nrow(trips)))
message('Saved plot and audit data to ', out_dir)
