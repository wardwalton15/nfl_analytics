# Run: Rscript turnover_differential.r [season=2026] [through_week=latest] [optional PBP .rds]
# Turnover differential (takeaways - giveaways) for every team, as diverging bars with logos.
# Packages: nflreadr, dplyr, ggplot2, ggpath, ragg.
suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(grid)})
args <- commandArgs(trailingOnly = TRUE)
season_id <- if (length(args) >= 1) as.integer(args[1]) else 2026L
pbp <- if (length(args) >= 3) readRDS(args[3]) else nflreadr::load_pbp(season_id)
source_time <- attr(pbp, 'nflverse_timestamp')
reg <- pbp |> filter(season == season_id, season_type == 'REG', !coalesce(play_deleted == 1, FALSE))
week_id <- if (length(args) >= 2) as.integer(args[2]) else max(reg$week)
stopifnot(!is.na(season_id), week_id %in% 1:18)
reg <- reg |> filter(week <= week_id)

# Flag a week still in progress by comparing played games with the schedule.
scheduled <- nflreadr::load_schedules(season_id) |> filter(game_type == 'REG', week == week_id) |> nrow()
played <- n_distinct(reg$game_id[reg$week == week_id])
partial <- played < scheduled
out_dir <- sprintf('outputs/turnover_differential_%s_through_week_%s%s',
                   season_id, week_id, if (partial) '_partial' else '')
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(reg, file.path(out_dir, 'source_pbp.rds'))

# Turnovers = interceptions + lost fumbles on any play (offense, defense or special teams).
# Plays nullified by penalty (no_play) do not count. A fumble lost by the returner after
# an interception counts as a second turnover, matching official scoring.
live <- reg |> filter(coalesce(play_type, '') != 'no_play', !is.na(home_team))
other <- function(team, home, away) if_else(team == home, away, home)
ints <- live |> filter(coalesce(interception, 0) == 1) |>
  transmute(game_id, play_id, week, kind = 'INT', giver = posteam, taker = defteam, desc)
fums <- live |> filter(coalesce(fumble_lost, 0) == 1) |>
  transmute(game_id, play_id, week, kind = 'FUM', giver = fumbled_1_team,
            taker = other(fumbled_1_team, home_team, away_team), desc)
turnovers <- bind_rows(ints, fums)
stopifnot(!anyNA(turnovers$giver), !anyNA(turnovers$taker), all(turnovers$giver != turnovers$taker))
write.csv(turnovers, file.path(out_dir, 'turnover_audit.csv'), row.names = FALSE)

games <- bind_rows(distinct(reg, game_id, team = home_team), distinct(reg, game_id, team = away_team)) |>
  count(team, name = 'games')
teams <- games |>
  left_join(turnovers |> count(team = taker, kind) |>
              tidyr::pivot_wider(names_from = kind, values_from = n, names_prefix = 'take_'), by = 'team') |>
  left_join(turnovers |> count(team = giver, kind) |>
              tidyr::pivot_wider(names_from = kind, values_from = n, names_prefix = 'give_'), by = 'team') |>
  mutate(across(matches('^(take|give)_'), \(x) coalesce(x, 0L)),
         takeaways = take_INT + take_FUM, giveaways = give_INT + give_FUM,
         differential = takeaways - giveaways) |>
  arrange(desc(differential), desc(takeaways), team)
stopifnot(nrow(teams) == 32L, sum(teams$differential) == 0L, sum(teams$takeaways) == nrow(turnovers))
write.csv(teams, file.path(out_dir, 'team_summary.csv'), row.names = FALSE)

# Reuse existing logos, then download missing ones from nflverse team metadata.
meta <- nflreadr::load_teams()
logo_dir <- file.path(out_dir, 'logos')
dir.create(logo_dir, showWarnings = FALSE)
existing_dirs <- c('outputs/epa_team_tiers_trend_2026_entering_2026-11-08/logos',
                   'outputs/epa_team_tiers_2026_entering_2026-11-01/logos')
for (tm in teams$team) {
  dest <- file.path(logo_dir, paste0(tm, '.png'))
  existing <- file.path(existing_dirs, paste0(tm, '.png'))
  existing <- existing[file.exists(existing)]
  if (!file.exists(dest) && length(existing)) file.copy(existing[1], dest)
  if (!file.exists(dest)) {
    url <- meta$team_logo_espn[match(tm, meta$team_abbr)]
    if (is.na(url)) stop('No logo available for ', tm)
    download.file(url, dest, mode = 'wb', quiet = TRUE)
  }
}
teams$logo <- file.path(logo_dir, paste0(teams$team, '.png'))
teams$color <- meta$team_color[match(teams$team, meta$team_abbr)]
stopifnot(!anyNA(teams$color))

# Best differential on top. Logo sits just past the end of each bar; value label past the logo.
teams <- teams |>
  mutate(row = rev(seq_len(n())), side = if_else(differential < 0, -1, 1),
         label = if_else(differential > 0, paste0('+', differential),
                         if_else(differential < 0, paste0('−', abs(differential)), '0')))
max_abs <- max(abs(teams$differential), 1)
lim <- ceiling(max_abs * 1.28)
logo_gap <- lim * .08; label_gap <- lim * .165
teams <- teams |> mutate(logo_x = differential + side * logo_gap,
                         label_x = differential + side * label_gap)

bg <- '#F7F3EA'; ink <- '#292923'; muted <- '#716E64'; good <- '#2E7058'; bad <- '#944D4B'
plot <- ggplot(teams) +
  geom_vline(xintercept = seq(-lim, lim, by = 5)[seq(-lim, lim, by = 5) != 0],
             color = '#DDD6C7', linewidth = .3) +
  geom_rect(aes(xmin = pmin(0, differential), xmax = pmax(0, differential),
                ymin = row - .36, ymax = row + .36, fill = color), alpha = .55) +
  geom_vline(xintercept = 0, color = ink, linewidth = .5) +
  ggpath::geom_from_path(aes(logo_x, row, path = logo), height = .034) +
  geom_text(aes(label_x, row, label = label, hjust = if_else(side < 0, 1, 0)),
            family = 'Arial', fontface = 'bold', size = 3.6, color = ink) +
  scale_fill_identity() +
  scale_x_continuous(breaks = seq(-lim, lim, by = 5),
                     labels = \(x) if_else(x > 0, paste0('+', x), if_else(x < 0, paste0('−', abs(x)), '0'))) +
  coord_cartesian(xlim = c(-lim, lim), ylim = c(.4, 32.6), expand = FALSE, clip = 'off') +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = 'Arial', base_size = 12) +
  theme(plot.background = element_rect(fill = bg, color = NA),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = muted, size = 11),
        plot.margin = margin(5, 5, 5, 5))

draw_poster <- function(path, scale = 1) {
  ragg::agg_png(path, width = 1080 * scale, height = 1920 * scale, res = 144 * scale, background = bg)
  grid.newpage()
  txt <- function(label, x, y, size, color = ink, face = 'plain', just = 'left') {
    grid.text(label, x, y, just = just,
              gp = gpar(fontfamily = 'Arial', fontsize = size, col = color, fontface = face))
  }
  grid.roundrect(x = .225, y = .928, width = .33, height = .029,
                 r = unit(.008, 'npc'), gp = gpar(fill = good, col = NA))
  txt(sprintf('%s  /  THRU WEEK %02d%s', season_id, week_id, if (partial) '*' else ''),
      .225, .928, 13, bg, 'bold', 'center')
  txt('TURNOVER\nDIFFERENTIAL', .06, .860, 36, face = 'bold')
  grid.lines(x = c(.06, .92), y = c(.793, .793), gp = gpar(col = '#C7C0B1', lwd = 1))
  txt('←  MORE GIVEAWAYS', .065, .774, 12, bad, 'bold')
  txt('MORE TAKEAWAYS  →', .92, .774, 12, good, 'bold', 'right')
  print(plot, newpage = FALSE, vp = viewport(x = .49, y = .47, width = .90, height = .585))
  txt('Differential = takeaways (INTs + fumbles recovered) minus giveaways.',
      .065, .152, 10, muted)
  txt('Includes special teams. Plays wiped out by penalty do not count.', .065, .133, 10, muted)
  if (partial) txt(sprintf('*Week %s in progress: %s of %s games included.', week_id, played, scheduled),
                   .065, .112, 10, muted, 'bold')
  txt('SOURCE: NFLVERSE', .065, .072, 11, muted, 'bold')
  txt('WARD WALTON', .92, .072, 11, muted, 'bold', 'right')
  dev.off()
}
draw_poster(file.path(out_dir, 'turnover_differential_tiktok.png'))
draw_poster(file.path(out_dir, 'turnover_differential_tiktok_2x.png'), 2)
writeLines(c(
  sprintf('# %s turnover differential, REG weeks 1-%s%s', season_id, week_id,
          if (partial) sprintf(' (week %s partial: %s of %s games)', week_id, played, scheduled) else ''),
  sprintf('Coverage: %s games, %s teams, %s turnovers (%s INT, %s lost fumbles).',
          n_distinct(reg$game_id), nrow(teams), nrow(turnovers), nrow(ints), nrow(fums)),
  paste('nflverse source timestamp:', paste(source_time, collapse = ' ')),
  'Source: https://nflreadr.nflverse.com/reference/load_pbp.html',
  'Giveaway: interception thrown (posteam) or fumble_lost == 1 (charged to fumbled_1_team).',
  'Takeaway: the opponent in that game. All phases count, including special teams and returns.',
  'Excluded: deleted plays and play_type no_play (turnovers nullified by penalty).',
  'Season totals, not per game: teams with an extra bye have played fewer games (see games column).',
  'Every play is in turnover_audit.csv; differentials sum to zero across the league.',
  'Bars use nflverse team_color at 55% opacity. Ties ordered by takeaways, then team.',
  sprintf('Recreate: Rscript turnover_differential.r %s %s %s', season_id, week_id,
          file.path(out_dir, 'source_pbp.rds'))
), file.path(out_dir, 'README.md'))
print(teams |> select(team, games, takeaways, giveaways, differential), n = 32)
message('Saved chart and audit files to ', out_dir)
