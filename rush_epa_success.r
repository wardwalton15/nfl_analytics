# Run: Rscript rush_epa_success.r [season=2026] [through_week=latest] [optional PBP .rds]
# Offensive rush EPA/play vs rush success rate (designed runs) for every team.
# Packages: nflreadr, dplyr, ggplot2, ggpath, ragg (and ggpath dependencies).
.libPaths(c('/private/tmp/nfl-chart-r-library', .libPaths()))
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
out_dir <- sprintf('outputs/rush_epa_success_%s_through_week_%s%s', season_id, week_id, if (partial) '_partial' else '')
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(reg, file.path(out_dir, 'source_pbp.rds'))

# Designed runs only (dropbacks, incl. scrambles, excluded); same filters as rush_pass_epa.r.
# Success = positive EPA.
plays <- reg |>
  filter(play_type %in% c('run', 'pass'), is.finite(epa), !is.na(posteam),
         coalesce(qb_dropback, 0) == 0,
         coalesce(qb_kneel, 0) == 0, coalesce(qb_spike, 0) == 0,
         coalesce(two_point_attempt, 0) == 0, between(wp, .02, .98)) |>
  mutate(success = epa > 0)
stopifnot(!anyDuplicated(plays[c('game_id', 'play_id')]))
teams <- plays |> group_by(team = posteam) |>
  summarise(games = n_distinct(game_id), rush_plays = n(),
            rush_epa = mean(epa), rush_success = mean(success),
            yards_per_carry = mean(yards_gained), .groups = 'drop') |>
  arrange(desc(rush_epa))
stopifnot(nrow(teams) == 32, all(is.finite(c(teams$rush_epa, teams$rush_success))))
lg_epa <- mean(plays$epa); lg_success <- mean(plays$success)
write.csv(teams, file.path(out_dir, 'team_summary.csv'), row.names = FALSE)
write.csv(plays |> select(game_id, play_id, posteam, defteam, rusher_player_name, yards_gained, epa, success),
          file.path(out_dir, 'play_audit.csv'), row.names = FALSE)

# Reuse existing logos, then download missing ones from nflverse team metadata.
logo_dir <- file.path(out_dir, 'logos')
dir.create(logo_dir, showWarnings = FALSE)
for (tm in teams$team) {
  dest <- file.path(logo_dir, paste0(tm, '.png'))
  existing <- file.path('outputs/first_down_drives_2025/logos', paste0(tm, '.png'))
  if (!file.exists(dest) && file.exists(existing)) file.copy(existing, dest)
  if (!file.exists(dest)) {
    meta <- nflreadr::load_teams()
    download.file(meta$team_logo_espn[match(tm, meta$team_abbr)], dest, mode = 'wb', quiet = TRUE)
  }
}
teams$logo <- file.path(logo_dir, paste0(teams$team, '.png'))

# Axes have different spreads, so collisions are resolved in range-normalized units.
pad <- function(r) r + c(-1, 1) * diff(r) * .09
xl <- pad(range(teams$rush_success)); yl <- pad(range(teams$rush_epa))
anchor <- cbind((teams$rush_success - xl[1]) / diff(xl), (teams$rush_epa - yl[1]) / diff(yl))
pos <- anchor
min_distance <- .075
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
teams$logo_x <- xl[1] + pos[, 1] * diff(xl); teams$logo_y <- yl[1] + pos[, 2] * diff(yl)
teams$moved <- sqrt(rowSums((pos - anchor)^2)) > .006
write.csv(teams |> select(team, rush_success, rush_epa, logo_x, logo_y, moved),
          file.path(out_dir, 'logo_positions.csv'), row.names = FALSE)

bg <- '#F7F3EA'; ink <- '#292923'; muted <- '#716E64'; green <- '#2E7058'
corner <- function(label, x, y, hjust, vjust, color = '#A39D8E') {
  annotate('text', x, y, label = label, hjust = hjust, vjust = vjust, family = 'Arial',
           fontface = 'bold', size = 3.1, color = color)
}
plot <- ggplot() +
  annotate('rect', xmin = lg_success, xmax = xl[2], ymin = lg_epa, ymax = yl[2], fill = '#E0E7CF') +
  annotate('rect', xmin = xl[1], xmax = lg_success, ymin = yl[1], ymax = lg_epa, fill = '#EED8D3') +
  geom_hline(yintercept = lg_epa, color = '#898477', linewidth = .35, linetype = 'dashed', alpha = .65) +
  geom_vline(xintercept = lg_success, color = '#898477', linewidth = .35, linetype = 'dashed', alpha = .65) +
  corner('BEST RUN GAMES', xl[2], yl[2], 1.08, 1.8, green) +
  corner('BOOM OR BUST', xl[1], yl[2], -.08, 1.8) +
  corner('STEADY, NO PUNCH', xl[2], yl[1], 1.06, -1) +
  corner('CAN\'T RUN IT', xl[1], yl[1], -.08, -1) +
  geom_segment(data = filter(teams, moved), aes(rush_success, rush_epa, xend = logo_x, yend = logo_y),
               color = muted, linewidth = .4) +
  geom_point(data = filter(teams, moved), aes(rush_success, rush_epa), color = muted, size = 1.1) +
  ggpath::geom_from_path(data = teams, aes(logo_x, logo_y, path = logo), width = .070) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_y_continuous(labels = scales::label_number(accuracy = .01, style_negative = 'minus')) +
  coord_cartesian(xlim = xl, ylim = yl, expand = FALSE) +
  labs(x = 'RUSH SUCCESS RATE  →', y = 'RUSH EPA / PLAY  →') +
  theme_minimal(base_family = 'Arial', base_size = 12) +
  theme(plot.background = element_rect(fill = bg, color = NA),
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, color = '#C7C0B1', linewidth = .5),
        axis.text = element_text(color = muted, size = 11),
        axis.title = element_text(color = ink, size = 12, face = 'bold'),
        axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.margin = margin(5, 5, 5, 5))

draw_poster <- function(path, scale = 1) {
  ragg::agg_png(path, width = 1080 * scale, height = 1920 * scale, res = 144 * scale, background = bg)
  grid.newpage()
  txt <- function(label, x, y, size, color = ink, face = 'plain', just = 'left') {
    grid.text(label, x, y, just = just,
              gp = gpar(fontfamily = 'Arial', fontsize = size, col = color, fontface = face))
  }
  grid.roundrect(x = .225, y = .928, width = .33, height = .029,
                 r = unit(.008, 'npc'), gp = gpar(fill = green, col = NA))
  txt(sprintf('%s  /  THRU WEEK %02d%s', season_id, week_id, if (partial) '*' else ''), .225, .928, 13, bg, 'bold', 'center')
  txt('WHO CAN\nRUN THE BALL', .06, .845, 34, face = 'bold')
  grid.lines(x = c(.06, .92), y = c(.775, .775), gp = gpar(col = '#C7C0B1', lwd = 1))
  txt('OFFENSIVE RUSHING, PER PLAY', .065, .754, 12, face = 'bold')
  txt('BETTER  ↗', .92, .754, 12, green, 'bold', 'right')
  print(plot, newpage = FALSE, vp = viewport(x = .485, y = .49, width = .90, height = .50))
  txt('Dashed lines = league average. Success = play with positive EPA.', .065, .205, 10, muted)
  txt('Designed runs only (no scrambles). Win prob. 2–98%; no kneels or 2-pt tries.', .065, .185, 10, muted)
  if (partial) txt(sprintf('*Week %s in progress: %s of %s games included.', week_id, played, scheduled),
                   .065, .162, 10, muted, 'bold')
  txt('SOURCE: NFLVERSE', .065, .108, 11, muted, 'bold')
  txt('WARD WALTON', .92, .108, 11, muted, 'bold', 'right')
  dev.off()
}
draw_poster(file.path(out_dir, 'rush_epa_success_tiktok.png'))
draw_poster(file.path(out_dir, 'rush_epa_success_tiktok_2x.png'), 2)
writeLines(c(
  sprintf('# %s rush EPA/play vs rush success rate, REG weeks 1-%s%s', season_id, week_id,
          if (partial) sprintf(' (week %s partial: %s of %s games)', week_id, played, scheduled) else ''),
  sprintf('Coverage: %s games, %s teams, %s designed runs.', n_distinct(reg$game_id), nrow(teams), nrow(plays)),
  paste('nflverse source timestamp:', paste(source_time, collapse = ' ')),
  'Source: https://nflreadr.nflverse.com/reference/load_pbp.html',
  'Rush = run/pass plays with qb_dropback == 0 (designed runs; scrambles and sacks excluded).',
  'Success = EPA > 0.',
  sprintf('League average: %.3f EPA/rush, %.1f%% success (dashed lines).', lg_epa, 100 * lg_success),
  'Only plays with offensive win probability between 2% and 98% (inclusive) are counted.',
  'Exclude deleted plays, kneels, spikes, two-point attempts, no-plays and special teams.',
  'No opponent adjustment. Logos are separated deterministically; leaders/dots show true locations.',
  'source_pbp.rds stores these games so a rerun with it reproduces the chart without downloading.',
  sprintf('Recreate: Rscript rush_epa_success.r %s %s %s', season_id, week_id, file.path(out_dir, 'source_pbp.rds'))
), file.path(out_dir, 'README.md'))
print(teams |> select(team, games, rush_plays, rush_epa, rush_success, yards_per_carry), n = 32)
message(sprintf('League avg %.3f EPA/rush / %.1f%% success', lg_epa, 100 * lg_success))
message('Saved chart and audit files to ', out_dir)
