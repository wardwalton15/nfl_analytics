# Run: Rscript explosive_three_and_out.r [season=2026] [through_week=7] [optional PBP .rds]
# Packages: nflreadr, dplyr, ggplot2, ggpath, ragg.
suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(grid)})
args <- commandArgs(trailingOnly = TRUE)
season_id <- if (length(args) >= 1) as.integer(args[1]) else 2026L
through_week <- if (length(args) >= 2) as.integer(args[2]) else 7L
stopifnot(!is.na(season_id), !is.na(through_week), through_week %in% 1:18)
out_dir <- sprintf('outputs/explosive_three_and_out_%s_through_week_%s', season_id, through_week)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
cache <- file.path(out_dir, 'source_pbp.rds')
source_path <- if (length(args) >= 3) args[3] else if (file.exists(cache)) cache else NULL
pbp <- if (!is.null(source_path)) readRDS(source_path) else nflreadr::load_pbp(season_id)
source_time <- attr(pbp, 'nflverse_timestamp')
season_pbp <- pbp |>
  filter(season == season_id, season_type == 'REG', week <= through_week,
         !coalesce(play_deleted == 1, FALSE))
stopifnot(!anyDuplicated(season_pbp[c('game_id', 'play_id')]),
          max(season_pbp$week) == through_week)
saveRDS(season_pbp, cache)

# Explosive: 10+ yard runs (scrambles included) and 20+ yard passes.
# Yards gained excludes penalty yardage; no-plays and two-point tries are not counted.
plays <- season_pbp |>
  filter(play_type %in% c('run', 'pass'), !is.na(posteam),
         coalesce(two_point_attempt, 0) == 0) |>
  mutate(explosive = coalesce((play_type == 'run' & yards_gained >= 10) |
                                (play_type == 'pass' & yards_gained >= 20), FALSE))
# Three-and-out: drive with zero first downs (including by penalty) ending in a punt,
# matching three_and_out_week_1.r.
drives <- season_pbp |> filter(!is.na(posteam), !is.na(fixed_drive)) |>
  group_by(game_id, posteam, fixed_drive) |>
  summarise(first_downs = sum(first_down, na.rm = TRUE),
            result = first(na.omit(fixed_drive_result), default = NA_character_),
            result_count = n_distinct(fixed_drive_result, na.rm = TRUE), .groups = 'drop') |>
  mutate(three_and_out = coalesce(first_downs == 0 & result == 'Punt', FALSE))
stopifnot(all(drives$result_count <= 1))

teams <- plays |> group_by(team = posteam) |>
  summarise(games = n_distinct(game_id), plays = n(),
            explosive_runs = sum(explosive & play_type == 'run'),
            explosive_passes = sum(explosive & play_type == 'pass'),
            explosive_plays = sum(explosive), .groups = 'drop') |>
  inner_join(drives |> group_by(team = posteam) |>
               summarise(drives = n(), three_and_outs = sum(three_and_out), .groups = 'drop'),
             by = 'team') |>
  mutate(explosive_per_game = explosive_plays / games,
         three_and_outs_per_game = three_and_outs / games,
         explosive_rate = explosive_plays / plays,
         three_and_out_rate = three_and_outs / drives) |>
  arrange(desc(explosive_per_game), team)
stopifnot(nrow(teams) == 32L, sum(teams$plays) == nrow(plays))
write.csv(teams, file.path(out_dir, 'team_summary.csv'), row.names = FALSE)
write.csv(drives, file.path(out_dir, 'drive_audit.csv'), row.names = FALSE)
write.csv(plays |> filter(explosive) |>
            select(game_id, play_id, week, posteam, defteam, play_type, qb_scramble, yards_gained, desc),
          file.path(out_dir, 'explosive_play_audit.csv'), row.names = FALSE)

# Reuse existing logos, then download missing ones from nflverse team metadata.
logo_dir <- file.path(out_dir, 'logos')
dir.create(logo_dir, showWarnings = FALSE)
existing_dirs <- c('outputs/epa_team_tiers_2026_entering_2026-11-01/logos',
                   'outputs/def_epa_success_2026_through_week_2/logos')
for (tm in teams$team) {
  dest <- file.path(logo_dir, paste0(tm, '.png'))
  existing <- file.path(existing_dirs, paste0(tm, '.png'))
  existing <- existing[file.exists(existing)]
  if (!file.exists(dest) && length(existing)) file.copy(existing[1], dest)
  if (!file.exists(dest)) {
    meta <- nflreadr::load_teams()
    url <- meta$team_logo_espn[match(tm, meta$team_abbr)]
    if (is.na(url)) stop('No logo available for ', tm)
    download.file(url, dest, mode = 'wb', quiet = TRUE)
  }
}
teams$logo <- file.path(logo_dir, paste0(teams$team, '.png'))

# Square panel; three-and-outs are negated so up/right always means better.
pad <- .12
xr <- range(teams$explosive_per_game); yr <- range(-teams$three_and_outs_per_game)
xl <- xr + c(-1, 1) * diff(xr) * pad; yl <- yr + c(-1, 1) * diff(yr) * pad
x_mean <- mean(teams$explosive_per_game); y_mean <- -mean(teams$three_and_outs_per_game)

# Deterministic collision resolution in panel-normalized units.
# Dots and leaders preserve true coordinates.
anchor <- cbind((teams$explosive_per_game - xl[1]) / diff(xl),
                (-teams$three_and_outs_per_game - yl[1]) / diff(yl))
pos <- anchor
min_distance <- .078
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
  pos[] <- pmax(min_distance / 2, pmin(1 - min_distance / 2, pos))
}
teams$logo_x <- xl[1] + pos[, 1] * diff(xl)
teams$logo_y <- yl[1] + pos[, 2] * diff(yl)
teams$moved <- sqrt(rowSums((pos - anchor)^2)) > .006
write.csv(teams |> select(team, explosive_per_game, three_and_outs_per_game, logo_x, logo_y, moved),
          file.path(out_dir, 'logo_positions.csv'), row.names = FALSE)

bg <- '#F7F3EA'; ink <- '#292923'; muted <- '#716E64'; good <- '#2E7058'; bad <- '#944D4B'
quadrants <- data.frame(
  x = c(xl[2], xl[1], xl[2], xl[1]), y = c(yl[2], yl[2], yl[1], yl[1]),
  hjust = c(1, 0, 1, 0), vjust = c(1, 1, 0, 0), color = c(good, muted, muted, bad),
  label = c('BIG PLAYS,\nFEW PUNTS', 'GRINDING IT OUT', 'BOOM OR BUST', 'STUCK IN NEUTRAL'))
plot <- ggplot() +
  geom_hline(yintercept = y_mean, color = '#898477', linewidth = .35, linetype = 'dashed', alpha = .65) +
  geom_vline(xintercept = x_mean, color = '#898477', linewidth = .35, linetype = 'dashed', alpha = .65) +
  geom_text(data = quadrants, aes(x, y, label = label, hjust = hjust, vjust = vjust),
            color = quadrants$color, family = 'Arial', fontface = 'bold', size = 3.3,
            lineheight = .9, nudge_x = c(-1, 1, -1, 1) * diff(xl) * .015,
            nudge_y = c(-1, -1, 1, 1) * diff(yl) * .015) +
  geom_segment(data = filter(teams, moved),
               aes(x = explosive_per_game, y = -three_and_outs_per_game, xend = logo_x, yend = logo_y),
               color = muted, linewidth = .4) +
  geom_point(data = filter(teams, moved), aes(explosive_per_game, -three_and_outs_per_game),
             color = muted, size = 1.1) +
  ggpath::geom_from_path(data = teams, aes(logo_x, logo_y, path = logo), width = .070) +
  scale_x_continuous(breaks = scales::breaks_width(1), labels = scales::label_number(accuracy = 1)) +
  scale_y_continuous(breaks = scales::breaks_width(.5), labels = function(x) sprintf('%.1f', -x)) +
  coord_cartesian(xlim = xl, ylim = yl, expand = FALSE) +
  labs(x = 'EXPLOSIVE PLAYS / GAME  →', y = 'FEWER 3-AND-OUTS / GAME  →') +
  theme_minimal(base_family = 'Arial', base_size = 12) +
  theme(plot.background = element_rect(fill = bg, color = NA),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(fill = NA, color = '#C7C0B1', linewidth = .5),
        aspect.ratio = 1,
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
  grid.roundrect(x = .205, y = .928, width = .29, height = .029,
                 r = unit(.008, 'npc'), gp = gpar(fill = good, col = NA))
  txt(sprintf('%s  /  THRU WEEK %s', season_id, through_week), .205, .928, 13, bg, 'bold', 'center')
  txt('BIG PLAYS VS', .06, .868, 36, face = 'bold')
  txt('3-AND-OUTS', .06, .832, 36, face = 'bold')
  grid.lines(x = c(.06, .92), y = c(.795, .795), gp = gpar(col = '#C7C0B1', lwd = 1))
  txt('NFL OFFENSES, PER GAME', .065, .774, 12, face = 'bold')
  txt('BETTER  ↗', .92, .774, 12, good, 'bold', 'right')
  print(plot, newpage = FALSE, vp = viewport(x = .485, y = .51, width = .90, height = .515))
  txt('EXPLOSIVE = 10+ YD RUN OR 20+ YD PASS', .065, .222, 11, muted, 'bold')
  txt('3-AND-OUT = DRIVE WITH NO FIRST DOWN THAT ENDS IN A PUNT', .065, .198, 11, muted, 'bold')
  txt('DASHED LINES = LEAGUE AVERAGE', .065, .174, 11, muted, 'bold')
  txt('SOURCE: NFLVERSE', .065, .108, 11, muted, 'bold')
  txt('WARD WALTON', .92, .108, 11, muted, 'bold', 'right')
  dev.off()
}
draw_poster(file.path(out_dir, 'explosive_three_and_out_tiktok.png'))
draw_poster(file.path(out_dir, 'explosive_three_and_out_tiktok_2x.png'), 2)
writeLines(c(
  sprintf('# %s explosive plays vs three-and-outs, REG weeks 1-%s', season_id, through_week),
  sprintf('Coverage: %s games, %s teams, %s run/pass plays, %s drives.',
          n_distinct(season_pbp$game_id), nrow(teams), nrow(plays), nrow(drives)),
  paste('nflverse source timestamp:', paste(source_time, collapse = ' ')),
  'Source: https://nflreadr.nflverse.com/reference/load_pbp.html',
  'Explosive: run play_type (scrambles included) with 10+ yards gained, or pass play_type with 20+.',
  'Yards gained excludes penalty yardage. No-plays, two-point tries and deleted plays excluded.',
  'Three-and-out: fixed_drive with zero first downs (incl. by penalty) and fixed_drive_result Punt.',
  'Both axes are per game played (teams have different bye weeks). Rates per play/drive in team_summary.csv.',
  'Y-axis is reversed so fewer three-and-outs is higher. Dashed lines = unweighted team means.',
  'Logos are separated deterministically for readability. Leaders/dots show displaced true locations.',
  'PNG exports: 1080 x 1920 and 2160 x 3840.',
  sprintf('Recreate: Rscript explosive_three_and_out.r %s %s', season_id, through_week)
), file.path(out_dir, 'README.md'))
print(teams |> select(team, games, explosive_per_game, three_and_outs_per_game), n = 32)
message('Saved chart and audit files to ', out_dir)
