# Run: Rscript qb_air_yards_epa.r [season=2026] [through_week=latest] [min_attempts=150] [optional PBP .rds]
# QB air yards per attempt (aDOT) vs passing EPA per dropback.
# Packages: nflreadr, dplyr, ggplot2, ggpath, magick, ragg.
.libPaths(c('/private/tmp/nfl-chart-r-library', .libPaths()))
suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(grid)})
args <- commandArgs(trailingOnly = TRUE)
season_id <- if (length(args) >= 1) as.integer(args[1]) else 2026L
min_attempts <- if (length(args) >= 3) as.integer(args[3]) else 150L
pbp <- if (length(args) >= 4) readRDS(args[4]) else nflreadr::load_pbp(season_id)
source_time <- attr(pbp, 'nflverse_timestamp')
reg <- pbp |> filter(season == season_id, season_type == 'REG', !coalesce(play_deleted == 1, FALSE))
week_id <- if (length(args) >= 2) as.integer(args[2]) else max(reg$week)
stopifnot(!is.na(season_id), week_id %in% 1:18, min_attempts > 0)
reg <- reg |> filter(week <= week_id)

scheduled <- nflreadr::load_schedules(season_id) |> filter(game_type == 'REG', week == week_id) |> nrow()
played <- n_distinct(reg$game_id[reg$week == week_id])
partial <- played < scheduled
out_dir <- sprintf('outputs/qb_air_yards_epa_%s_through_week_%s%s', season_id, week_id, if (partial) '_partial' else '')
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(reg, file.path(out_dir, 'source_pbp.rds'))

# Dropbacks = attempts + sacks + scrambles (no spikes, 2-pt tries); WP 2-98% to match the other charts.
# EPA/play is per dropback. Air yards/att = mean intended air yards on attempts (sacks/scrambles excluded).
plays <- reg |>
  filter(qb_dropback == 1, !is.na(epa), coalesce(qb_spike, 0) == 0,
         coalesce(two_point_attempt, 0) == 0, between(wp, .02, .98)) |>
  mutate(qb_id = coalesce(passer_player_id, rusher_player_id),
         qb_name = coalesce(passer_player_name, rusher_player_name),
         is_attempt = pass_attempt == 1 & coalesce(sack, 0) == 0 & !is.na(air_yards)) |>
  filter(!is.na(qb_id))
stopifnot(!anyDuplicated(plays[c('game_id', 'play_id')]))
qbs <- plays |> group_by(player_id = qb_id) |>
  summarise(player = last(qb_name), team = names(which.max(table(posteam))),
            games = n_distinct(game_id), dropbacks = n(), attempts = sum(is_attempt),
            epa_per_play = mean(epa), air_yards_per_att = mean(air_yards[is_attempt]),
            .groups = 'drop') |>
  filter(attempts >= min_attempts) |>
  arrange(desc(epa_per_play))
stopifnot(nrow(qbs) >= 5)
lg_ay <- mean(plays$air_yards[plays$is_attempt]); lg_epa <- mean(plays$epa)
write.csv(qbs, file.path(out_dir, 'qb_summary.csv'), row.names = FALSE)
write.csv(plays |> select(game_id, play_id, posteam, qb_name, down, ydstogo, pass_attempt, sack,
                          qb_scramble, air_yards, is_attempt, epa),
          file.path(out_dir, 'play_audit.csv'), row.names = FALSE)

# Headshots: download once, crop to a circle with a team-color ring.
players <- nflreadr::load_players() |> select(gsis_id, headshot)
team_meta <- nflreadr::load_teams() |> select(team_abbr, team_color)
qbs <- qbs |> left_join(players, by = c(player_id = 'gsis_id')) |>
  left_join(team_meta, by = c(team = 'team_abbr'))
stopifnot(!anyNA(qbs$team_color))
shot_dir <- file.path(out_dir, 'headshots'); dir.create(shot_dir, showWarnings = FALSE)
qbs$photo <- vapply(seq_len(nrow(qbs)), function(i) {
  raw <- file.path(shot_dir, paste0(qbs$player_id[i], '_raw.png'))
  out <- file.path(shot_dir, paste0(qbs$player_id[i], '.png'))
  if (!file.exists(raw) && !is.na(qbs$headshot[i])) {
    url <- sub('f_auto,q_auto', 'f_png,q_auto,w_400', qbs$headshot[i], fixed = TRUE)
    try(download.file(url, raw, mode = 'wb', quiet = TRUE), silent = TRUE)
  }
  size <- 300; ring <- 14
  face <- if (file.exists(raw) && file.info(raw)$size > 0) {
    img <- magick::image_read(raw)
    info <- magick::image_info(img)
    side <- min(info$width, info$height)  # square crop from the top keeps the face centered
    img <- magick::image_crop(img, sprintf('%dx%d+%d+0', side, side, (info$width - side) %/% 2))
    magick::image_background(magick::image_resize(img, sprintf('%dx%d!', size, size)), '#E9E4D8')
  } else {
    message('No headshot for ', qbs$player[i], '; using a name badge.')
    magick::image_annotate(magick::image_blank(size, size, '#E9E4D8'), qbs$player[i],
                           gravity = 'center', size = 44, weight = 700, color = '#292923')
  }
  mask <- magick::image_draw(magick::image_blank(size, size, 'none'))
  symbols(size / 2, size / 2, circles = size / 2 - ring, bg = 'white', fg = NA,
          inches = FALSE, add = TRUE)
  dev.off()
  face <- magick::image_composite(face, mask, operator = 'CopyOpacity')
  canvas <- magick::image_draw(magick::image_blank(size, size, 'none'))
  symbols(size / 2, size / 2, circles = size / 2 - 1, bg = qbs$team_color[i], fg = NA,
          inches = FALSE, add = TRUE)
  dev.off()
  magick::image_write(magick::image_composite(canvas, face), out, format = 'png')
  out
}, character(1))

# Separate overlapping headshots in range-normalized units; leaders show true spots.
pad <- function(r) r + c(-1, 1) * diff(r) * .13
xl <- pad(range(qbs$air_yards_per_att)); yl <- pad(range(qbs$epa_per_play))
anchor <- cbind((qbs$air_yards_per_att - xl[1]) / diff(xl), (qbs$epa_per_play - yl[1]) / diff(yl))
pos <- anchor
min_distance <- .085; y_stretch <- 1.25
for (iteration in 1:800) {
  pos <- pos + .012 * (anchor - pos)
  for (i in 1:(nrow(pos) - 1)) for (j in (i + 1):nrow(pos)) {
    # Headshot + name label is taller than wide, so vertical gaps count for less.
    delta <- (pos[j, ] - pos[i, ]) / c(1, y_stretch); distance <- sqrt(sum(delta^2))
    if (distance < min_distance) {
      direction <- if (distance < 1e-10) c(1, 0) else delta / distance
      push <- direction * (min_distance - distance) * .5 * c(1, y_stretch)
      pos[i, ] <- pos[i, ] - push; pos[j, ] <- pos[j, ] + push
    }
  }
  pos <- pmin(pmax(pos, min_distance * .6), 1 - min_distance * .6)
}
qbs$img_x <- xl[1] + pos[, 1] * diff(xl); qbs$img_y <- yl[1] + pos[, 2] * diff(yl)
qbs$moved <- sqrt(rowSums((pos - anchor)^2)) > .006
qbs$last_name <- toupper(sub('^[^.]*\\.', '', qbs$player))
write.csv(qbs |> select(player, team, air_yards_per_att, epa_per_play, img_x, img_y, moved),
          file.path(out_dir, 'headshot_positions.csv'), row.names = FALSE)

bg <- '#F7F3EA'; ink <- '#292923'; muted <- '#716E64'; accent <- '#2E5E8C'
corner <- function(label, x, y, hjust, vjust, color = '#A39D8E') {
  annotate('text', x, y, label = label, hjust = hjust, vjust = vjust, family = 'Arial',
           fontface = 'bold', size = 3.0, color = color, lineheight = .9)
}
img_w <- .078
plot <- ggplot() +
  geom_hline(yintercept = lg_epa, color = '#898477', linewidth = .35, linetype = 'dashed', alpha = .65) +
  geom_vline(xintercept = lg_ay, color = '#898477', linewidth = .35, linetype = 'dashed', alpha = .65) +
  corner('AGGRESSIVE\n& EFFICIENT', xl[2], yl[2], 1.05, 1.4, accent) +
  corner('DINK & DUNK\nTHAT WORKS', xl[1], yl[2], -.06, 1.4) +
  corner('AGGRESSIVE\n& INEFFICIENT', xl[2], yl[1], 1.05, -.6) +
  corner('CONSERVATIVE\n& INEFFICIENT', xl[1], yl[1], -.06, -.6) +
  geom_segment(data = filter(qbs, moved), aes(air_yards_per_att, epa_per_play, xend = img_x, yend = img_y),
               color = muted, linewidth = .4) +
  geom_point(data = filter(qbs, moved), aes(air_yards_per_att, epa_per_play), color = muted, size = 1.1) +
  ggpath::geom_from_path(data = qbs, aes(img_x, img_y, path = photo), width = img_w) +
  geom_text(data = qbs, aes(img_x, img_y - diff(yl) * img_w * .62 * 1.0, label = last_name),
            family = 'Arial', fontface = 'bold', size = 1.9, color = ink, vjust = 1) +
  scale_x_continuous(labels = scales::label_number(accuracy = 1)) +
  scale_y_continuous(labels = scales::label_number(accuracy = .01, style_positive = 'plus')) +
  coord_cartesian(xlim = xl, ylim = yl, expand = FALSE, clip = 'off') +
  labs(x = 'AIR YARDS PER ATTEMPT  →', y = 'EPA PER DROPBACK  →') +
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
                 r = unit(.008, 'npc'), gp = gpar(fill = accent, col = NA))
  txt(sprintf('%s  /  THRU WEEK %02d%s', season_id, week_id, if (partial) '*' else ''), .225, .928, 13, bg, 'bold', 'center')
  txt('AGGRESSION\nVS. EFFICIENCY', .06, .845, 34, face = 'bold')
  grid.lines(x = c(.06, .92), y = c(.775, .775), gp = gpar(col = '#C7C0B1', lwd = 1))
  txt(sprintf('QB AIR YARDS PER ATTEMPT VS. EPA/PLAY  ·  MIN. %s ATT.', min_attempts), .065, .754, 12, face = 'bold')
  print(plot, newpage = FALSE, vp = viewport(x = .485, y = .455, width = .90, height = .58))
  txt('Dashed lines = league average. Air yards = intended air yards on pass attempts.', .065, .135, 10, muted)
  txt('EPA per dropback incl. sacks & scrambles. No spikes or 2-pt tries; win prob. 2–98%.', .065, .115, 10, muted)
  if (partial) txt(sprintf('*Week %s in progress: %s of %s games included.', week_id, played, scheduled),
                   .065, .095, 10, muted, 'bold')
  txt('SOURCE: NFLVERSE', .065, .065, 11, muted, 'bold')
  txt('WARD WALTON', .92, .065, 11, muted, 'bold', 'right')
  dev.off()
}
draw_poster(file.path(out_dir, 'qb_air_yards_epa_tiktok.png'))
draw_poster(file.path(out_dir, 'qb_air_yards_epa_tiktok_2x.png'), 2)
writeLines(c(
  sprintf('# %s QB air yards per attempt vs EPA per dropback, REG weeks 1-%s%s', season_id, week_id,
          if (partial) sprintf(' (week %s partial: %s of %s games)', week_id, played, scheduled) else ''),
  sprintf('Coverage: %s games, %s qualifying QBs (min. %s attempts), %s league dropbacks.',
          n_distinct(reg$game_id), nrow(qbs), min_attempts, nrow(plays)),
  paste('nflverse source timestamp:', paste(source_time, collapse = ' ')),
  'Source: https://nflreadr.nflverse.com/reference/load_pbp.html',
  'Dropbacks = qb_dropback == 1 (attempts, sacks, scrambles) excluding spikes, two-point tries; WP 2-98% inclusive.',
  'EPA/play = mean EPA per dropback. Air yards/att = mean air_yards on non-sack pass attempts (incompletions included).',
  sprintf('League average: %.2f air yards/att, %+.3f EPA/dropback (dashed lines).', lg_ay, lg_epa),
  'QB team = team with most of his attempts. Headshots separated deterministically; leaders/dots show true spots.',
  sprintf('Recreate: Rscript qb_air_yards_epa.r %s %s %s %s', season_id, week_id, min_attempts,
          file.path(out_dir, 'source_pbp.rds'))
), file.path(out_dir, 'README.md'))
print(qbs |> select(player, team, dropbacks, attempts, air_yards_per_att, epa_per_play), n = 50)
message(sprintf('League avg %.2f air yds/att / %+.3f EPA/dropback', lg_ay, lg_epa))
message('Saved chart and audit files to ', out_dir)
