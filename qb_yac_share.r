# Run: Rscript qb_yac_share.r [season=2026] [through_week=latest] [min_attempts=150] [top_n=10] [optional PBP .rds]
# QBs with the highest share of passing yards gained after the catch (YAC / passing yards).
# Packages: nflreadr, dplyr, magick, ragg.
.libPaths(c('/private/tmp/nfl-chart-r-library', .libPaths()))
suppressPackageStartupMessages({library(dplyr); library(grid)})
args <- commandArgs(trailingOnly = TRUE)
season_id <- if (length(args) >= 1) as.integer(args[1]) else 2026L
min_attempts <- if (length(args) >= 3) as.integer(args[3]) else 150L
top_n <- if (length(args) >= 4) as.integer(args[4]) else 10L
pbp <- if (length(args) >= 5) readRDS(args[5]) else nflreadr::load_pbp(season_id)
source_time <- attr(pbp, 'nflverse_timestamp')
reg <- pbp |> filter(season == season_id, season_type == 'REG', !coalesce(play_deleted == 1, FALSE))
week_id <- if (length(args) >= 2) as.integer(args[2]) else max(reg$week)
stopifnot(!is.na(season_id), week_id %in% 1:18, min_attempts > 0, top_n %in% 5:10)
reg <- reg |> filter(week <= week_id)

scheduled <- nflreadr::load_schedules(season_id) |> filter(game_type == 'REG', week == week_id) |> nrow()
played <- n_distinct(reg$game_id[reg$week == week_id])
partial <- played < scheduled
out_dir <- sprintf('outputs/qb_yac_share_%s_through_week_%s%s', season_id, week_id, if (partial) '_partial' else '')
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(reg, file.path(out_dir, 'source_pbp.rds'))

# Pass attempts (no sacks, no 2-pt tries; spikes count as attempts like the box score).
# Passing yards and YAC are summed on completions; air yards on completions = yards - YAC.
plays <- reg |>
  filter(pass_attempt == 1, coalesce(sack, 0) == 0, coalesce(two_point_attempt, 0) == 0,
         !is.na(passer_player_id))
stopifnot(!anyDuplicated(plays[c('game_id', 'play_id')]))
comps <- plays |> filter(complete_pass == 1)
stopifnot(!anyNA(comps$passing_yards))
missing_yac <- sum(is.na(comps$yards_after_catch))
qbs <- plays |> group_by(player_id = passer_player_id) |>
  summarise(player = last(passer_player_name), team = names(which.max(table(posteam))),
            attempts = n(), completions = sum(complete_pass == 1),
            pass_yards = sum(passing_yards[complete_pass == 1]),
            yac = sum(yards_after_catch[complete_pass == 1], na.rm = TRUE),
            .groups = 'drop') |>
  mutate(air_yards_comp = pass_yards - yac, yac_share = yac / pass_yards,
         yac_per_comp = yac / completions) |>
  filter(attempts >= min_attempts) |>
  arrange(desc(yac_share), desc(pass_yards))
stopifnot(nrow(qbs) >= top_n)
lg_share <- sum(comps$yards_after_catch, na.rm = TRUE) / sum(comps$passing_yards)
qual_share <- qbs$yac_share
write.csv(qbs, file.path(out_dir, 'qb_summary.csv'), row.names = FALSE)
write.csv(comps |> select(game_id, play_id, week, posteam, passer_player_name, receiver_player_name,
                          air_yards, yards_after_catch, passing_yards),
          file.path(out_dir, 'completion_audit.csv'), row.names = FALSE)
top <- head(qbs, top_n) |> mutate(rank = row_number())

# Headshots: download once, crop to a circle with a team-color ring.
players <- nflreadr::load_players() |> select(gsis_id, display_name, headshot)
team_meta <- nflreadr::load_teams() |> select(team_abbr, team_color, team_nick)
top <- top |> left_join(players, by = c(player_id = 'gsis_id')) |>
  left_join(team_meta, by = c(team = 'team_abbr'))
stopifnot(!anyNA(top$team_color))
top$display_name <- coalesce(top$display_name, top$player)
shot_dir <- file.path(out_dir, 'headshots'); dir.create(shot_dir, showWarnings = FALSE)
top$photo <- vapply(seq_len(nrow(top)), function(i) {
  raw <- file.path(shot_dir, paste0(top$player_id[i], '_raw.png'))
  out <- file.path(shot_dir, paste0(top$player_id[i], '.png'))
  if (!file.exists(raw) && !is.na(top$headshot[i])) {
    url <- sub('f_auto,q_auto', 'f_png,q_auto,w_400', top$headshot[i], fixed = TRUE)
    try(download.file(url, raw, mode = 'wb', quiet = TRUE), silent = TRUE)
  }
  size <- 300; ring <- 14
  face <- if (file.exists(raw) && file.info(raw)$size > 0) {
    img <- magick::image_read(raw)
    info <- magick::image_info(img)
    side <- min(info$width, info$height)
    img <- magick::image_crop(img, sprintf('%dx%d+%d+0', side, side, (info$width - side) %/% 2))
    magick::image_background(magick::image_resize(img, sprintf('%dx%d!', size, size)), '#E9E4D8')
  } else {
    message('No headshot for ', top$player[i], '; using a name badge.')
    magick::image_annotate(magick::image_blank(size, size, '#E9E4D8'), top$player[i],
                           gravity = 'center', size = 44, weight = 700, color = '#292923')
  }
  mask <- magick::image_draw(magick::image_blank(size, size, 'none'))
  symbols(size / 2, size / 2, circles = size / 2 - ring, bg = 'white', fg = NA, inches = FALSE, add = TRUE)
  dev.off()
  face <- magick::image_composite(face, mask, operator = 'CopyOpacity')
  canvas <- magick::image_draw(magick::image_blank(size, size, 'none'))
  symbols(size / 2, size / 2, circles = size / 2 - 1, bg = top$team_color[i], fg = NA, inches = FALSE, add = TRUE)
  dev.off()
  magick::image_write(magick::image_composite(canvas, face), out, format = 'png')
  out
}, character(1))
top$img <- lapply(top$photo, function(p) as.raster(magick::image_read(p)))

bg <- '#F7F3EA'; ink <- '#292923'; muted <- '#716E64'; rule <- '#C7C0B1'
accent <- '#2E5E8C'; track <- '#E6E0D2'

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
  txt('YARDS AFTER\nTHE CATCH QBs', .06, .845, 34, face = 'bold')
  grid.lines(x = c(.06, .94), y = c(.775, .775), gp = gpar(col = rule, lwd = 1))
  txt(sprintf('HIGHEST %% OF PASSING YARDS FROM YAC  ·  MIN. %s ATT.', min_attempts), .065, .754, 12, face = 'bold')

  # Column layout (npc). Bar column shows YAC share against a 0-max scale.
  x_rank <- .085; x_img <- .155; x_name <- .21; x_yds <- .565; x_bar0 <- .63; x_bar1 <- .815; x_pct <- .94
  y_head <- .722; y_top <- .688; row_h <- .055
  txt('#', x_rank, y_head, 10, muted, 'bold', 'center')
  txt('QB', x_name, y_head, 10, muted, 'bold')
  txt('PASS YDS', x_yds, y_head, 10, muted, 'bold', 'right')
  txt('YAC SHARE', x_pct, y_head, 10, muted, 'bold', 'right')
  bar_max <- ceiling(max(top$yac_share) * 10) / 10
  bar_x <- function(v) x_bar0 + (x_bar1 - x_bar0) * v / bar_max
  ys <- y_top - (seq_len(nrow(top)) - 1) * row_h
  for (i in seq_len(nrow(top))) {
    y <- ys[i]
    if (i > 1) grid.lines(x = c(.06, .94), y = y + row_h / 2, gp = gpar(col = '#E1DBCD', lwd = .8))
    txt(top$rank[i], x_rank, y, 15, if (i == 1) accent else muted, 'bold', 'center')
    grid.raster(top$img[[i]], x = x_img, y = y, height = unit(row_h * .82, 'npc'))
    txt(toupper(top$display_name[i]), x_name, y + .009, 13.5, ink, 'bold')
    txt(sprintf('%s  ·  %s YAC / %s comp.', top$team[i], scales::comma(top$yac[i]), top$completions[i]),
        x_name, y - .011, 9.5, muted)
    txt(scales::comma(top$pass_yards[i]), x_yds, y, 13, ink, 'plain', 'right')
    grid.rect(x = x_bar0, y = y, width = x_bar1 - x_bar0, height = row_h * .30, just = 'left',
              gp = gpar(fill = track, col = NA))
    grid.rect(x = x_bar0, y = y, width = bar_x(top$yac_share[i]) - x_bar0, height = row_h * .30,
              just = 'left', gp = gpar(fill = if (i == 1) accent else top$team_color[i], col = NA))
    txt(sprintf('%.1f%%', 100 * top$yac_share[i]), x_pct, y, 14.5, if (i == 1) accent else ink, 'bold', 'right')
  }
  # League-average marker across the bar column.
  y_bottom <- ys[nrow(top)] - row_h / 2
  grid.lines(x = bar_x(lg_share), y = c(y_bottom + .004, y_top + row_h / 2 - .004),
             gp = gpar(col = '#898477', lwd = 1, lty = 'dashed'))
  txt(sprintf('NFL AVG %.1f%%', 100 * lg_share), bar_x(lg_share), y_bottom - .008, 9, muted, 'bold', 'center')
  grid.lines(x = c(.06, .94), y = y_bottom - .022, gp = gpar(col = rule, lwd = 1))

  txt('YAC share = yards after catch / passing yards on completions.', .065, .113, 10, muted)
  txt('Regular season. Sacks & 2-pt tries excluded. Team = team with most attempts.', .065, .093, 10, muted)
  if (partial) txt(sprintf('*Week %s in progress: %s of %s games included.', week_id, played, scheduled),
                   .065, .078, 10, muted, 'bold')
  txt('SOURCE: NFLVERSE', .065, .05, 11, muted, 'bold')
  txt('WARD WALTON', .92, .05, 11, muted, 'bold', 'right')
  dev.off()
}
draw_poster(file.path(out_dir, 'qb_yac_share_tiktok.png'))
draw_poster(file.path(out_dir, 'qb_yac_share_tiktok_2x.png'), 2)
writeLines(c(
  sprintf('# %s QB share of passing yards from YAC, REG weeks 1-%s%s', season_id, week_id,
          if (partial) sprintf(' (week %s partial: %s of %s games)', week_id, played, scheduled) else ''),
  sprintf('Coverage: %s games, %s qualifying QBs (min. %s attempts), top %s shown.',
          n_distinct(reg$game_id), nrow(qbs), min_attempts, top_n),
  paste('nflverse source timestamp:', paste(source_time, collapse = ' ')),
  'Source: https://nflreadr.nflverse.com/reference/load_pbp.html',
  'Attempts = pass_attempt == 1 excluding sacks and two-point tries. No WP filter (counting stat).',
  'YAC share = sum(yards_after_catch) / sum(passing_yards) on completions.',
  sprintf('League YAC share: %.1f%%. Completions missing yards_after_catch: %s (counted as 0).', 100 * lg_share, missing_yac),
  sprintf('Recreate: Rscript qb_yac_share.r %s %s %s %s %s', season_id, week_id, min_attempts, top_n,
          file.path(out_dir, 'source_pbp.rds'))
), file.path(out_dir, 'README.md'))
print(qbs |> select(player, team, attempts, completions, pass_yards, yac, yac_share), n = 50)
message(sprintf('League YAC share %.1f%%; missing YAC on %s completions', 100 * lg_share, missing_yac))
message('Saved chart and audit files to ', out_dir)
