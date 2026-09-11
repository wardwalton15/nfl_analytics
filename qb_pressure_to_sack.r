# Run: Rscript qb_pressure_to_sack.r [optional cached input RDS]
# Requires nflreadr, dplyr, gt, webshot2, and Chrome for PNG export.
if (dir.exists('/private/tmp/nfl-gt-library')) {
  .libPaths(c('/private/tmp/nfl-gt-library', .libPaths()))
}
library(dplyr)
library(gt)

season <- 2025L
min_starts <- 10L
top_n <- 10L
out_dir <- paste0('outputs/qb_pressure_to_sack_', season)
dir.create(file.path(out_dir, 'headshots'), recursive = TRUE, showWarnings = FALSE)
args <- commandArgs(trailingOnly = TRUE)
inputs <- if (length(args)) readRDS(args[1]) else list(
  weekly = nflreadr::load_pfr_advstats(season, stat_type = 'pass', summary_level = 'week'),
  schedules = nflreadr::load_schedules(season),
  players = nflreadr::load_players()
)
weekly <- inputs$weekly |> filter(season == !!season, game_type == 'REG')
schedules <- inputs$schedules |> filter(season == !!season, game_type == 'REG')
stopifnot(n_distinct(weekly$game_id) == 272L, nrow(schedules) == 272L,
          !anyNA(weekly$times_sacked), !anyNA(weekly$times_pressured),
          !anyDuplicated(weekly[c('game_id', 'pfr_player_id')]),
          all(weekly$times_sacked <= weekly$times_pressured))
# Schedule QB IDs identify starts, not merely games with a passing attempt.
starts <- tibble(gsis_id = c(schedules$home_qb_id, schedules$away_qb_id)) |>
  count(gsis_id, name = 'gs')
players <- inputs$players |> select(pfr_id, gsis_id, headshot)
totals <- weekly |>
  group_by(pfr_player_id) |>
  summarise(player = first(pfr_player_name), team = paste(unique(team), collapse = ' / '),
            sacks = sum(times_sacked), pressures = sum(times_pressured), .groups = 'drop') |>
  left_join(players, by = c('pfr_player_id' = 'pfr_id')) |>
  left_join(starts, by = 'gsis_id')
stopifnot(!anyNA(totals$gsis_id), !anyDuplicated(totals$pfr_player_id))
ranking <- totals |> filter(gs >= min_starts, pressures > 0) |>
  mutate(sack_pct = sacks / pressures) |>
  arrange(desc(sack_pct), desc(sacks), player) |> mutate(rank = row_number())
top <- head(ranking, top_n)
stopifnot(nrow(top) == top_n, !anyNA(top$headshot))
write.csv(ranking, file.path(out_dir, 'qualified_qbs.csv'), row.names = FALSE)
write.csv(top, file.path(out_dir, 'top_10.csv'), row.names = FALSE)
write.csv(weekly, file.path(out_dir, 'weekly_audit.csv'), row.names = FALSE)
saveRDS(inputs, file.path(out_dir, 'source_data.rds'))

# Embed local headshots so the HTML and PNG are self-contained.
top$photo <- vapply(seq_len(nrow(top)), function(i) {
  path <- file.path(out_dir, 'headshots', paste0(top$pfr_player_id[i], '.png'))
  url <- sub('f_auto,q_auto', 'f_png,q_auto,w_240', top$headshot[i], fixed = TRUE)
  if (!file.exists(path)) download.file(url, path, mode = 'wb', quiet = TRUE)
  stopifnot(file.info(path)$size > 0)
  as.character(local_image(path, height = 58))
}, character(1))

qb_table <- top |> select(rank, photo, player, team, gs, sacks, pressures, sack_pct) |>
  gt() |>
  tab_header(title = md('**WHEN PRESSURE BECOMES A SACK**'),
             subtitle = '2025 NFL regular season · Top 10 quarterbacks · Minimum 10 starts') |>
  cols_label(rank = '#', photo = '', player = 'Quarterback', team = 'Team',
             gs = 'GS', sacks = 'Sacks', pressures = md('Pressured<br>dropbacks'),
             sack_pct = md('Sack /<br>pressure %')) |>
  fmt_percent(columns = sack_pct, decimals = 1) |>
  fmt_markdown(columns = photo) |>
  cols_align('left', columns = player) |>
  cols_align('center', columns = c(rank, photo, team, gs, sacks, pressures, sack_pct)) |>
  cols_width(rank ~ px(40), photo ~ px(90), player ~ px(205), team ~ px(65),
             gs ~ px(50), sacks ~ px(70), pressures ~ px(115), sack_pct ~ px(125)) |>
  data_color(columns = sack_pct, palette = c('#F4DDC9', '#B44A16'),
             domain = range(top$sack_pct)) |>
  tab_style(cell_text(weight = 'bold'), locations = cells_body(columns = c(player, sack_pct))) |>
  tab_source_note(md('**Rate = sacks ÷ pressured dropbacks.** Season totals include all appearances.')) |>
  tab_source_note('Pressure counts include sacks. GS = games started; postseason excluded.') |>
  tab_source_note('Source: Pro Football Reference via nflverse; starts: nflverse schedules | Ward Walton') |>
  tab_options(table.background.color = '#F5F0E6', table.font.names = 'Arial',
              table.font.color = '#242320', table.font.size = px(16),
              heading.align = 'left', heading.title.font.size = px(27),
              heading.subtitle.font.size = px(14), heading.padding = px(18),
              column_labels.font.weight = 'bold', column_labels.font.size = px(13),
              data_row.padding = px(3), table.border.top.color = '#B44A16',
              table.border.top.width = px(5), source_notes.font.size = px(11),
              source_notes.padding = px(5))
gtsave(qb_table, 'qb_pressure_to_sack.html', path = out_dir)
gtsave(qb_table, 'qb_pressure_to_sack.png', path = out_dir, vwidth = 1000, zoom = 2)
print(top |> select(rank, player, gs, sacks, pressures, sack_pct))
