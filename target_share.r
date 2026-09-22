# Run: Rscript target_share.r [season=2026] [optional PBP .rds] [optional players .rds]
# Target share uses all team targets this season, including games a player missed.
# Players who change teams receive a separate row for each team.
if (dir.exists('/private/tmp/nfl-week1-library')) {
  .libPaths(c('/private/tmp/nfl-week1-library', .libPaths()))
}
library(dplyr)
library(gt)

args <- commandArgs(trailingOnly = TRUE)
target_season <- if (length(args)) as.integer(args[1]) else 2026L
stopifnot(!is.na(target_season), target_season >= 1999)
pbp <- if (length(args) >= 2) readRDS(args[2]) else nflreadr::load_pbp(target_season)
players <- if (length(args) >= 3) readRDS(args[3]) else nflreadr::load_players()
season_plays <- pbp |>
  filter(season == target_season, season_type == 'REG',
         !coalesce(play_deleted == 1, FALSE))
if (!nrow(season_plays)) stop('No regular-season data available for ', target_season)
targets <- season_plays |>
  filter(play_type == 'pass', coalesce(pass_attempt, 0) == 1,
         coalesce(two_point_attempt, 0) == 0,
         !is.na(posteam), !is.na(receiver_player_id), receiver_player_id != '')
stopifnot(nrow(targets) > 0, !anyDuplicated(targets[c('game_id', 'play_id')]))
team_totals <- targets |> count(posteam, name = 'team_targets')
player_names <- players |>
  filter(!is.na(gsis_id)) |>
  distinct(gsis_id, .keep_all = TRUE) |>
  select(receiver_player_id = gsis_id, display_name)
ranking <- targets |>
  group_by(posteam, receiver_player_id) |>
  summarise(short_name = first(receiver_player_name),
            receptions = sum(complete_pass == 1, na.rm = TRUE),
            targets = n(),
            red_zone_targets = sum(yardline_100 <= 20, na.rm = TRUE),
            .groups = 'drop') |>
  left_join(team_totals, by = 'posteam') |>
  left_join(player_names, by = 'receiver_player_id') |>
  mutate(player = coalesce(display_name, short_name),
         target_share = targets / team_targets) |>
  arrange(desc(target_share), desc(targets), player, posteam) |>
  mutate(rank = row_number())
stopifnot(sum(ranking$targets) == nrow(targets),
          all(ranking$receptions <= ranking$targets),
          all(ranking$red_zone_targets <= ranking$targets),
          all(ranking$target_share > 0 & ranking$target_share <= 1))
out_dir <- paste0('outputs/target_share_', target_season)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(ranking, file.path(out_dir, 'receiver_summary.csv'), row.names = FALSE)
top_receivers <- slice_head(ranking, n = 10)
top_receivers$logo <- vapply(top_receivers$posteam, function(team) {
  cached <- file.path('outputs/first_down_drives_2025/logos', paste0(team, '.png'))
  if (file.exists(cached)) return(as.character(local_image(cached, height = 36)))
  logos <- nflreadr::load_teams()
  url <- logos$team_logo_espn[match(team, logos$team_abbr)]
  if (is.na(url)) return(team)
  as.character(web_image(url, height = 36))
}, character(1))
through_date <- max(as.Date(season_plays$game_date), na.rm = TRUE)
through_week <- max(season_plays$week, na.rm = TRUE)
target_share_table <- top_receivers |>
  select(rank, player, logo, receptions, targets, target_share, red_zone_targets) |>
  gt() |>
  tab_header(title = md('**NFL TARGET SHARE LEADERS**'),
             subtitle = paste0(target_season, ' regular season | Top 10 pass catchers | Data through ',
                               format(through_date, '%b %d'), ' (Week ', through_week, ')')) |>
  cols_label(rank = '#', player = 'Player', logo = 'Team', receptions = 'Receptions',
             targets = 'Targets', target_share = 'Target share', red_zone_targets = 'Red-zone targets') |>
  fmt_markdown(columns = logo) |>
  fmt_percent(columns = target_share, decimals = 1) |>
  cols_align('center') |>
  cols_align('left', columns = player) |>
  cols_width(rank ~ px(45), player ~ px(215), logo ~ px(65), receptions ~ px(105),
             targets ~ px(85), target_share ~ px(125), red_zone_targets ~ px(135)) |>
  data_color(columns = target_share, palette = c('#F5F0E6', '#B44A16'),
             domain = c(0, max(ranking$target_share))) |>
  tab_style(style = cell_text(weight = 'bold'),
            locations = cells_body(columns = c(player, target_share))) |>
  tab_source_note('Target share = player targets / all team targets this season. Includes all receiving positions.') |>
  tab_source_note('Red zone: opponent 20-yard line or closer. Excludes nullified plays and two-point attempts.') |>
  tab_source_note('Available games only; latest week may be incomplete. Ties ordered by targets, then name. Traded players listed by team.') |>
  tab_source_note('Source: nflverse / nflreadr | Ward Walton') |>
  tab_options(table.background.color = '#F5F0E6', table.font.names = 'Arial',
              table.font.color = '#242320', table.font.size = px(16),
              heading.title.font.size = px(28), heading.subtitle.font.size = px(13),
              heading.align = 'left', data_row.padding = px(8),
              column_labels.font.weight = 'bold', source_notes.font.size = px(11),
              table.border.top.color = '#B44A16', table.border.top.width = px(4))
gtsave(target_share_table, file.path(out_dir, 'target_share.html'))
if (requireNamespace('webshot2', quietly = TRUE)) {
  tryCatch(gtsave(target_share_table, file.path(out_dir, 'target_share.png'), zoom = 2),
           error = function(e) warning('HTML saved; PNG export failed: ', conditionMessage(e)))
}
print(top_receivers |> select(player, posteam, receptions, targets, target_share, red_zone_targets))
message('Saved table and rankings to ', out_dir)
if (interactive()) print(target_share_table)
