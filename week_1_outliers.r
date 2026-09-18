# Run: Rscript week_1_outliers.r [season=2026] [optional cached PBP .rds]
# Dependencies: dplyr, nflreadr, gt; optional webshot2 + Chrome for PNG.
.libPaths(c('/private/tmp/nfl-week1-library', .libPaths()))
library(dplyr)
library(gt)
args <- commandArgs(trailingOnly = TRUE)
target_season <- if (length(args)) as.integer(args[1]) else 2026L
stopifnot(!is.na(target_season), target_season >= 1999)
pbp <- if (length(args) >= 2) readRDS(args[2]) else nflreadr::load_pbp(target_season)
week1 <- pbp |>
  filter(season == target_season, season_type == 'REG', week == 1,
         !coalesce(play_deleted == 1, FALSE))
if (!nrow(week1)) stop('No regular-season Week 1 data found for ', target_season)

# One league-wide distribution, never separate percentiles within each team.
# Runs/passes include sacks and scrambles; exclude special teams, no-plays,
# kneels, spikes, two-point attempts, and missing/nonfinite EPA.
plays <- week1 |>
  filter(play_type %in% c('run', 'pass'), is.finite(epa),
         !is.na(posteam), coalesce(qb_kneel, 0) == 0,
         coalesce(qb_spike, 0) == 0, coalesce(two_point_attempt, 0) == 0)
stopifnot(nrow(plays) > 0, !anyDuplicated(plays[c('game_id', 'play_id')]))
cutoffs <- quantile(plays$epa, c(.05, .95), type = 7, names = FALSE)
plays <- plays |> mutate(bottom_5 = epa <= cutoffs[1], top_5 = epa >= cutoffs[2],
                         outlier = bottom_5 | top_5)
games <- week1 |> group_by(game_id, home_team, away_team) |>
  summarise(home_score = max(home_score, na.rm = TRUE),
            away_score = max(away_score, na.rm = TRUE), .groups = 'drop')
results <- bind_rows(
  games |> transmute(game_id, team = home_team, opponent = away_team,
                     scored = home_score, allowed = away_score),
  games |> transmute(game_id, team = away_team, opponent = home_team,
                     scored = away_score, allowed = home_score)
) |> mutate(result = paste0(case_when(scored > allowed ~ 'W', scored < allowed ~ 'L', TRUE ~ 'T'),
                            ' ', scored, '\u2013', allowed))
ranking <- plays |> group_by(team = posteam) |>
  summarise(plays = n(), positive = sum(top_5), negative = sum(bottom_5),
            outliers = sum(outlier), outlier_rate = mean(outlier), .groups = 'drop') |>
  left_join(results |> select(team, opponent, result), by = 'team') |>
  arrange(desc(outliers), desc(outlier_rate), team) |>
  mutate(rank = min_rank(desc(outliers)))
stopifnot(!anyDuplicated(ranking$team), sum(ranking$plays) == nrow(plays),
          all(ranking$outliers == ranking$positive + ranking$negative),
          !anyNA(ranking$result))
if (nrow(ranking) != 32L || nrow(games) != 16L) {
  stop('Incomplete Week 1 coverage: ', nrow(games), ' games and ', nrow(ranking), ' teams. Retry after data updates.')
}
out_dir <- paste0('outputs/week_1_outliers_', target_season)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(ranking, file.path(out_dir, 'team_summary.csv'), row.names = FALSE)
write.csv(plays |> select(game_id, play_id, posteam, defteam, play_type, desc, epa,
                         bottom_5, top_5, outlier),
          file.path(out_dir, 'play_audit.csv'), row.names = FALSE)
write.csv(data.frame(season = target_season, week = 1, plays = nrow(plays),
                     p05 = cutoffs[1], p95 = cutoffs[2]),
          file.path(out_dir, 'epa_cutoffs.csv'), row.names = FALSE)

top_teams <- ranking |> filter(rank <= 10)
top_teams$logo <- vapply(top_teams$team, function(team) {
  path <- file.path('outputs/first_down_drives_2025/logos', paste0(team, '.png'))
  stopifnot(file.exists(path))
  as.character(local_image(path, height = 32))
}, character(1))

outlier_table <- top_teams |>
  select(rank, logo, team, opponent, result, plays, positive, negative, outliers, outlier_rate) |>
  gt() |>
  tab_header(title = md('**WHICH WEEK 1 RESULTS SHOULD WE TRUST LEAST?**'),
             subtitle = paste0(target_season, ' NFL \u00b7 Offensive EPA outliers \u00b7 Top 10 including ties')) |>
  cols_label(rank = '#', logo = '', team = 'Team', opponent = 'Opp.', result = 'Result', plays = 'Plays',
             positive = 'Top 5%', negative = 'Bottom 5%', outliers = 'Total', outlier_rate = 'Share') |>
  tab_spanner(label = 'Outlier plays', columns = c(positive, negative, outliers, outlier_rate)) |>
  fmt_percent(columns = outlier_rate, decimals = 1) |>
  fmt_markdown(columns = logo) |>
  cols_align('center') |>
  cols_align('left', columns = c(team, result)) |>
  cols_width(rank ~ px(45), logo ~ px(50), team ~ px(65), opponent ~ px(60), result ~ px(100),
             plays ~ px(65), positive ~ px(85), negative ~ px(90), outliers ~ px(75), outlier_rate ~ px(85)) |>
  data_color(columns = outliers, palette = c('#F5F0E6', '#B44A16'),
             domain = c(0, max(ranking$outliers))) |>
  tab_style(style = cell_text(weight = 'bold'), locations = cells_body(columns = c(team, outliers))) |>
  tab_source_note(sprintf('Outliers: EPA \u2264 %.2f or \u2265 %.2f. Cutoffs are the league-wide Week 1 5th/95th percentiles.', cutoffs[1], cutoffs[2])) |>
  tab_source_note('Offense only: runs + passes, including sacks/scrambles. Excludes kneels, spikes, no-plays, special teams and two-point attempts.') |>
  tab_source_note('Share = outliers / eligible plays. Count ties share a rank; share orders tied teams. Percentile-boundary ties are included.') |>
  tab_source_note('Extreme-play exposure is descriptive; this ranking does not establish that a team\u2019s performance is unrepeatable.') |>
  tab_source_note('Source: nflverse / nflreadr | Ward Walton') |>
  tab_options(table.background.color = '#F5F0E6', table.font.names = 'Arial',
              table.font.color = '#242320', table.font.size = px(14),
              heading.title.font.size = px(23), heading.subtitle.font.size = px(14),
              heading.align = 'left', data_row.padding = px(6),
              column_labels.font.weight = 'bold', source_notes.font.size = px(11),
              table.border.top.color = '#B44A16', table.border.top.width = px(4))
gtsave(outlier_table, file.path(out_dir, 'week_1_outliers.html'))
if (requireNamespace('webshot2', quietly = TRUE)) {
  tryCatch(gtsave(outlier_table, file.path(out_dir, 'week_1_outliers.png'), zoom = 2),
           error = function(e) warning('HTML saved; PNG export failed: ', conditionMessage(e)))
}
print(ranking, n = 32)
message('Saved table and audit data to ', out_dir)
