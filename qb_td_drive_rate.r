# Run: Rscript qb_td_drive_rate.r [last_season=2026] [minimum_starts=20] [garbage_from_qtr=3] [wp_cutoff=0.05]
# Delete cached RDS files in the output directory to refresh downloaded data.
.libPaths(c('/private/tmp/nfl-chart-r-library', .libPaths()))
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(ggplot2)})
args <- commandArgs(trailingOnly = TRUE)
last_season <- if (length(args)) as.integer(args[1]) else 2026L
minimum_starts <- if (length(args) > 1) as.integer(args[2]) else 20L
garbage_from_qtr <- if (length(args) > 2) as.integer(args[3]) else 3L
wp_cutoff <- if (length(args) > 3) as.numeric(args[4]) else .05
stopifnot(!is.na(last_season), last_season >= 2024, minimum_starts > 0)
stopifnot(garbage_from_qtr %in% 1:4, is.finite(wp_cutoff), wp_cutoff > 0, wp_cutoff < .5)
out_dir <- 'outputs/qb_td_drive_rate_since_2024'
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
cached <- function(name, fetch) {
  path <- file.path(out_dir, paste0(name, '.rds'))
  if (file.exists(path)) return(readRDS(path))
  value <- fetch(); stopifnot(nrow(value) > 0); saveRDS(value, path); value
}
schedule <- cached('schedules', function() nflreadr::load_schedules(2024:last_season))
players <- cached('players', nflreadr::load_players)
pbp <- bind_rows(lapply(2024:last_season, function(year)
  cached(paste0('pbp_', year), function() nflreadr::load_pbp(year)))) |>
  filter(season_type == 'REG', !coalesce(play_deleted == 1, FALSE))
games <- schedule |> filter(season %in% 2024:last_season, game_type == 'REG', !is.na(result))
missing_games <- setdiff(games$game_id, unique(pbp$game_id))
if (length(missing_games)) stop('Missing PBP for completed games: ', paste(missing_games, collapse = ', '))
pbp <- semi_join(pbp, games, by = 'game_id')
stopifnot(nrow(pbp) > 0, !anyDuplicated(pbp[c('game_id', 'play_id')]))
starters <- bind_rows(
  games |> transmute(game_id, posteam = home_team, starter_id = home_qb_id, starter_name = home_qb_name),
  games |> transmute(game_id, posteam = away_team, starter_id = away_qb_id, starter_name = away_qb_name))
stopifnot(!anyNA(starters$starter_id), !anyDuplicated(starters[c('game_id', 'posteam')]))
starts <- starters |> group_by(qb_id = starter_id) |>
  summarise(starts = n(), qb_name = last(starter_name), .groups = 'drop')
qb_ids <- union(players$gsis_id[players$position == 'QB'], starters$starter_id)

# PBP does not identify the QB on ordinary handoffs. Use observed QB pass/run
# events within each drive, then carry the previous QB forward for unobserved
# drives (or the listed starter at the start of a game). Exclude drives with
# multiple observed QBs rather than assigning the TD to an arbitrary player.
drive_audit <- pbp |>
  filter(!is.na(posteam), !is.na(fixed_drive),
         !play_type %in% c('kickoff', 'extra_point'),
         coalesce(two_point_attempt, 0) == 0, coalesce(extra_point_attempt, 0) == 0) |>
  arrange(game_id, play_id) |>
  mutate(observed_qb = case_when(
    passer_player_id %in% qb_ids ~ passer_player_id,
    rusher_player_id %in% qb_ids ~ rusher_player_id,
    TRUE ~ NA_character_)) |>
  group_by(game_id, posteam, fixed_drive) |>
  summarise(season = first(season), week = first(week), first_play = min(play_id),
    start_qtr = first(qtr[!is.na(down)]), start_wp = first(wp[!is.na(down)]),
    offensive_drive = any(!is.na(down)),
    td = any(touchdown == 1 & td_team == posteam, na.rm = TRUE),
    observed_qbs = n_distinct(observed_qb, na.rm = TRUE),
    observed_id = last(observed_qb[!is.na(observed_qb)], default = NA_character_),
    .groups = 'drop') |>
  filter(offensive_drive) |>
  left_join(starters, by = c('game_id', 'posteam')) |>
  arrange(game_id, posteam, first_play) |>
  group_by(game_id, posteam) |>
  mutate(qb_id = observed_id) |>
  fill(qb_id, .direction = 'down') |>
  mutate(qb_id = coalesce(qb_id, starter_id),
         attribution = case_when(observed_qbs > 1 ~ 'excluded_multiple_qbs',
                                 observed_qbs == 1 ~ 'observed',
                                 TRUE ~ 'inferred_from_previous_qb_or_starter')) |>
  ungroup() |>
  mutate(garbage_time = start_qtr >= garbage_from_qtr & start_qtr <= 4 &
           (start_wp < wp_cutoff | start_wp > 1 - wp_cutoff),
         included = observed_qbs <= 1 & !garbage_time)
if (anyNA(drive_audit$garbage_time)) stop('Missing start-of-drive quarter or win probability.')
summary <- drive_audit |> filter(included) |>
  group_by(qb_id) |>
  summarise(drives = n(), td_drives = sum(td), inferred_drives = sum(observed_qbs == 0),
            .groups = 'drop') |>
  inner_join(starts, by = 'qb_id') |>
  filter(starts >= minimum_starts) |>
  mutate(td_rate = td_drives / drives) |>
  arrange(desc(td_rate), desc(td_drives), qb_name) |>
  mutate(rank = row_number())
stopifnot(nrow(summary) > 0, all(summary$td_drives <= summary$drives),
          !anyNA(summary$td_rate), !anyDuplicated(drive_audit[c('game_id','posteam','fixed_drive')]))
write.csv(summary, file.path(out_dir, 'qb_summary.csv'), row.names = FALSE)
write.csv(drive_audit, file.path(out_dir, 'drive_audit.csv'), row.names = FALSE)
write.csv(starters, file.path(out_dir, 'starts_audit.csv'), row.names = FALSE)
latest <- games |> arrange(gameday, game_id) |> slice_tail(n = 1)
coverage <- sprintf('2024–%s regular season • Through %s, Week %s • Min. %s starts',
                    last_season, latest$gameday, latest$week, minimum_starts)
garbage_note <- sprintf('Excluded: drives starting in Q%s–Q4 with win probability below %s%% or above %s%%.',
                        garbage_from_qtr, 100 * wp_cutoff, 100 * (1 - wp_cutoff))
if (garbage_from_qtr == 4) garbage_note <- sub('Q4–Q4', 'Q4', garbage_note, fixed = TRUE)
summary <- summary |> mutate(label = factor(qb_name, levels = rev(qb_name)),
                            highlight = rank <= 3)
bg <- '#F7F3EA'; ink <- '#292923'; muted <- '#716E64'
plot <- ggplot(summary, aes(td_rate, label)) +
  geom_col(aes(fill = highlight), width = .65) +
  geom_text(aes(label = sprintf('%.1f%%', 100 * td_rate)), hjust = -.16,
            size = 4.0, fontface = 'bold', color = ink) +
  geom_text(aes(x = .53, label = sprintf('%s / %s', td_drives, drives)),
            hjust = 1, size = 3.6, color = muted) +
  geom_text(aes(x = .59, label = starts), hjust = 1, size = 3.6, color = muted) +
  annotate('text', x = .53, y = nrow(summary) + 1, label = 'TD / DRIVES',
           hjust = 1, size = 3.2, fontface = 'bold', color = muted) +
  annotate('text', x = .59, y = nrow(summary) + 1, label = 'STARTS',
           hjust = 1, size = 3.2, fontface = 'bold', color = muted) +
  scale_fill_manual(values = c('FALSE' = '#9BAD9A', 'TRUE' = '#2E7058'), guide = 'none') +
  scale_x_continuous(limits = c(0, .60), breaks = seq(0, .4, .1),
                     labels = scales::label_percent(accuracy = 1), expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = c(.6, 1.6))) +
  labs(title = 'WHO FINISHES DRIVES WITH TDs?', subtitle = coverage,
       x = 'OFFENSIVE TOUCHDOWNS / DRIVE', y = NULL,
       caption = paste(garbage_note,
         'Any offensive TD. Includes remaining kneel/end-of-half drives and relief appearances.',
         'QB inferred from play-by-play; mixed-QB drives excluded. Starts counted since 2024.',
         'Data: nflverse  |  Ward Walton', sep = '\n')) +
  theme_minimal(base_family = 'Arial', base_size = 12) +
  theme(plot.background = element_rect(fill = bg, color = NA),
        panel.grid = element_blank(), axis.text.y = element_text(color = ink, size = 12),
        axis.text.x = element_text(color = muted),
        axis.title.x = element_text(color = muted, size = 10, margin = margin(t = 12)),
        plot.title = element_text(face = 'bold', color = ink, size = 23),
        plot.subtitle = element_text(color = muted, size = 10.5, margin = margin(b = 15)),
        plot.caption = element_text(color = muted, size = 9, hjust = 0, lineheight = 1.4,
                                    margin = margin(t = 18)),
        plot.margin = margin(24, 24, 20, 24))
ggsave(file.path(out_dir, 'qb_td_drive_rate.png'), plot, width = 10,
       height = max(9, nrow(summary) * .30 + 2.6), dpi = 180, device = ragg::agg_png)
writeLines(c('# QB touchdown drive rate', coverage, garbage_note,
  'Win probability is posteam wp before the first offensive down; thresholds are strict.',
  'The filter excludes whole drives. Overtime remains included. Start eligibility is unchanged.',
  sprintf('Garbage-time drives excluded before QB eligibility: %s.', sum(drive_audit$garbage_time)),
  'Rate = attributed offensive TD drives / all attributed offensive drives; pooled across seasons.',
  'Eligibility: at least the specified number of regular-season starts since 2024, from schedule QB IDs.',
  'Drives in relief appearances also count for eligible quarterbacks. Teams combined by GSIS player ID.',
  'Corrected fixed_drive IDs; includes kneels, spikes, end-of-half drives, sacks and turnovers.',
  'Kickoffs, conversion attempts and deleted records excluded; requires at least one offensive down.',
  'TD must be credited to the possession team; defensive and return TDs do not qualify.',
  'QB attribution is an estimate: the observed QB passer/rusher identifies a drive. Drives without an',
  'observed QB inherit the most recent observed QB for that offense, falling back to its listed starter.',
  'Handoff-only drives immediately after an unobserved substitution can be misattributed.',
  'Drives with multiple observed QBs are excluded. This is not a snap-participation-based metric.',
  sprintf('Excluded mixed-QB drives: %s. Inferred eligible-QB drives: %s of %s.',
    sum(drive_audit$observed_qbs > 1), sum(summary$inferred_drives), sum(summary$drives)),
  'Sources: https://nflreadr.nflverse.com/reference/load_pbp.html',
  'https://nflreadr.nflverse.com/reference/load_schedules.html',
  'https://nflreadr.nflverse.com/reference/load_players.html',
  'Cached RDS files preserve this snapshot. Delete them to download updated inputs.',
  sprintf('Recreate: Rscript qb_td_drive_rate.r %s %s %s %s', last_season, minimum_starts,
          garbage_from_qtr, wp_cutoff)),
  file.path(out_dir, 'README.md'))
print(summary |> select(rank, qb_name, starts, td_drives, drives, td_rate), n = Inf)
message('Saved chart and audit files to ', out_dir)
