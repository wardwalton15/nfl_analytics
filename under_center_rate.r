# Run: Rscript under_center_rate.r [optional cached nflverse PBP .rds]
.libPaths(c('/private/tmp/nfl-chart-r-library', .libPaths()))
library(dplyr)
library(ggplot2)
library(ggpath)

args <- commandArgs(trailingOnly = TRUE)
pbp <- if (length(args)) readRDS(args[1]) else nflreadr::load_pbp(2025)
stopifnot(nrow(pbp) > 0)
regular <- pbp |>
  filter(season == 2025, season_type == 'REG',
         !coalesce(play_deleted == 1, FALSE))
stopifnot(n_distinct(regular$game_id) == 272)

# wp is the possession team's pre-play win probability (inclusive bounds).
# Non-shotgun is the PBP proxy for under center; this is not charted formation data.
# Rate uses all eligible plays with known formation, even if EPA is missing.
plays <- regular |>
  filter(play_type %in% c('run', 'pass'), !is.na(posteam),
         between(wp, .05, .95), coalesce(qb_kneel, 0) == 0,
         coalesce(qb_spike, 0) == 0, coalesce(two_point_attempt, 0) == 0)
missing_formation <- sum(!plays$shotgun %in% c(0, 1))
plays <- plays |>
  filter(shotgun %in% c(0, 1)) |>
  mutate(under_center = shotgun == 0)
stopifnot(!anyDuplicated(plays[c('game_id', 'play_id')]))
team_summary <- plays |>
  group_by(team = posteam) |>
  summarise(eligible_plays = n(), under_center_plays = sum(under_center),
            under_center_rate = mean(under_center),
            under_center_epa_plays = sum(under_center & is.finite(epa)),
            under_center_epa = mean(epa[under_center & is.finite(epa)]),
            .groups = 'drop') |>
  arrange(desc(under_center_rate))
stopifnot(nrow(team_summary) == 32, all(is.finite(team_summary$under_center_epa)),
          all(team_summary$under_center_epa_plays > 0),
          sum(team_summary$eligible_plays) == nrow(plays))
league_rate <- mean(plays$under_center)
league_epa <- with(plays, mean(epa[under_center & is.finite(epa)]))
out_dir <- 'outputs/under_center_2025'
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(team_summary, file.path(out_dir, 'team_summary.csv'), row.names = FALSE)
write.csv(plays |> select(game_id, play_id, posteam, play_type, wp, shotgun,
                          under_center, epa, desc),
          file.path(out_dir, 'play_audit.csv'), row.names = FALSE)
write.csv(data.frame(eligible_plays = nrow(plays),
                     excluded_missing_formation = missing_formation,
                     under_center_rate = league_rate, under_center_epa = league_epa),
          file.path(out_dir, 'league_summary.csv'), row.names = FALSE)

# Separate Chicago and Detroit, whose actual coordinates nearly coincide.
plot_data <- team_summary |>
  mutate(logo = file.path('outputs/first_down_drives_2025/logos', paste0(team, '.png')),
         logo_x = under_center_rate + case_when(team == 'CHI' ~ -.009,
                                                team == 'DET' ~ .009, TRUE ~ 0),
         logo_y = under_center_epa + case_when(team == 'CHI' ~ -.014,
                                               team == 'DET' ~ -.035, TRUE ~ 0))
stopifnot(all(file.exists(plot_data$logo)))
under_center_plot <- ggplot(plot_data, aes(under_center_rate, under_center_epa)) +
  geom_vline(xintercept = league_rate, linetype = 'dashed', color = '#B7AC9A') +
  geom_hline(yintercept = league_epa, linetype = 'dashed', color = '#B7AC9A') +
  geom_segment(data = plot_data |> filter(team %in% c('CHI', 'DET')),
               aes(xend = logo_x, yend = logo_y), color = '#686158', linewidth = .4) +
  ggpath::geom_from_path(aes(x = logo_x, y = logo_y, path = logo), width = .03) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),
                     breaks = seq(0, 1, .05), expand = expansion(mult = .07)) +
  scale_y_continuous(labels = scales::label_number(accuracy = .01),
                     expand = expansion(mult = c(.08, .12))) +
  labs(title = 'UNDER CENTER: USAGE VS. EFFICIENCY',
       subtitle = '2025 NFL regular season | Pre-play win probability: 5–95%',
       x = 'Under-center share of offensive plays',
       y = 'EPA/play from under center',
       caption = sprintf(paste0('Dashed lines: play-weighted NFL averages (%.1f%% usage, %+.3f EPA/play).\n',
         'Under center = shotgun == 0. Runs + passes include sacks and scrambles.\n',
         'Excludes kneels, spikes, no-plays and two-point attempts. Both axes use the same WP filter.\n',
         'CHI / DET logos offset slightly; leader lines mark their actual coordinates.\n',
         'Source: nflverse / nflreadr | Plot: Ward Walton'), 100 * league_rate, league_epa)) +
  theme_minimal(base_size = 13, base_family = 'Arial') +
  theme(plot.background = element_rect(fill = '#F5F0E6', color = NA),
        panel.background = element_rect(fill = '#F5F0E6', color = NA),
        panel.grid.minor = element_blank(), panel.grid.major = element_line(color = '#E4DDD1'),
        plot.title = element_text(face = 'bold', size = 22, color = '#242320'),
        plot.subtitle = element_text(color = '#686158', margin = margin(b = 18)),
        plot.caption = element_text(hjust = 0, color = '#686158', size = 10, lineheight = 1.25),
        axis.title = element_text(face = 'bold'), plot.margin = margin(20, 25, 15, 20))
ggsave(file.path(out_dir, 'under_center_2025.png'), under_center_plot,
       width = 12, height = 9, dpi = 180)
print(team_summary, n = 32)
message('Saved plot and audit data to ', out_dir)
