# Run: Rscript def_pressure_rate.r [season=2026] [through_week=2] [pressure CSV=outputs/pressure.csv]
.libPaths(c('/private/tmp/nfl-chart-r-library', .libPaths()))
suppressPackageStartupMessages({library(dplyr); library(ggplot2)})

args <- commandArgs(trailingOnly = TRUE)
season <- if (length(args)) as.integer(args[1]) else 2026L
through_week <- if (length(args) >= 2) as.integer(args[2]) else 2L
stopifnot(!is.na(season), !is.na(through_week), through_week %in% 1:18)
out <- sprintf('outputs/def_pressure_%s_through_week_%s', season, through_week)
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# This supplied snapshot is for 2026 through Week 2; require an explicit
# replacement file for other periods to avoid silently relabeling these totals.
source_csv <- if (length(args) >= 3) args[3] else 'outputs/pressure.csv'
if (length(args) < 3 && (season != 2026L || through_week != 2L)) {
    stop('Supply a matching pressure CSV as the third argument for another period.')
}
raw <- read.csv(source_csv, check.names = FALSE, stringsAsFactors = FALSE)
required <- c('Tm', 'G', 'Att', 'Prss', 'Sk')
stopifnot(all(required %in% names(raw)), nrow(raw) == 32L, !anyDuplicated(raw$Tm))
rate_column <- grep('^Prss%', names(raw), value = TRUE)
stopifnot(length(rate_column) == 1L)
raw$pfr_pressure_rate <- as.numeric(sub('%', '', raw[[rate_column]], fixed = TRUE)) / 100
# Explicit names avoid a network dependency for this local CSV workflow.
team_names <- c(
    ARI = 'Arizona Cardinals', ATL = 'Atlanta Falcons', BAL = 'Baltimore Ravens',
    BUF = 'Buffalo Bills', CAR = 'Carolina Panthers', CHI = 'Chicago Bears',
    CIN = 'Cincinnati Bengals', CLE = 'Cleveland Browns', DAL = 'Dallas Cowboys',
    DEN = 'Denver Broncos', DET = 'Detroit Lions', GB = 'Green Bay Packers',
    HOU = 'Houston Texans', IND = 'Indianapolis Colts', JAX = 'Jacksonville Jaguars',
    KC = 'Kansas City Chiefs', LA = 'Los Angeles Rams', LAC = 'Los Angeles Chargers',
    LV = 'Las Vegas Raiders', MIA = 'Miami Dolphins', MIN = 'Minnesota Vikings',
    NE = 'New England Patriots', NO = 'New Orleans Saints', NYG = 'New York Giants',
    NYJ = 'New York Jets', PHI = 'Philadelphia Eagles', PIT = 'Pittsburgh Steelers',
    SEA = 'Seattle Seahawks', SF = 'San Francisco 49ers', TB = 'Tampa Bay Buccaneers',
    TEN = 'Tennessee Titans', WAS = 'Washington Commanders')
final <- raw %>% transmute(
    team_abbr = names(team_names)[match(Tm, team_names)], Tm, Games = G, Att, Prss, Sk,
    # Use PFR's published percentage directly, including its rounding.
    prss_rate = pfr_pressure_rate, Prss_to_sack_rate = if_else(Prss > 0, Sk / Prss, NA_real_))
stopifnot(!anyNA(final$team_abbr), !anyDuplicated(final$team_abbr),
          all(is.finite(as.matrix(final[c('Games', 'Att', 'Prss', 'Sk')]))),
          all(final$Games > 0 & final$Games <= through_week),
          all(final$Att > 0), all(final$Prss >= 0), all(final$Sk >= 0),
          all(is.finite(final$prss_rate)), all(final$prss_rate >= 0 & final$prss_rate <= 1), all(final$Sk <= final$Prss))
if (season == 2026L && through_week == 2L) stopifnot(all(final$Games == 2L))
final <- final %>% arrange(desc(prss_rate), team_abbr) %>%
    mutate(rank = row_number(), row = n() + 1L - rank, sack_rate = if_else(Prss > 0, prss_rate * Sk / Prss, 0))
write.csv(final, file.path(out, 'team_summary.csv'), row.names = FALSE)
write.csv(raw, file.path(out, 'source_pressure.csv'), row.names = FALSE)
write.csv(final %>% select(team_abbr, Games), file.path(out, 'coverage.csv'), row.names = FALSE)
# Retire audit files from the superseded, incomplete API-based chart.
unlink(file.path(out, c('missing_games.csv', 'source_pfr.rds', 'attempts_by_game.csv')))
# Exact dropback counts are absent; label the unweighted team mean explicitly.
league_rate <- mean(final$prss_rate)
coverage_note <- sprintf('32 teams · %s games · Team totals from supplied CSV', sum(final$Games) / 2)
writeLines(c(
    sprintf('# Defensive pressure: %s through Week %s', season, through_week),
    sprintf('Source: %s (user-supplied period; CSV has no season/week fields).', source_csv),
    'All 32 team names and numeric totals validated; 2026 Week 2 requires two games per team.',
    'Chart rate = published PFR Prss% from the CSV. Sacks are included in pressures.',
    'Sack shading = published pressure rate times Sk / Prss; a proportional split of the rounded rate.',
    'Dashed reference = unweighted mean of team PFR rates, not a pooled league rate.',
    'Source snapshot: source_pressure.csv.',
    sprintf('Recreate: Rscript def_pressure_rate.r %s %s %s', season, through_week, source_csv)
), file.path(out, 'README.md'))
# Reuse downloaded logos; abbreviations keep the chart readable without them.
final$logo <- file.path('outputs/epa_team_tiers_2026_week_1/logos', paste0(final$team_abbr, '.png'))
logos <- final %>% filter(file.exists(logo))
right <- max(final$prss_rate) + .055
p <- ggplot(final, aes(y = row)) +
    geom_segment(aes(x = 0, xend = right, yend = row), color = '#E8E4DC', linewidth = .3) +
    geom_col(aes(x = prss_rate, fill = 'Other pressures'), width = .64, orientation = 'y') +
    geom_col(aes(x = sack_rate, fill = 'Sacks'), width = .64, orientation = 'y') +
    geom_vline(xintercept = league_rate, linetype = 'dashed', color = '#827A6F', linewidth = .5) +
    geom_text(aes(x = prss_rate + .007, label = scales::percent(prss_rate, accuracy = .1)),
              hjust = 0, size = 3.5, fontface = 'bold', color = '#223632') +
    geom_text(aes(x = -.088, label = sprintf('%02d', rank)), color = '#8A857B', size = 3.1) +
    geom_text(aes(x = -.030, label = team_abbr), hjust = 1, fontface = 'bold', size = 3.2) +
    geom_text(aes(x = right + .038, label = Games), size = 3.3, color = '#514F49') +
    geom_text(aes(x = right + .105, label = sprintf('%s / %s', Prss, Sk)), size = 3.3, color = '#514F49') +
    annotate('text', x = right + .038, y = 33.2, label = 'GP', fontface = 'bold', size = 3) +
    annotate('text', x = right + .105, y = 33.2, label = 'PRS / SK', fontface = 'bold', size = 3) +
    scale_fill_manual(NULL, values = c('Other pressures' = '#75A69B', 'Sacks' = '#214E48'),
                      breaks = c('Other pressures', 'Sacks')) +
    scale_x_continuous(breaks = seq(0, right, .1), labels = scales::label_percent(accuracy = 1)) +
    scale_y_continuous(breaks = NULL) +
    coord_cartesian(xlim = c(-.10, right + .15), ylim = c(.3, 33.6), expand = FALSE, clip = 'off') +
    labs(title = 'NFL DEFENSIVE PRESSURE',
         subtitle = sprintf('%s · Weeks 1–%s
%s', season, through_week, coverage_note),
         x = 'PFR PRESSURE RATE', y = NULL,
         caption = paste0('Rates match PFR Prss%. Sacks are included in pressures; shading shows their share.',
                          '\nDashed line: average team rate (', scales::percent(league_rate, accuracy = .1),
                          '). GP = games with PFR data.',
                          '\n', 'Source: PFR via supplied pressure.csv  |  Ward Walton')) +
    theme_minimal(base_family = 'Arial', base_size = 12) +
    theme(plot.background = element_rect(fill = '#F7F3EA', color = NA),
          panel.grid = element_blank(), axis.text.x = element_text(color = '#68675F', size = 10),
          axis.title.x = element_text(size = 10, face = 'bold', margin = margin(t = 12)),
          plot.title = element_text(size = 29, face = 'bold', color = '#223632'),
          plot.subtitle = element_text(size = 12, color = '#68675F', lineheight = 1.5, margin = margin(t = 7, b = 8)),
          plot.caption = element_text(hjust = 0, size = 9, color = '#68675F', lineheight = 1.4, margin = margin(t = 15)),
          legend.position = 'top', legend.justification = 'left', legend.text = element_text(size = 10),
          plot.margin = margin(24, 24, 20, 24))
if (nrow(logos)) p <- p + ggpath::geom_from_path(data = logos, aes(x = -.063, path = logo), width = .029)
ggsave(file.path(out, 'def_pressure_rate.png'), p, width = 11, height = 14, dpi = 180, device = ragg::agg_png)
if (interactive()) print(p)
message('Saved chart and audit data to ', out)
