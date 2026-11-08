# Run: Rscript epa_team_tiers_trend.r [season=2026] [YYYY-MM-DD = today] [games=4] [optional PBP .rds]
# Season-to-date EPA tiers (logo = current position) with a trail showing how each team's
# cumulative position moved over its own last N games (bye weeks handled per team).
# Packages: nflreadr, dplyr, ggplot2, ggpath, ragg.
.libPaths(c('/private/tmp/nfl-chart-r-library', .libPaths()))
suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(grid)})
args <- commandArgs(trailingOnly = TRUE)
season_id <- if (length(args) >= 1) as.integer(args[1]) else 2026L
before_date <- if (length(args) >= 2) as.Date(args[2]) else Sys.Date()
n_recent <- if (length(args) >= 3) as.integer(args[3]) else 4L
stopifnot(!is.na(season_id), !is.na(before_date), n_recent >= 1)
out_dir <- sprintf('outputs/epa_team_tiers_trend_%s_entering_%s', season_id, before_date)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
cache <- file.path(out_dir, 'source_pbp.rds')
source_path <- if (length(args) >= 4) args[4] else if (file.exists(cache)) cache else NULL
pbp <- if (!is.null(source_path)) readRDS(source_path) else nflreadr::load_pbp(season_id)
source_time <- attr(pbp, 'nflverse_timestamp')
season_pbp <- pbp |>
  filter(season == season_id, season_type == 'REG', !coalesce(play_deleted == 1, FALSE),
         as.Date(game_date) < before_date)
week_id <- max(season_pbp$week)
plays <- season_pbp |>
  filter(play_type %in% c('run', 'pass'), is.finite(epa),
         !is.na(posteam), !is.na(defteam), coalesce(qb_kneel, 0) == 0,
         coalesce(qb_spike, 0) == 0, coalesce(two_point_attempt, 0) == 0,
         between(wp, .02, .98))
if (!nrow(plays)) stop('No eligible plays before ', before_date)
stopifnot(!anyDuplicated(plays[c('game_id', 'play_id')]))
saveRDS(season_pbp, cache)

# Per-team game log, then cumulative (season-to-date) EPA after each game.
team_games <- bind_rows(
  plays |> group_by(team = posteam, game_id, game_date) |>
    summarise(off_n = n(), off_sum = sum(epa), def_n = 0L, def_sum = 0, .groups = 'drop'),
  plays |> group_by(team = defteam, game_id, game_date) |>
    summarise(off_n = 0L, off_sum = 0, def_n = n(), def_sum = sum(epa), .groups = 'drop')
) |>
  group_by(team, game_id, game_date) |>
  summarise(across(c(off_n, off_sum, def_n, def_sum), sum), .groups = 'drop') |>
  arrange(team, game_date) |>
  group_by(team) |>
  mutate(game_num = row_number(), games = n(),
         offense_epa = cumsum(off_sum) / cumsum(off_n),
         defense_epa_allowed = cumsum(def_sum) / cumsum(def_n)) |>
  ungroup()
if (n_distinct(team_games$team) != 32) stop('Expected 32 teams, found ', n_distinct(team_games$team))
if (min(team_games$games) <= n_recent) stop('Every team needs more than ', n_recent, ' games.')

# Trail = cumulative position before the last N games, then after each of them.
trail <- team_games |> filter(game_num >= games - n_recent) |>
  group_by(team) |> mutate(step = game_num - min(game_num)) |> ungroup()
teams <- trail |> group_by(team) |>
  summarise(games = first(games),
            start_offense_epa = first(offense_epa), start_defense_epa = first(defense_epa_allowed),
            offense_epa = last(offense_epa), defense_epa_allowed = last(defense_epa_allowed),
            .groups = 'drop') |>
  mutate(net_epa = offense_epa - defense_epa_allowed,
         start_net_epa = start_offense_epa - start_defense_epa,
         net_change = net_epa - start_net_epa)
recent <- team_games |> filter(game_num > games - n_recent) |> group_by(team) |>
  summarise(recent_offense_epa = sum(off_sum) / sum(off_n),
            recent_defense_epa = sum(def_sum) / sum(def_n), .groups = 'drop')
teams <- left_join(teams, recent, by = 'team') |>
  mutate(recent_net_epa = recent_offense_epa - recent_defense_epa)
stopifnot(abs(sum(team_games$off_n) - nrow(plays)) == 0, abs(sum(team_games$def_n) - nrow(plays)) == 0)

cuts <- c(-Inf, -.20, -.05, .05, .20, Inf)
tier_names <- c('STRUGGLING', 'SHAKY', 'MIDDLE', 'STRONG', 'ELITE')
tier_colors <- c('#944D4B', '#8A643D', '#69665E', '#617345', '#2E7058')
band_colors <- c('#EED8D3', '#F0E3CE', '#E8E5DC', '#E0E7CF', '#CFE3D6')
# Trend direction on net EPA/play change; a small dead zone keeps noise neutral.
trend_cut <- .03
up_col <- '#2E7058'; down_col <- '#A8433F'; flat_col <- '#8C877A'
teams <- teams |>
  mutate(tier = cut(net_epa, cuts, labels = tier_names, right = FALSE),
         start_tier = cut(start_net_epa, cuts, labels = tier_names, right = FALSE),
         trend = case_when(net_change >= trend_cut ~ 'UP', net_change <= -trend_cut ~ 'DOWN', TRUE ~ 'FLAT')) |>
  arrange(desc(net_epa), team)
trail <- trail |> left_join(select(teams, team, trend), by = 'team')
write.csv(teams, file.path(out_dir, 'team_summary.csv'), row.names = FALSE)
write.csv(trail |> select(team, step, game_id, game_date, offense_epa, defense_epa_allowed),
          file.path(out_dir, 'trail_positions.csv'), row.names = FALSE)
write.csv(plays |> select(game_id, play_id, posteam, defteam, play_type, epa),
          file.path(out_dir, 'play_audit.csv'), row.names = FALSE)

logo_dir <- file.path(out_dir, 'logos')
dir.create(logo_dir, showWarnings = FALSE)
existing_dirs <- list.files('outputs', full.names = TRUE)
for (tm in teams$team) {
  dest <- file.path(logo_dir, paste0(tm, '.png'))
  if (!file.exists(dest)) {
    found <- file.path(existing_dirs, 'logos', paste0(tm, '.png'))
    found <- found[file.exists(found)]
    if (length(found)) file.copy(found[1], dest)
  }
  if (!file.exists(dest)) {
    meta <- nflreadr::load_teams()
    url <- meta$team_logo_espn[match(tm, meta$team_abbr)]
    if (is.na(url)) stop('No logo available for ', tm)
    download.file(url, dest, mode = 'wb', quiet = TRUE)
  }
}
teams$logo <- file.path(logo_dir, paste0(teams$team, '.png'))

# Square window framed on current positions (45° bands stay true); long trails run off the edge.
xr <- range(teams$offense_epa); yr <- range(-teams$defense_epa_allowed)
lim <- (max(diff(xr), diff(yr)) + .12) / 2
xl <- mean(xr) + c(-lim, lim); yl <- mean(yr) + c(-lim, lim)
break_step <- if (lim > .3) .2 else .1
clip_band <- function(poly, threshold, above) {
  if (!nrow(poly)) return(poly)
  output <- matrix(numeric(0), ncol = 2)
  inside <- function(p) if (above) sum(p) >= threshold else sum(p) <= threshold
  for (i in seq_len(nrow(poly))) {
    a <- poly[i, ]; b <- poly[if (i == nrow(poly)) 1 else i + 1, ]
    ia <- inside(a); ib <- inside(b)
    if (ia) output <- rbind(output, a)
    if (ia != ib) output <- rbind(output, a + (b - a) * (threshold - sum(a)) / sum(b - a))
  }
  output
}
bands <- bind_rows(lapply(seq_along(tier_names), function(i) {
  p <- rbind(c(xl[1], yl[1]), c(xl[2], yl[1]), c(xl[2], yl[2]), c(xl[1], yl[2]))
  if (is.finite(cuts[i])) p <- clip_band(p, cuts[i], TRUE)
  if (is.finite(cuts[i + 1])) p <- clip_band(p, cuts[i + 1], FALSE)
  data.frame(x = p[, 1], y = p[, 2], tier = tier_names[i])
}))

# Deterministic logo collision resolution; leaders show displaced true locations.
anchor <- cbind(teams$offense_epa, -teams$defense_epa_allowed)
pos <- anchor
min_distance <- 2 * lim * .075
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
  pos[, 1] <- pmax(xl[1] + min_distance / 2, pmin(xl[2] - min_distance / 2, pos[, 1]))
  pos[, 2] <- pmax(yl[1] + min_distance / 2, pmin(yl[2] - min_distance / 2, pos[, 2]))
}
teams$logo_x <- pos[, 1]; teams$logo_y <- pos[, 2]
teams$moved <- sqrt(rowSums((pos - anchor)^2)) > .004
write.csv(teams |> select(team, offense_epa, defense_epa_allowed, logo_x, logo_y, moved),
          file.path(out_dir, 'logo_positions.csv'), row.names = FALSE)

# One straight trend line per team: cumulative position N games ago -> now.
trail_lines <- teams |> mutate(flat = trend == 'FLAT') |> arrange(!flat)
trend_values <- c(UP = up_col, DOWN = down_col, FLAT = flat_col)
bg <- '#F7F3EA'; ink <- '#292923'; muted <- '#716E64'
plot <- ggplot() +
  geom_polygon(data = bands, aes(x, y, group = tier, fill = tier)) +
  scale_fill_manual(values = setNames(band_colors, tier_names), guide = 'none') +
  geom_abline(intercept = cuts[2:5], slope = -1, color = '#999383', linewidth = .35, alpha = .35) +
  geom_hline(yintercept = 0, color = '#898477', linewidth = .35, linetype = 'dashed', alpha = .65) +
  geom_vline(xintercept = 0, color = '#898477', linewidth = .35, linetype = 'dashed', alpha = .65) +
  # Flat trails first and faint so trending trails sit on top.
  geom_segment(data = trail_lines,
               aes(start_offense_epa, -start_defense_epa, xend = offense_epa,
                   yend = -defense_epa_allowed, color = trend, alpha = flat, linewidth = flat),
               lineend = 'round') +
  geom_point(data = trail_lines, aes(start_offense_epa, -start_defense_epa, color = trend, alpha = flat),
             shape = 21, fill = bg, size = 1.7, stroke = .9) +
  scale_color_manual(values = trend_values, guide = 'none') +
  scale_alpha_manual(values = c(`FALSE` = .9, `TRUE` = .45), guide = 'none') +
  scale_linewidth_manual(values = c(`FALSE` = 1.1, `TRUE` = .6), guide = 'none') +
  geom_segment(data = filter(teams, moved),
               aes(x = offense_epa, y = -defense_epa_allowed, xend = logo_x, yend = logo_y),
               color = '#716E64', linewidth = .35) +
  geom_point(data = filter(teams, moved), aes(offense_epa, -defense_epa_allowed),
             color = '#716E64', size = .9) +
  ggpath::geom_from_path(data = teams, aes(logo_x, logo_y, path = logo), width = .064) +
  scale_x_continuous(breaks = seq(ceiling(xl[1] / break_step), floor(xl[2] / break_step)) * break_step,
                     labels = scales::label_number(accuracy = .1)) +
  scale_y_continuous(breaks = seq(ceiling(yl[1] / break_step), floor(yl[2] / break_step)) * break_step,
                     labels = function(x) sprintf('%.1f', ifelse(abs(x) < 1e-8, 0, -x))) +
  coord_fixed(xlim = xl, ylim = yl, expand = FALSE) +
  labs(x = 'OFFENSIVE EPA / PLAY  →', y = 'DEFENSIVE EPA / PLAY ALLOWED  →') +
  theme_minimal(base_family = 'Arial', base_size = 12) +
  theme(plot.background = element_rect(fill = bg, color = NA),
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, color = '#C7C0B1', linewidth = .5),
        axis.text = element_text(color = muted, size = 11),
        axis.title = element_text(color = ink, size = 12, face = 'bold'),
        axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(r = 10)),
        plot.margin = margin(5, 5, 5, 5))

# Biggest movers strip: top 3 risers and fallers by change in season net EPA/play.
fmt <- function(v, digits = 2) sub('-', '−', sprintf(paste0('%+.', digits, 'f'), v))
movers <- bind_rows(
  teams |> slice_max(net_change, n = 3, with_ties = FALSE) |> mutate(row = 'UP', rank = row_number()),
  teams |> slice_min(net_change, n = 3, with_ties = FALSE) |> mutate(row = 'DOWN', rank = row_number())
) |> mutate(x = rank, y = if_else(row == 'UP', 2, 1), label = fmt(net_change))
movers_plot <- ggplot(movers) +
  ggpath::geom_from_path(aes(x - .2, y, path = logo), width = .085) +
  geom_text(aes(x + .08, y, label = label, color = row), hjust = 0, family = 'Arial',
            fontface = 'bold', size = 4.6) +
  scale_color_manual(values = c(UP = up_col, DOWN = down_col), guide = 'none') +
  coord_cartesian(xlim = c(.6, 3.75), ylim = c(.5, 2.5), expand = FALSE) +
  theme_void() + theme(plot.background = element_rect(fill = bg, color = NA))

draw_poster <- function(path, scale = 1) {
  ragg::agg_png(path, width = 1080 * scale, height = 1920 * scale, res = 144 * scale, background = bg)
  grid.newpage()
  txt <- function(label, x, y, size, color = ink, face = 'plain', just = 'left') {
    grid.text(label, x, y, just = just,
              gp = gpar(fontfamily = 'Arial', fontsize = size, col = color, fontface = face))
  }
  grid.roundrect(x = .205, y = .935, width = .29, height = .029,
                 r = unit(.008, 'npc'), gp = gpar(fill = '#2E7058', col = NA))
  txt(sprintf('ENTERING %s', toupper(format(before_date, '%b %d'))), .205, .935, 13, bg, 'bold', 'center')
  txt('NFL TEAM TIERS', .06, .875, 38, face = 'bold')
  txt(sprintf('WHO’S TRENDING  ·  SEASON EPA OVER THE LAST %d GAMES', n_recent), .062, .835, 11.5, muted, 'bold')
  grid.lines(x = c(.06, .92), y = c(.812, .812), gp = gpar(col = '#C7C0B1', lwd = 1))
  # Trail key
  ky <- .789
  grid.lines(x = c(.065, .115), y = c(ky, ky), gp = gpar(col = up_col, lwd = 2.6, lineend = 'round'))
  grid.points(unit(.065, 'npc'), unit(ky, 'npc'), pch = 21, size = unit(5, 'pt'), gp = gpar(col = up_col, fill = bg, lwd = 1.4))
  txt(sprintf('%d GAMES AGO  →  LOGO = NOW', n_recent), .128, ky, 10.5, ink, 'bold')
  txt('▲ RISING', .74, ky, 10.5, up_col, 'bold', 'right')
  txt('▼ FALLING', .92, ky, 10.5, down_col, 'bold', 'right')
  print(plot, newpage = FALSE, vp = viewport(x = .485, y = .525, width = .90, height = .505))
  # Tier legend
  txt('TIERS BY NET EPA / PLAY', .065, .255, 11, face = 'bold')
  for (j in 1:5) {
    i <- 6 - j; x <- .065 + (j - 1) * .175
    grid.roundrect(x = x + .079, y = .225, width = .162, height = .024,
                   r = unit(.005, 'npc'), gp = gpar(fill = band_colors[i], col = NA))
    txt(tier_names[i], x + .079, .225, 9.4, tier_colors[i], 'bold', 'center')
    label <- if (i == 1) paste('<', fmt(cuts[2])) else if (i == 5) paste('≥', fmt(cuts[5])) else
      paste(fmt(cuts[i]), 'to', fmt(cuts[i + 1]))
    txt(label, x + .079, .202, 8.6, muted, just = 'center')
  }
  # Movers
  txt(sprintf('BIGGEST MOVERS  ·  CHANGE IN NET EPA / PLAY, LAST %d GAMES', n_recent), .065, .163, 11, face = 'bold')
  txt('RISING', .065, .1235, 10.5, up_col, 'bold')
  txt('FALLING', .065, .0925, 10.5, down_col, 'bold')
  print(movers_plot, newpage = FALSE, vp = viewport(x = .59, y = .108, width = .66, height = .062))
  txt('SOURCE: NFLVERSE', .065, .038, 11, muted, 'bold')
  txt('WARD WALTON', .92, .038, 11, muted, 'bold', 'right')
  dev.off()
}
draw_poster(file.path(out_dir, 'epa_team_tiers_trend_tiktok.png'))
draw_poster(file.path(out_dir, 'epa_team_tiers_trend_tiktok_2x.png'), 2)
writeLines(c(
  sprintf('# %s EPA/play team tiers with %d-game trend trails, REG games before %s (through week %s)',
          season_id, n_recent, before_date, week_id),
  sprintf('Coverage: %s games, %s teams, %s eligible plays.',
          n_distinct(plays$game_id), nrow(teams), nrow(plays)),
  paste('nflverse source timestamp:', paste(source_time, collapse = ' ')),
  'Logo = season-to-date offense EPA/play vs defense EPA/play allowed (same as epa_team_tiers.r date mode).',
  sprintf('Trend line = straight path from the team\'s cumulative season position before its last %d games to now.', n_recent),
  'Window is framed on current positions; long trend lines may run off the plot edge (see trail_positions.csv).',
  'Uses each team\'s own games, so bye weeks do not shorten the window. Hollow dot marks the start.',
  sprintf('Trend colour: net EPA/play change >= +%.2f rising, <= -%.2f falling, otherwise neutral grey.',
          trend_cut, trend_cut),
  'Biggest movers rank the same season net EPA/play change (net_change in team_summary.csv).',
  sprintf('team_summary.csv also lists recent_*: EPA over only the last %d games.', n_recent),
  sprintf('Tier cutoffs: Struggling < %.2f; Shaky [%.2f,%.2f); Middle [%.2f,%.2f); Strong [%.2f,%.2f); Elite >= %.2f.',
          cuts[2], cuts[2], cuts[3], cuts[3], cuts[4], cuts[4], cuts[5], cuts[5]),
  'Play filter: run/pass, finite EPA, WP 2-98%; no kneels, spikes, two-point tries or deleted plays.',
  'No opponent adjustment or predictive claim.',
  sprintf('Recreate: Rscript epa_team_tiers_trend.r %s %s %s', season_id, before_date, n_recent)
), file.path(out_dir, 'README.md'))
print(teams |> select(team, games, net_epa, tier, start_tier, net_change, trend, recent_net_epa), n = 32)
message('Saved chart and audit files to ', out_dir)
