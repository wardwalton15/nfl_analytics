# Run: Rscript epa_team_tiers.r [season=2026] [week=1] [optional PBP .rds]
# Packages: nflreadr, dplyr, ggplot2, ggpath, ragg (and ggpath dependencies).
# The /tmp library is optional; it supports this project's existing local setup.
.libPaths(c('/private/tmp/nfl-chart-r-library', .libPaths()))
suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(grid)})
args <- commandArgs(trailingOnly = TRUE)
season_id <- if (length(args) >= 1) as.integer(args[1]) else 2026L
week_id <- if (length(args) >= 2) as.integer(args[2]) else 1L
stopifnot(!is.na(season_id), !is.na(week_id), week_id >= 1, week_id <= 18)
out_dir <- sprintf('outputs/epa_team_tiers_%s_week_%s', season_id, week_id)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
cache <- file.path(out_dir, 'source_pbp.rds')
source_path <- if (length(args) >= 3) args[3] else if (file.exists(cache)) cache else NULL
pbp <- if (!is.null(source_path)) readRDS(source_path) else nflreadr::load_pbp(season_id)
source_time <- attr(pbp, 'nflverse_timestamp')
week_pbp <- pbp |>
  filter(season == season_id, season_type == 'REG', week == week_id,
         !coalesce(play_deleted == 1, FALSE))
plays <- week_pbp |>
  filter(play_type %in% c('run', 'pass'), is.finite(epa),
         !is.na(posteam), !is.na(defteam), coalesce(qb_kneel, 0) == 0,
         coalesce(qb_spike, 0) == 0, coalesce(two_point_attempt, 0) == 0)
if (!nrow(plays)) stop('No eligible plays for requested season/week.')
stopifnot(!anyDuplicated(plays[c('game_id', 'play_id')]))
offense <- plays |> group_by(team = posteam) |>
  summarise(off_plays = n(), offense_epa = mean(epa), .groups = 'drop')
defense <- plays |> group_by(team = defteam) |>
  summarise(def_plays = n(), defense_epa_allowed = mean(epa), .groups = 'drop')
teams <- inner_join(offense, defense, by = 'team') |>
  mutate(net_epa = offense_epa - defense_epa_allowed)
games <- n_distinct(week_pbp$game_id)
if (week_id == 1L && (games != 16L || nrow(teams) != 32L)) {
  stop('Week 1 coverage is incomplete: ', games, ' games / ', nrow(teams), ' teams.')
}
stopifnot(sum(teams$off_plays) == nrow(plays), sum(teams$def_plays) == nrow(plays),
          all(is.finite(teams$net_epa)), !anyDuplicated(teams$team),
          abs(weighted.mean(teams$offense_epa, teams$off_plays) -
              weighted.mean(teams$defense_epa_allowed, teams$def_plays)) < 1e-10)
saveRDS(week_pbp, cache)

# Explicit editorial tiers of single-week net EPA; these are not predictive rankings.
cuts <- c(-Inf, -.30, -.10, .10, .30, Inf)
tier_names <- c('STRUGGLING', 'SHAKY', 'MIDDLE', 'STRONG', 'ELITE')
tier_colors <- c('#944D4B', '#8A643D', '#69665E', '#617345', '#2E7058')
band_colors <- c('#EED8D3', '#F0E3CE', '#E8E5DC', '#E0E7CF', '#CFE3D6')
teams <- teams |>
  mutate(tier = cut(net_epa, cuts, labels = tier_names, right = FALSE)) |>
  arrange(desc(net_epa), team)
write.csv(teams, file.path(out_dir, 'team_summary.csv'), row.names = FALSE)
write.csv(plays |> select(game_id, play_id, posteam, defteam, play_type, epa),
          file.path(out_dir, 'play_audit.csv'), row.names = FALSE)

# Reuse existing logos, then download missing ones from nflverse team metadata.
logo_dir <- file.path(out_dir, 'logos')
dir.create(logo_dir, showWarnings = FALSE)
for (tm in teams$team) {
  dest <- file.path(logo_dir, paste0(tm, '.png'))
  existing <- file.path('outputs/first_down_drives_2025/logos', paste0(tm, '.png'))
  if (!file.exists(dest) && file.exists(existing)) file.copy(existing, dest)
  if (!file.exists(dest)) {
    meta <- nflreadr::load_teams()
    url <- meta$team_logo_espn[match(tm, meta$team_abbr)]
    if (is.na(url)) stop('No logo available for ', tm)
    download.file(url, dest, mode = 'wb', quiet = TRUE)
  }
}
teams$logo <- file.path(logo_dir, paste0(teams$team, '.png'))

# Defensive axis is flipped so up/right always means better.
lim <- ceiling((max(abs(c(teams$offense_epa, teams$defense_epa_allowed))) + .075) * 10) / 10
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
  p <- rbind(c(-lim, -lim), c(lim, -lim), c(lim, lim), c(-lim, lim))
  if (is.finite(cuts[i])) p <- clip_band(p, cuts[i], TRUE)
  if (is.finite(cuts[i + 1])) p <- clip_band(p, cuts[i + 1], FALSE)
  data.frame(x = p[, 1], y = p[, 2], tier = tier_names[i])
}))

# Deterministic collision resolution. Dots and leaders preserve true coordinates.
anchor <- cbind(teams$offense_epa, -teams$defense_epa_allowed)
pos <- anchor
min_distance <- 2 * lim * .083
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
  pos[] <- pmax(-lim + min_distance / 2, pmin(lim - min_distance / 2, pos))
}
teams$logo_x <- pos[, 1]; teams$logo_y <- pos[, 2]
teams$moved <- sqrt(rowSums((pos - anchor)^2)) > .004
write.csv(teams |> select(team, offense_epa, defense_epa_allowed, logo_x, logo_y, moved),
          file.path(out_dir, 'logo_positions.csv'), row.names = FALSE)
bg <- '#F7F3EA'; ink <- '#292923'; muted <- '#716E64'
plot <- ggplot() +
  geom_polygon(data = bands, aes(x, y, group = tier, fill = tier)) +
  scale_fill_manual(values = setNames(band_colors, tier_names), guide = 'none') +
  geom_abline(intercept = cuts[2:5], slope = -1, color = '#999383',
              linewidth = .35, alpha = .35) +
  geom_hline(yintercept = 0, color = '#898477', linewidth = .35, linetype = 'dashed', alpha = .65) +
  geom_vline(xintercept = 0, color = '#898477', linewidth = .35, linetype = 'dashed', alpha = .65) +
  geom_segment(data = filter(teams, moved),
               aes(x = offense_epa, y = -defense_epa_allowed, xend = logo_x, yend = logo_y),
               color = '#716E64', linewidth = .4) +
  geom_point(data = filter(teams, moved), aes(offense_epa, -defense_epa_allowed),
             color = '#716E64', size = 1.1) +
  ggpath::geom_from_path(data = teams, aes(logo_x, logo_y, path = logo), width = .070) +
  scale_x_continuous(breaks = seq(-floor(lim / .2), floor(lim / .2)) * .2,
                     labels = scales::label_number(accuracy = .1)) +
  scale_y_continuous(breaks = seq(-floor(lim / .2), floor(lim / .2)) * .2,
                     labels = function(x) sprintf('%.1f', ifelse(abs(x) < 1e-8, 0, -x))) +
  coord_fixed(xlim = c(-lim, lim), ylim = c(-lim, lim), expand = FALSE) +
  labs(x = 'OFFENSIVE EPA / PLAY  →', y = 'DEFENSIVE EPA / PLAY ALLOWED  →') +
  theme_minimal(base_family = 'Arial', base_size = 12) +
  theme(plot.background = element_rect(fill = bg, color = NA),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
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
  grid.roundrect(x = .205, y = .928, width = .29, height = .029,
                 r = unit(.008, 'npc'), gp = gpar(fill = '#2E7058', col = NA))
  txt(sprintf('%s  /  WEEK %02d', season_id, week_id), .205, .928, 13, bg, 'bold', 'center')
  txt('NFL TEAM TIERS', .06, .875, 38, face = 'bold')
  txt('Who actually owned the week?', .063, .833, 18, muted)
  grid.lines(x = c(.06, .92), y = c(.802, .802), gp = gpar(col = '#C7C0B1', lwd = 1))
  txt('EPA / PLAY', .065, .779, 12, face = 'bold')
  txt('BETTER TEAMS  ↗', .92, .779, 12, '#2E7058', 'bold', 'right')
  print(plot, newpage = FALSE, vp = viewport(x = .485, y = .515, width = .90, height = .515))
  txt('TIERS BY NET EPA / PLAY', .065, .237, 12, face = 'bold')
  for (j in 1:5) {
    i <- 6 - j; x <- .065 + (j - 1) * .175
    grid.roundrect(x = x + .079, y = .192, width = .162, height = .027,
                   r = unit(.005, 'npc'), gp = gpar(fill = band_colors[i], col = NA))
    txt(tier_names[i], x + .079, .192, 9.8, tier_colors[i], 'bold', 'center')
    label <- c('< −0.30', '−0.30 to −0.10', '−0.10 to +0.10', '+0.10 to +0.30', '≥ +0.30')[i]
    txt(label, x + .079, .167, 9, muted, just = 'center')
  }
  txt('SOURCE: NFLVERSE', .065, .108, 11, muted, 'bold')
  txt('WARD WALTON', .92, .108, 11, muted, 'bold', 'right')
  dev.off()
}
draw_poster(file.path(out_dir, 'epa_team_tiers_tiktok.png'))
draw_poster(file.path(out_dir, 'epa_team_tiers_tiktok_2x.png'), 2)
writeLines(c(
  sprintf('# %s Week %s EPA/play team tiers', season_id, week_id),
  sprintf('Coverage: %s games, %s teams, %s eligible plays.', games, nrow(teams), nrow(plays)),
  paste('nflverse source timestamp:', paste(source_time, collapse = ' ')),
  'Source: https://nflreadr.nflverse.com/reference/load_pbp.html',
  'Offense: mean offensive EPA. Defense: mean opponent EPA allowed (lower is better).',
  'Net EPA/play = offense EPA/play minus defense EPA/play allowed.',
  'Tier cutoffs are editorial, fixed in raw EPA units, and lower-bound inclusive:',
  'Struggling < -0.30; Shaky [-0.30,-0.10); Middle [-0.10,0.10); Strong [0.10,0.30); Elite >= 0.30.',
  'These describe the selected week only, with no opponent adjustment or predictive claim.',
  'Eligible plays: finite EPA, run/pass play_type, nonmissing offense/defense; includes sacks/scrambles.',
  'Exclude deleted plays, kneels, spikes, two-point attempts, no-plays and special teams.',
  'Axes: offense increases right; defense allowed decreases up. Dashed lines indicate zero EPA.',
  'Logos are separated deterministically for readability. Leaders/dots show displaced true locations.',
  'PNG exports: 1080 x 1920 and 2160 x 3840. All tier values also appear in team_summary.csv.',
  'source_pbp.rds stores this week so a default rerun reproduces the chart without downloading.',
  sprintf('Recreate: Rscript epa_team_tiers.r %s %s', season_id, week_id)
), file.path(out_dir, 'README.md'))
print(teams |> select(team, offense_epa, defense_epa_allowed, net_epa, tier), n = 32)
message('Saved chart and audit files to ', out_dir)
