# 2026 QB air yards per attempt vs EPA per dropback, REG weeks 1-9
Coverage: 136 games, 30 qualifying QBs (min. 150 attempts), 9302 league dropbacks.
nflverse source timestamp: 2026-11-11 05:03:51 EST
Source: https://nflreadr.nflverse.com/reference/load_pbp.html
Dropbacks = qb_dropback == 1 (attempts, sacks, scrambles) excluding spikes, two-point tries; WP 2-98% inclusive.
EPA/play = mean EPA per dropback. Air yards/att = mean air_yards on non-sack pass attempts (incompletions included).
League average: 7.81 air yards/att, +0.050 EPA/dropback (dashed lines).
QB team = team with most of his attempts. Headshots separated deterministically; leaders/dots show true spots.
Recreate: Rscript qb_air_yards_epa.r 2026 9 150 outputs/qb_air_yards_epa_2026_through_week_9/source_pbp.rds
