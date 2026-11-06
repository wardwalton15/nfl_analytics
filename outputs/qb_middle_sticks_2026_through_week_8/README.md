# 2026 QB middle-of-field % vs past-the-sticks %, REG weeks 1-8
Coverage: 121 games, 28 qualifying QBs (min. 150 attempts), 7398 league attempts.
nflverse source timestamp: 2026-11-05 05:03:23 EST
Source: https://nflreadr.nflverse.com/reference/load_pbp.html
Attempts = pass_attempt == 1 excluding sacks, spikes, two-point tries; WP 2-98% inclusive.
Middle = pass_location == "middle". Past sticks = air_yards >= ydstogo (goal-to-go: reaches the goal line).
League average: 21.1% middle, 42.3% past sticks (dashed lines).
QB team = team with most of his attempts. Headshots separated deterministically; leaders/dots show true spots.
Recreate: Rscript qb_middle_sticks.r 2026 8 150 outputs/qb_middle_sticks_2026_through_week_8/source_pbp.rds
