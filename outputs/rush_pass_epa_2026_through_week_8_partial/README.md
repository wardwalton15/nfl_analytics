# 2026 rush vs pass EPA/play, REG weeks 1-8 (week 8 partial: 9 of 14 games)
Coverage: 116 games, 32 teams, 13119 eligible plays.
nflverse source timestamp: 2026-11-01 17:03:13 EST
Source: https://nflreadr.nflverse.com/reference/load_pbp.html
Pass = qb_dropback plays (includes sacks and scrambles). Rush = all other run/pass plays (designed runs).
League average: rush -0.073, pass 0.061 EPA/play (dashed lines).
Only plays with offensive win probability between 2% and 98% (inclusive) are counted.
Exclude deleted plays, kneels, spikes, two-point attempts, no-plays and special teams.
No opponent adjustment. Logos are separated deterministically; leaders/dots show true locations.
source_pbp.rds stores these games so a rerun with it reproduces the chart without downloading.
Recreate: Rscript rush_pass_epa.r 2026 8 outputs/rush_pass_epa_2026_through_week_8_partial/source_pbp.rds
