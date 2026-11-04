# 2026 rush EPA/play vs rush success rate, REG weeks 1-8
Coverage: 121 games, 32 teams, 5372 designed runs.
nflverse source timestamp: 2026-11-03 05:03:31 EST
Source: https://nflreadr.nflverse.com/reference/load_pbp.html
Rush = run/pass plays with qb_dropback == 0 (designed runs; scrambles and sacks excluded).
Success = EPA > 0.
League average: -0.073 EPA/rush, 40.5% success (dashed lines).
Only plays with offensive win probability between 2% and 98% (inclusive) are counted.
Exclude deleted plays, kneels, spikes, two-point attempts, no-plays and special teams.
No opponent adjustment. Logos are separated deterministically; leaders/dots show true locations.
source_pbp.rds stores these games so a rerun with it reproduces the chart without downloading.
Recreate: Rscript rush_epa_success.r 2026 8 outputs/rush_epa_success_2026_through_week_8/source_pbp.rds
