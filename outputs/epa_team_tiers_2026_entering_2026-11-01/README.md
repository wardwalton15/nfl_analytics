# 2026 EPA/play team tiers, REG games before 2026-11-01 (through week 8)
Coverage: 108 games, 32 teams, 12273 eligible plays.
nflverse source timestamp: 2026-10-30 07:36:55 EDT
Source: https://nflreadr.nflverse.com/reference/load_pbp.html
Offense: mean offensive EPA. Defense: mean opponent EPA allowed (lower is better).
Net EPA/play = offense EPA/play minus defense EPA/play allowed.
Tier cutoffs are editorial, fixed in raw EPA units, and lower-bound inclusive:
Struggling < -0.20; Shaky [-0.20,-0.05); Middle [-0.05,0.05); Strong [0.05,0.20); Elite >= 0.20.
These describe the selected games only, with no opponent adjustment or predictive claim.
Eligible plays: finite EPA, run/pass play_type, nonmissing offense/defense; includes sacks/scrambles.
Only plays with offensive win probability between 2% and 98% (inclusive) are counted.
Exclude deleted plays, kneels, spikes, two-point attempts, no-plays and special teams.
Axes: offense increases right; defense allowed decreases up. Dashed lines indicate zero EPA.
Logos are separated deterministically for readability. Leaders/dots show displaced true locations.
PNG exports: 1080 x 1920 and 2160 x 3840. All tier values also appear in team_summary.csv.
source_pbp.rds stores this week so a default rerun reproduces the chart without downloading.
Recreate: Rscript epa_team_tiers.r 2026 2026-11-01
