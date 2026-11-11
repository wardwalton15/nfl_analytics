# 2026 under-center usage vs EPA/play, REG Weeks 1–9
Coverage: 136 games, 32 teams, 14502 eligible plays (0 dropped for missing formation).
nflverse source timestamp: 2026-11-11 05:03:51 EST
Under center = shotgun == 0 in nflverse PBP (pistol and under-center both count). Not charted formation data.
Plays: runs + passes (incl. sacks, scrambles); pre-play WP 5-95%; no kneels, spikes, 2-pt tries, no-plays.
League: 38.3% under center, -0.018 EPA/play under center. Team correlation r = 0.45 (p = 0.010).
source_pbp.rds stores these games so a rerun with it reproduces the chart without downloading.
Recreate: Rscript under_center_rate.r 2026 9 outputs/under_center_2026_through_week_9/source_pbp.rds
