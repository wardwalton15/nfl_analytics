# 2026 turnover differential, REG weeks 1-9
Coverage: 136 games, 32 teams, 354 turnovers (197 INT, 157 lost fumbles).
nflverse source timestamp: 2026-11-10 05:08:17 EST
Source: https://nflreadr.nflverse.com/reference/load_pbp.html
Giveaway: interception thrown (posteam) or fumble_lost == 1 (charged to fumbled_1_team).
Takeaway: the opponent in that game. All phases count, including special teams and returns.
Excluded: deleted plays and play_type no_play (turnovers nullified by penalty).
Season totals, not per game: teams with an extra bye have played fewer games (see games column).
Every play is in turnover_audit.csv; differentials sum to zero across the league.
Bars use nflverse team_color at 55% opacity. Ties ordered by takeaways, then team.
Recreate: Rscript turnover_differential.r 2026 9 outputs/turnover_differential_2026_through_week_9/source_pbp.rds
