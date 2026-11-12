# 2026 red zone outcomes, REG Weeks 1–9
Coverage: 136 games, 32 teams, 908 red zone trips. League TD rate 57.8%.
nflverse source timestamp: 2026-11-12 12:34:16 EST
Trip: offensive drive (fixed_drive) with any run/pass/FG/punt/kneel/spike/no_play snap at yardline_100 <= 20; PATs and 2-pt tries excluded.
Outcome from fixed_drive_result: Touchdown; FG attempt = made + missed FG; Turnover = INT/lost fumble incl. Opp touchdown;
Downs = turnover on downs; End of half = clock expired (plus any rare punt/safety). Every trip is in trip_audit.csv.
Sorted by TD count, ties by TD rate. Totals are not per game: check the games column for bye weeks.
Recreate: Rscript red_zone_outcomes.r 2026 9 outputs/red_zone_outcomes_2026_through_week_9/source_pbp.rds
