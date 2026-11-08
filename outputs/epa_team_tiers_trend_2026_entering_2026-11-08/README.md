# 2026 EPA/play team tiers with 4-game trend trails, REG games before 2026-11-08 (through week 9)
Coverage: 122 games, 32 teams, 13760 eligible plays.
nflverse source timestamp: 2026-11-07 10:40:02 EST
Logo = season-to-date offense EPA/play vs defense EPA/play allowed (same as epa_team_tiers.r date mode).
Trend line = straight path from the team's cumulative season position before its last 4 games to now.
Window is framed on current positions; long trend lines may run off the plot edge (see trail_positions.csv).
Uses each team's own games, so bye weeks do not shorten the window. Hollow dot marks the start.
Trend colour: net EPA/play change >= +0.03 rising, <= -0.03 falling, otherwise neutral grey.
Biggest movers rank the same season net EPA/play change (net_change in team_summary.csv).
team_summary.csv also lists recent_*: EPA over only the last 4 games.
Tier cutoffs: Struggling < -0.20; Shaky [-0.20,-0.05); Middle [-0.05,0.05); Strong [0.05,0.20); Elite >= 0.20.
Play filter: run/pass, finite EPA, WP 2-98%; no kneels, spikes, two-point tries or deleted plays.
No opponent adjustment or predictive claim.
Recreate: Rscript epa_team_tiers_trend.r 2026 2026-11-08 4
