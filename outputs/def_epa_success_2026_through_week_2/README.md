# Defensive efficiency: 2026 through Week 2
32 teams; 32 games; 3830 eligible plays.
Grouped by defending team. EPA allowed/play = mean offensive EPA; opponent success rate = fraction with offensive EPA > 0. Lower is better for both; axes are reversed.
Regular season run/pass plays including sacks/scrambles; exclude deleted plays, kneels, spikes, two-point attempts and nonfinite EPA.
Reference lines are league averages weighted by plays. Highlight rings identify BUF (blue), SF (red) and BAL (purple).
Logos are displaced to reduce overlap; dots and leader lines preserve exact coordinates.
Source: nflverse play-by-play via nflreadr. Saved source_pbp.rds supports offline reproduction.
Recreate: Rscript def_epa_success_rate.r 2026 2
