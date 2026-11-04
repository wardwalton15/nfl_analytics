# 2026 explosive plays vs three-and-outs, REG weeks 1-8
Coverage: 121 games, 32 teams, 14851 run/pass plays, 2673 drives.
nflverse source timestamp: 2026-11-03 05:03:31 EST
Source: https://nflreadr.nflverse.com/reference/load_pbp.html
Explosive: run play_type (scrambles included) with 10+ yards gained, or pass play_type with 20+.
Yards gained excludes penalty yardage. No-plays, two-point tries and deleted plays excluded.
Three-and-out: fixed_drive with zero first downs (incl. by penalty) and fixed_drive_result Punt.
Both axes are per game played (teams have different bye weeks). Rates per play/drive in team_summary.csv.
Y-axis is reversed so fewer three-and-outs is higher. Dashed lines = unweighted team means.
Logos are separated deterministically for readability. Leaders/dots show displaced true locations.
PNG exports: 1080 x 1920 and 2160 x 3840.
Recreate: Rscript explosive_three_and_out.r 2026 8
