# 2026 QB share of passing yards from YAC, REG weeks 1-9
Coverage: 136 games, 30 qualifying QBs (min. 150 attempts), top 10 shown.
nflverse source timestamp: 2026-11-12 12:34:16 EST
Source: https://nflreadr.nflverse.com/reference/load_pbp.html
Attempts = pass_attempt == 1 excluding sacks and two-point tries. No WP filter (counting stat).
YAC share = sum(yards_after_catch) / sum(passing_yards) on completions.
League YAC share: 46.6%. Completions missing yards_after_catch: 0 (counted as 0).
Recreate: Rscript qb_yac_share.r 2026 9 150 10 outputs/qb_yac_share_2026_through_week_9/source_pbp.rds
