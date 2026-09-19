# QB touchdown drive rate
2024–2026 regular season • Through 2026-09-17, Week 2 • Min. 20 starts
Excluded: drives starting in Q3–Q4 with win probability below 5% or above 95%.
Win probability is posteam wp before the first offensive down; thresholds are strict.
The filter excludes whole drives. Overtime remains included. Start eligibility is unchanged.
Garbage-time drives excluded before QB eligibility: 1815.
Rate = attributed offensive TD drives / all attributed offensive drives; pooled across seasons.
Eligibility: at least the specified number of regular-season starts since 2024, from schedule QB IDs.
Drives in relief appearances also count for eligible quarterbacks. Teams combined by GSIS player ID.
Corrected fixed_drive IDs; includes kneels, spikes, end-of-half drives, sacks and turnovers.
Kickoffs, conversion attempts and deleted records excluded; requires at least one offensive down.
TD must be credited to the possession team; defensive and return TDs do not qualify.
QB attribution is an estimate: the observed QB passer/rusher identifies a drive. Drives without an
observed QB inherit the most recent observed QB for that offense, falling back to its listed starter.
Handoff-only drives immediately after an unobserved substitution can be misattributed.
Drives with multiple observed QBs are excluded. This is not a snap-participation-based metric.
Excluded mixed-QB drives: 68. Inferred eligible-QB drives: 129 of 7036.
Sources: https://nflreadr.nflverse.com/reference/load_pbp.html
https://nflreadr.nflverse.com/reference/load_schedules.html
https://nflreadr.nflverse.com/reference/load_players.html
Cached RDS files preserve this snapshot. Delete them to download updated inputs.
Recreate: Rscript qb_td_drive_rate.r 2026 20 3 0.05
