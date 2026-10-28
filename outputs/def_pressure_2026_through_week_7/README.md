# Defensive pressure: 2026 through Week 7
Source: outputs/pressure_week7.csv (user-supplied period; CSV has no season/week fields).
All 32 team names and numeric totals validated; 2026 Week 2 requires two games per team.
Chart rate = published PFR Prss% from the CSV. Sacks are included in pressures.
Sack shading = published pressure rate times Sk / Prss; a proportional split of the rounded rate.
Dashed reference = unweighted mean of team PFR rates, not a pooled league rate.
Source snapshot: source_pressure.csv.
TikTok exports: def_pressure_rate_tiktok.png (1080 x 1920) and _2x.png (2160 x 3840).
Recreate: Rscript def_pressure_rate.r 2026 7 outputs/pressure_week7.csv
