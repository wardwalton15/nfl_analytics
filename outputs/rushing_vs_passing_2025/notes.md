# 2025 NFL regular season: rushing, winning and EPA

Source: https://github.com/nflverse/nflverse-data/releases/download/pbp/play_by_play_2025.rds
Downloaded September 11, 2026. Reproduce with `Rscript rushing_vs_passing.r`.
Field documentation: https://nflreadr.nflverse.com/articles/dictionary_pbp.html

All 272 regular-season games are present. Three games had equal rushing yards.
The rushing-yard leader won 187 of the remaining 269 games (69.5167%); this
denominator includes one tied game, counted as a non-win. Excluding that tied
game instead gives 187/268 = 69.7761%. Net-passing-yard leaders won 172/271
(63.4686%). These are game-level win frequencies, not correlation coefficients.

Rushing totals sum credited rushing yards, including scrambles and kneels.
EPA and run-frequency samples use actual run/pass plays with nonmissing EPA,
excluding deleted plays, no-play penalties, kneels, spikes and conversions.
Dropbacks include pass attempts, sacks and scrambles; designed runs exclude
scrambles. This sample contains 19,734 dropbacks and 13,079 designed runs.
Leading/trailing status uses the offensive team's score differential before
the play. Rates are pooled across plays, not averaged across teams.

The observed run-rate difference is consistent with reverse causality through
game script: leading changes play selection and accumulated rushing totals.
These descriptive comparisons do not identify a causal effect, prove that
all rushing/winning association is reverse causality, or prove that passing
is necessary to establish a lead. EPA averages also reflect different situations.

Suggested narration:

Last regular season, the team with more rushing yards won 69.5 percent of the
time. But that doesn't prove running is better than passing. Teams ran on
47.5 percent of plays when leading, versus 33.7 percent when trailing.
That's the reverse-causality problem: having the lead can help produce those
rushing totals. Meanwhile, dropbacks averaged plus 0.043 expected points
added, compared with minus 0.046 for designed runs. So rushing yards can
tell you who won without telling you what caused the win.
