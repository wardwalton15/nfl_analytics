library(nflreadr)
library(tidyverse)
library(nflplotR)

season <- 2026L
through_week <- 2L

weekly <- load_pfr_advstats(
    season, stat_type = "pass", summary_level = "week"
) %>%
    filter(game_type == "REG", between(week, 1L, through_week))

if (nrow(weekly) == 0L) {
    stop("No PFR passing data available for the requested weeks.")
}
stopifnot(!anyNA(weekly$times_pressured), !anyNA(weekly$times_sacked))

coverage <- weekly %>%
    distinct(week, game_id) %>%
    count(week, name = "games_available")
print(coverage)

scheduled_games <- load_schedules(season) %>%
    filter(game_type == "REG", between(week, 1L, through_week)) %>%
    select(game_id)
missing_games <- scheduled_games %>%
    anti_join(distinct(weekly, game_id), by = "game_id")
if (nrow(missing_games) > 0L) {
    warning("PFR coverage is incomplete: ", nrow(missing_games),
            " scheduled games are missing. Plot includes available games only.")
}

# Each quarterback's opponent is the defense generating the pressures.
pressure <- weekly %>%
    group_by(team_abbr = opponent) %>%
    summarise(Prss = sum(times_pressured), Sk = sum(times_sacked),
              .groups = "drop")

# Use the same games for attempts and pressures when source updates lag.
attempts_by_game <- load_pbp(season) %>%
    filter(season_type == "REG", between(week, 1L, through_week),
           play_type == "pass", !is.na(defteam)) %>%
    semi_join(distinct(weekly, game_id), by = "game_id") %>%
    group_by(game_id, team_abbr = defteam) %>%
    summarise(Att = sum(pass_attempt == 1 & sack == 0, na.rm = TRUE),
              .groups = "drop")

missing_attempts <- weekly %>%
    distinct(game_id, team_abbr = opponent) %>%
    anti_join(attempts_by_game, by = c("game_id", "team_abbr"))
if (nrow(missing_attempts) > 0L) {
    stop("Play-by-play is missing for some PFR team games; try again after updates.")
}

attempts <- attempts_by_game %>%
    group_by(team_abbr) %>%
    summarise(Att = sum(Att), .groups = "drop")

final <- pressure %>%
    left_join(attempts, by = "team_abbr") %>%
    left_join(load_teams(), by = "team_abbr") %>%
    mutate(Tm = team_name,
           # Preserve the original attempts-based rate (not per dropback).
           prss_rate = if_else(Att > 0, Prss / Att, NA_real_),
           Prss_to_sack_rate = if_else(Prss > 0, Sk / Prss, NA_real_),
           team = reorder(Tm, prss_rate))
if (interactive()) View(final)

final %>%
    arrange(prss_rate) %>%
    ggplot(aes(x = team, y = Att)) +
    geom_bar(aes(y = Att, fill = "Opponent Pass Attempts"), stat = "identity", alpha = .5) +
    geom_bar(aes(y = Prss, fill = "Pressures"), stat = "identity", alpha = .7) +
    geom_bar(aes(y = Sk, fill = "Sacks"), stat = "identity", alpha = .8) +
    geom_nfl_logos(aes(y = Att, team_abbr = team_abbr, width = 0.06)) +
    scale_fill_manual(
        name = "",
        values = c("Opponent Pass Attempts" = "#9beb83", "Pressures" = "#e9ae40", "Sacks" = "#dd5959")
    ) +
    coord_flip() +
    labs(
        title = paste(season, "NFL Defensive Pressure and Sack Rates"),
        subtitle = paste0("Weeks 1–", through_week,
                          " · Available games: ", n_distinct(weekly$game_id),
                          "/", nrow(scheduled_games)),
        y = "Total Pass Attempts",
        caption = "Data: PFR and nflverse via nflreadr | Plot: Ward Walton"
    ) + theme_minimal() +
    theme(legend.position = "top",
          plot.title = element_text(face = "bold", size = 30, hjust = 0.5),
          plot.subtitle = element_text(size = 20, hjust = 0.5),
          plot.caption = element_text(size = 15),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
