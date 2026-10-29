library(nflseedR)
library(nflreadr)
library(nflplotR)
library(tidyverse)
library(rvest)
library(gt)
nflreadr::.clear_cache()
season_id <- 2026
n_sims <- 50000
set.seed(15)

games <- load_schedules(season_id) %>% filter(game_type == "REG")
next_week <- games %>% filter(is.na(result)) %>% pull(week) %>% min()
out <- sprintf("outputs/playoff_sim_%s_entering_week_%s", season_id, next_week)
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# ---- nfelo ratings ----
# the power ratings table is rendered client side; the data ships as escaped JSON in the page
page <- read_html("https://www.nfeloapp.com/nfl-power-ratings/") %>% as.character()
pattern <- sprintf('\\\\"team\\\\":\\\\"([A-Z]+)\\\\",\\\\"season\\\\":%s,\\\\"nfelo\\\\":([0-9.]+)', season_id)
ratings <- str_match_all(page, pattern)[[1]] %>%
  as_tibble(.name_repair = ~ c("match", "team", "nfelo")) %>%
  distinct(team, .keep_all = TRUE) %>%
  transmute(team = recode(team, "OAK" = "LV", "LAR" = "LA"), nfelo = as.numeric(nfelo))

stopifnot(nrow(ratings) == 32, setequal(ratings$team, unique(games$home_team)))
write_csv(ratings, file.path(out, "nfelo_ratings.csv"))

# ---- simulate ----
# default nflseedR game model: elo diff (+20 home, rest adj) -> margin ~ N(diff/25, 13), elo updates within each sim
future::plan(future::multisession, workers = parallel::detectCores() - 1)
sims <- nfl_simulations(
  games = games,
  elo = setNames(ratings$nfelo, ratings$team),
  simulations = n_sims,
  chunks = 25,
  verbosity = "MIN"
)
future::plan(future::sequential)

playoff_odds <- sims$overall %>%
  left_join(ratings, by = "team") %>%
  select(conf, division, team, nfelo, wins, playoff, div1, seed1, won_conf, won_sb) %>%
  arrange(conf, desc(playoff))
write_csv(playoff_odds, file.path(out, "playoff_odds.csv"))

# ---- playoff leverage ----
# for every remaining game: each team's playoff odds if they win vs if they lose (ties dropped)
made_playoffs <- sims$standings %>% transmute(sim, team, made = exit != "REG")

remaining <- sims$games %>%
  filter(game_type == "REG", week >= next_week, result != 0) %>%
  transmute(sim, week, away_team, home_team, home_win = result > 0)

swing <- bind_rows(
  remaining %>% mutate(side = "home", team = home_team, won = home_win),
  remaining %>% mutate(side = "away", team = away_team, won = !home_win)
) %>%
  left_join(made_playoffs, by = c("sim", "team")) %>%
  group_by(week, away_team, home_team, side) %>%
  summarise(win_prob = mean(won), p_if_win = mean(made[won]), p_if_loss = mean(made[!won]), .groups = "drop") %>%
  mutate(swing = p_if_win - p_if_loss) %>%
  pivot_wider(names_from = side, values_from = c(win_prob, p_if_win, p_if_loss, swing)) %>%
  left_join(playoff_odds %>% select(team, away_playoff = playoff), by = c("away_team" = "team")) %>%
  left_join(playoff_odds %>% select(team, home_playoff = playoff), by = c("home_team" = "team")) %>%
  mutate(leverage = swing_home + swing_away) %>%
  select(week, away_team, home_team, leverage, swing_away, swing_home, home_win_prob = win_prob_home,
         away_playoff, away_p_if_win = p_if_win_away, away_p_if_loss = p_if_loss_away,
         home_playoff, home_p_if_win = p_if_win_home, home_p_if_loss = p_if_loss_home) %>%
  arrange(week, desc(leverage))
write_csv(swing, file.path(out, "game_leverage_all_weeks.csv"))

swing %>% group_by(week) %>% slice_max(leverage, n = 3) %>% print(n = 40, width = Inf)

# ---- plots ----
bg <- "#fcfcfb"
ink <- "#0b0b0b"
muted <- "#52514e"
blue <- "#2a78d6"
plot_theme <- theme_minimal(base_size = 12) +
  theme(plot.background = element_rect(fill = bg, color = NA),
        panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "#e6e5e0", linewidth = 0.3),
        plot.title = element_text(face = "bold", color = ink),
        plot.subtitle = element_text(color = muted),
        plot.caption = element_text(color = muted, size = 8),
        axis.text = element_text(color = muted),
        strip.text = element_text(face = "bold", color = ink, size = 12))

odds_plot <- playoff_odds %>%
  group_by(conf) %>%
  mutate(rank = rank(-playoff, ties.method = "first")) %>%
  ungroup() %>%
  ggplot(aes(y = reorder(rank, -rank), x = playoff)) +
  geom_col(fill = blue, width = 0.7) +
  geom_nfl_logos(aes(team_abbr = team, x = -0.04), width = 0.045) +
  geom_text(aes(label = scales::percent(playoff, accuracy = 1)), hjust = -0.15, size = 3.2, color = ink) +
  facet_wrap(~conf, scales = "free_y") +
  scale_x_continuous(labels = scales::percent, limits = c(-0.08, 1.1), breaks = seq(0, 1, 0.25)) +
  labs(title = sprintf("%s NFL playoff odds entering week %s", season_id, next_week),
       subtitle = sprintf("%s season simulations seeded with nfelo power ratings", scales::comma(n_sims)),
       caption = "Ratings: nfeloapp.com | Sims: nflseedR", x = NULL, y = NULL) +
  plot_theme +
  theme(axis.text.y = element_blank())
ggsave(file.path(out, "playoff_odds.png"), odds_plot, width = 11, height = 8, dpi = 200, bg = bg)

# ---- week leverage table ----
teams <- load_teams() %>% select(team_abbr, team_nick, team_logo_espn)
logo <- setNames(teams$team_logo_espn, teams$team_abbr)
nick <- setNames(teams$team_nick, teams$team_abbr)
img <- function(team, size = 38) sprintf('<img src="%s" style="height:%spx;vertical-align:middle;">', logo[team], size)

week_games <- swing %>%
  filter(week == next_week) %>%
  slice_max(leverage, n = 10) %>%
  mutate(
    rank = row_number(),
    away_logo = img(away_team), away_name = nick[away_team],
    sep = "at",
    home_logo = img(home_team), home_name = nick[home_team],
    favorite = if_else(home_win_prob >= 0.5, home_team, away_team),
    favorite = sprintf("%s&nbsp;&nbsp;%s", img(favorite, 30), scales::percent(pmax(home_win_prob, 1 - home_win_prob), accuracy = 1)),
    impact = sprintf('<div style="display:flex;align-items:center;gap:12px;"><div style="background:%s;height:22px;width:%.0fpx;"></div><b>%s</b></div>',
                     blue, 180 * leverage / max(leverage), round(100 * leverage))
  ) %>%
  select(rank, away_logo, away_name, sep, home_logo, home_name, favorite, impact)

leverage_table <- week_games %>%
  gt() %>%
  tab_header(title = md("**This week's biggest playoff games**"),
             subtitle = sprintf("Week %s games ranked by how much they move %s playoff odds", next_week, season_id)) %>%
  cols_label(rank = "", away_logo = "Away", away_name = "", sep = "", home_logo = "Home", home_name = "",
             favorite = "Favorite", impact = "Impact") %>%
  fmt_markdown(columns = c(away_logo, home_logo, favorite, impact)) %>%
  cols_align("left") %>%
  cols_align("right", columns = rank) %>%
  cols_align("center", columns = c(sep, away_logo, home_logo)) %>%
  cols_width(rank ~ px(45), away_logo ~ px(60), away_name ~ px(150), sep ~ px(50), home_logo ~ px(60),
             home_name ~ px(150), favorite ~ px(130), impact ~ px(250)) %>%
  tab_style(style = cell_text(color = muted), locations = cells_body(columns = c(rank, sep))) %>%
  tab_style(style = cell_text(color = muted, weight = "bold", size = px(16)), locations = cells_column_labels()) %>%
  tab_source_note(paste("Impact = a team's playoff odds if it wins minus if it loses, summed for both teams (percentage points).",
                        sprintf("Odds from %s season simulations seeded with nfelo ratings.", scales::comma(n_sims)))) %>%
  tab_source_note("Ratings: nfeloapp.com | Sims: nflseedR | Ward Walton") %>%
  opt_table_font(font = list(google_font("Inter"), default_fonts())) %>%
  tab_options(table.background.color = bg, table.font.color = ink, table.font.size = px(22),
              heading.align = "left", heading.title.font.size = px(40), heading.subtitle.font.size = px(20),
              heading.border.bottom.style = "none", data_row.padding = px(10),
              table.border.top.style = "none", column_labels.border.top.color = "#d6d5d0",
              column_labels.border.top.width = px(2), column_labels.border.bottom.color = "#d6d5d0",
              column_labels.border.bottom.width = px(2), table_body.hlines.color = "#e6e5e0",
              table_body.border.bottom.color = "#d6d5d0", table.border.bottom.color = "#b0afa9",
              table.border.bottom.width = px(2), source_notes.font.size = px(13))
gtsave(leverage_table, file.path(out, sprintf("week_%s_leverage.html", next_week)))
gtsave(leverage_table, file.path(out, sprintf("week_%s_leverage.png", next_week)), zoom = 2, expand = 20)
