library(nflreadr)
library(nflfastR)
library(tidyverse)
library(nflplotR)

pfr <- read_csv("pfr_def.csv")

view(pfr)
teams <- nflfastR::teams_colors_logos
view(teams)

final <- pfr %>%
    select(Tm, Att, Prss, Sk) %>%
    left_join(teams, by = c("Tm" = "team_name")) %>%
    mutate(team = factor(Tm, levels = unique(Tm))) %>%
    mutate(prss_rate = Prss / Att,
           Prss_to_sack_rate = Sk / Prss) 
view(final)
#remove duplicate rams column
final <- final %>%
    filter(team_abbr != 'LA') 
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
        title = "NFL Defensive Pressure and Sack Rates",
        subtitle = "How often does each team generate pressure?",
        y = "Total Pass Attempts",
        caption = "Data: profootballreference | Plot: Ward Walton"
    ) + theme_minimal() +
    theme(legend.position = "top",
          plot.title = element_text(face = "bold", size = 30, hjust = 0.5),
          plot.subtitle = element_text(size = 20, hjust = 0.5),
          plot.caption = element_text(size = 15),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
