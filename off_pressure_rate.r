library(nflreadr)
library(nflfastR)
library(tidyverse)
library(nflplotR)

colors <- nflfastR::teams_colors_logos

pfr_pass <- load_pfr_advstats(2025, stat_type = "pass", summary_level = "season")
view(pfr_pass)

pbp <- load_pbp(2025)
sk <- pbp %>%
    filter(!is.na(posteam) & pass == 1) %>%
    group_by(posteam) %>% 
    summarise(sk = sum(sack, na.rm = TRUE),
              att = n())
view(sk)

team_pass <- pfr_pass %>% 
    group_by(team) %>%
    summarise(
                prss = sum(times_pressured))

view(team_pass)


final <- team_pass %>%
    left_join(sk, by = c("team" = "posteam"))
view(final)

#horizontal bar chart of with the length of each team's bar representing their number of pass attmpets and the bar divided into segments representing the proportion of sacka and pressures.
final <- final %>%
    mutate(prss_per_att = prss / att,
           sk_per_att = sk / att)   

final <- final %>%
    left_join(colors, by = c("team" = "team_abbr"))

final <- final %>%
    arrange((prss_per_att)) %>% 
    mutate(team = factor(team, levels = unique(team)))
view(final)

final %>%
    ggplot(aes(x = team, y = att)) +
    geom_bar(aes(y = att , fill = "Pass Attempts"), stat = "identity", alpha = .5) +
    geom_bar(aes(y = prss, fill = "Times Pressured"), stat = "identity", alpha = .7) +
    geom_bar(aes(y = sk, fill = "Sacked"), stat = "identity", alpha = .8) +
    geom_nfl_logos(aes(y = att, team_abbr = team, width = 0.06)) +
    scale_fill_manual(
        name = "",
        values = c("Pass Attempts" = "#9beb83", "Times Pressured" = "orange", "Sacked" = "red")
    ) +
    coord_flip() +
    labs(
        title = "2025 NFL Team Pressures and Sacks Allowed",
        subtitle = "How often is each team pressured and sacked on passing plays?",
        y = "Total Pass Attempts",
        caption = "Data: profootballreference & nflreadr | Plot: Ward Walton"
    ) + theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        plot.caption = element_text(hjust = 0.5, size = 15),
        legend.position = "top"
    )
