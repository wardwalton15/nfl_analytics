
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)

pbp <- load_pbp(2025)
#turnovers
#to differential vertical bar chart
turnovers <- pbp %>% filter((interception == 1 | fumble_lost == 1))




team_turnovers <- turnovers %>% group_by(posteam) %>% summarise(tos = sum(interception + fumble_lost),,
                                                                epa_lost = sum(epa, na.rm = TRUE),
                                                                win_prob_lost = sum(wpa, na.rm = TRUE))




team_takeaways <- turnovers %>% group_by(defteam) %>% summarise(takeaways = sum(interception + fumble_lost),
                                                                epa_gained = -sum(epa, na.rm = TRUE),
                                                                win_prob_gained = -sum(wpa, na.rm = TRUE))

view(team_turnovers)
team_to_stats <- load_pbp(2025) %>% filter(!is.na(posteam)) %>%
group_by(posteam) %>% 
summarise() %>%
left_join(team_turnovers, by = c("posteam" = "posteam")) %>%
left_join(team_takeaways, by = c("posteam" = "defteam")) 

view(team_to_stats)
team_to_stats[is.na(team_to_stats)] <- 0

logos <- teams_colors_logos %>% select(team_abbr,team_color)

stats_w_logos <- team_to_stats %>% left_join(logos, by = c("posteam" = "team_abbr")) %>%
mutate(epa = (epa_gained) + (epa_lost),
        win_prob = (win_prob_gained) + (win_prob_lost))

view(stats_w_logos)

stats_w_logos %>%
  ggplot(aes(x = epa, y = reorder(posteam, epa), fill = team_color))+
  geom_col(aes(alpha = .08))+
  scale_fill_identity()+
  geom_nfl_logos(aes(team_abbr = posteam, width = 0.075))+
  labs(
    x = "EPA",
    y = "",
    caption = "Chart: Ward Walton | Data: nflreadR",
    title = "2025 NFL Turnover Impact",
    subtitle = "Total EPA Lost/Gained from Turnovers")+
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        plot.caption = element_text(hjust = 0.5, size = 15),
        legend.position = "",
       axis.text.y = element_blank(),
       axis.ticks.y = element_blank()
    )
