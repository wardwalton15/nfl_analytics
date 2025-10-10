library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)
nflreadr::.clear_cache()
pbp <- load_pbp(2025)

#explosive plays vs success rate
pbp <- pbp %>% filter(!is.na(epa),
                        !is.na(posteam),
                      play_type %in% c("pass", "run")) %>%
    mutate(success = ifelse(epa > 0, 1, 0),
           explosive = ifelse((epa >= 2), 1, 0)) %>%
    group_by(posteam) %>%
    summarise(success_rate = mean(success),
              explosive_rate = mean(explosive),
              plays = n()) 

view(pbp)

#plot explosive rate vs success rate
pbp %>%
  ggplot(aes(x = explosive_rate, y = success_rate, label = posteam)) +
  geom_nfl_logos(aes(team_abbr = posteam), width = .1) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Explosive Play Rate vs Success Rate",
       x = "Explosive Play Rate (EPA >= 2)",
       y = "Success Rate (EPA > 0)",
       caption = "Data: nflfastR | Plot: Ward Walton") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 15),
    plot.caption = element_text(hjust = 0.5, size = 12)
  )
