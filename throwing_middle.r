library(nflreadr)
library(nflfastR)
library(tidyverse)
library(nflplotR)
nflreadr::.clear_cache()
pbp <- load_pbp(2025)

pass_direction <- pbp %>% filter(pass_attempt == 1,
                               !is.na(pass_location),
                               !is.na(epa)) %>%
group_by(pass_location) %>%
summarise(n = n(),
        epa = mean(epa),
        comp = sum(complete_pass),
        comp_pct = comp/n,
         .groups = "drop") %>%
mutate(pct = n / sum(n))
view(pass_direction)

#which qbs throw to the middle the most?
throwing_summary <- pbp %>% filter(pass_attempt == 1,
                                    air_yards >= 5,
                                  pass_location == "middle",
                                  !is.na(epa),
                                  !is.na(passer_player_name)) %>%
  group_by(passer_player_name) %>%
  summarise(n = n(),
            epa = mean(epa),
            comp = sum(complete_pass),
            comp_pct = comp/n,
            .groups = "drop") %>%
    arrange(desc(n))

  view(throwing_summary)

recieving_summary <- pbp %>% filter(pass_attempt == 1,
                                  pass_location == "middle",
                                  !is.na(epa),
                                  !is.na(receiver_player_name)) %>%
  group_by(receiver_player_name) %>%
  summarise(n = n(),
            epa = mean(epa),
            comp = sum(complete_pass),
            comp_pct = comp/n,
            .groups = "drop") %>%
    arrange(desc(n))
    view(recieving_summary)

### air yards per attempt by qb vs pct of attempts to middle
qb_summary <- pbp %>% filter(pass_attempt == 1,
                             !is.na(epa),
                             !is.na(passer_player_name)) %>%
  group_by(passer_player_name, passer_player_id) %>%
  summarise(n = n(),
            air_yards = mean(air_yards, na.rm=TRUE),
            epa = mean(epa),
            comp = sum(complete_pass),
            comp_pct = comp/n,
            mid = sum(pass_location == "middle", na.rm=TRUE),
            mid_pct = mid/n,
            .groups = "drop") %>%
            filter(n >= 50) %>%
  arrange(desc(n))
view(qb_summary)

library(ggrepel)

ggplot(qb_summary, aes(x=air_yards, y=mid_pct)) +
  geom_nfl_headshots(aes(player_gsis = passer_player_id), height = .05)  +
  labs(title="NFL Quarterbacks: Air Yards per Attempt vs. Percentage of Attempts to Middle of Field",
       subtitle="2025 Regular Season (min. 50 attempts)",
       x="Average Air Yards per Attempt",
       y="Percentage of Attempts to Middle of Field",
       caption="Data: @nflfastR | Plot: Ward Walton") +
  theme_minimal() +
  theme(legend.position = "bottom")
