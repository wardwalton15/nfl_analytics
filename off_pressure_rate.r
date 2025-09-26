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
        values = c("Pass Attempts" = "#9beb83", "Times Pressured" = "#e9ae40", "Sacked" = "#dd5959")
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


#epa/play on 1st and 2nd down vs 3rd and 4th down
epa <- pbp %>%
    filter(!is.na(posteam) & (pass + rush) == 1 & between(wp, .05, .95)) %>%
    group_by(posteam, down) %>% 
    summarise(epa = mean(epa, na.rm = TRUE),
              plays = n()) %>%
    pivot_wider(names_from = down, values_from = c(epa, plays), names_prefix = "down_") %>%
    mutate(epa_1_2 = (epa_down_1 + epa_down_2) / (plays_down_1 + plays_down_2),
           epa_3_4 = (epa_down_3 + epa_down_4) / (plays_down_3 + plays_down_4)) %>%
    select(posteam, epa_1_2, epa_3_4)

view(epa)
pbp2025 <- load_pbp(2025)
# team 2nd and 10 pass rate
early <- pbp2024 %>% 
    filter(!is.na(posteam) & (pass + rush) == 1 & down <= 2) %>%
    group_by(posteam) %>%
    summarise(pass = sum(pass, na.rm = TRUE),
              plays = n(),
              rate = pass/plays) 

#4th down go for it rate by team
fourth <- pbp2024 %>%
    filter(!is.na(posteam) & down == 4 & ydstogo <= 7) %>%
    group_by(posteam) %>%
    summarise(plays = n(),
              go = sum(fourth_down_converted == 1 | fourth_down_failed == 1, na.rm = TRUE),
              pct = go / plays)
view(fourth)

#plot early down pass rate vs 4th down go rate

final <- sec %>%
    left_join(fourth, by = "posteam") %>%
    left_join(colors, by = c("posteam" = "team_abbr"))

final %>%
    ggplot(aes(x = pct, y = rate)) + 
    geom_nfl_logos(aes(team_abbr = posteam), width = 0.05) +
    labs(
        title = "2024 Early Down Pass Rate vs 4th Down Go Rate",
        subtitle = "Did more aggresive 4th down teams run more on 2nd and 10 last season?",
        x = "4th Down Go Rate",
        y = "2nd and 10 Pass Rate",
        caption = "Data: @nflfastR | Plot: Ward Walton"
    ) + theme_minimal()

#which qbs throw past the sticks the most?
pbp %>%
 filter(!is.na(passer_player_id) & pass == 1 & !is.na(ydstogo)) %>% 
 group_by(passer_player_id) %>%
 summarise(team = max(posteam),
 name = max(passer_player_name),
 plays = n(),
              passes_past_stick = sum(air_yards >= ydstogo, na.rm = TRUE),
              pct = passes_past_stick / plays) %>%
    filter(plays >= 100) %>%
    ungroup() %>%
    arrange(desc(pct)) %>%
    left_join(colors, by = c("team" = "team_abbr")) %>%
    slice_head(n = 10) %>%
    ggplot(aes(x = reorder(name, pct), y = pct)) +
    geom_bar(aes(fill = team_color), stat = "identity") +
    scale_fill_identity() +
    coord_flip() +
    labs(
        title = "Top 10 QBs Who Throw Past the Sticks the Most",
        subtitle = "Minimum 100 pass attempts")

#distribution of epa on passing vs rushing plays (box plot)
pbp %>%
    filter(!is.na(posteam) & (pass + rush) == 1) %>%
    mutate(play_type = factor(pass, labels = c("Run", "Pass"))) %>%
    ggplot(aes(x = play_type, y = epa, fill = play_type)) +
    geom_violin(outlier.shape = NA, alpha = 0.7) +
    coord_cartesian(ylim = c(-3, 5)) +
    labs(
        title = "Distribution of EPA on Passing vs Rushing Plays",
        subtitle = "Passing plays have a higher median EPA but also a wider range of outcomes",
        x = "Play Type",
        y = "EPA",
        caption = "Data: @nflfastR | Plot: Ward Walton"
    ) + theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        plot.caption = element_text(hjust = 0.5, size = 15),
        legend.position = "none"
    )

# plotting the % of run plays that are over 1 EPA vs the % of pass plays that are over 1 EPA by team
run_pass_epa <- pbp %>%
    filter(!is.na(posteam) & (pass + rush) == 1) %>%
    group_by(posteam, pass) %>%
    summarise(plays = n(),
              big_plays = sum(epa > 1, na.rm = TRUE),
              pct = big_plays / plays) %>%
    pivot_wider(names_from = pass, values_from = c(plays, big_plays, pct), names_prefix = "pass_") %>%
    rename(run_plays = plays_pass_0,
           run_big_plays = big_plays_pass_0,
           run_pct = pct_pass_0,
           pass_plays = plays_pass_1,
           pass_big_plays = big_plays_pass_1,
           pass_pct = pct_pass_1) %>%
    left_join(colors, by = c("posteam" = "team_abbr"))

view(run_pass_epa)

run_pass_epa %>%
    ggplot(aes(x = run_pct, y = pass_pct)) +
    geom_nfl_logos(aes(team_abbr = posteam), width = 0.09) +
    labs(
        title = "Explosive Play Rate on Run vs Pass Plays",
        subtitle = "Explosive plays = plays where EPA > 1",
        x = "Explosive Run Play Rate",
        y = "Explosive Pass Play Rate",
        caption = "Data: @nflfastR | Plot: Ward Walton"
    ) + theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, size = 35, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 25),
        plot.caption = element_text(hjust = 0.5, size = 15)
    )
