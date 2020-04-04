library(tidyverse)
library(nflscrapR)

pbp_2019 <- readRDS(("~/Desktop/nflscrapR/data/reg_pbp_2019.rds"))

bills_rush_2019 <- pbp_2019 %>%
  filter(play_type == "run", posteam == "BUF", qb_dropback == 0) %>%
  group_by(run_location, run_gap) %>%
  summarise(yards_per_carry = mean(yards_gained, na.rm = T),
            epa_per_carry = mean(epa, na.rm = T),
            plays = n()) %>%
  ungroup() %>%
  filter(!is.na(run_location)) %>%
  mutate(hole = paste(run_location, run_gap),
         hole = str_remove(hole, " NA"))

league_rush_2019 <- pbp_2019 %>%
  filter(play_type == "run", posteam != "BUF", qb_dropback == 0) %>%
  group_by(run_location, run_gap) %>%
  summarise(yards_per_carry = mean(yards_gained, na.rm = T),
            epa_per_carry = mean(epa, na.rm = T),
            plays = n()) %>%
  ungroup() %>%
  filter(!is.na(run_location)) %>%
  mutate(hole = paste(run_location, run_gap),
         hole = str_remove(hole, " NA"))
  
ggplot(bills_rush_2019) +
  geom_col(aes(x = factor(hole), y = epa_per_carry), fill = "#00338d", color = "#c60c30") +
  theme_BUF() +
  scale_x_discrete(limits = c("left end", "left tackle", "left guard", "middle", 
                              "right guard", "right tackle", "right end")) +
  geom_col(data = league_rush_2019, aes(x = factor(hole), y = epa_per_carry), color = "#c60c30", alpha = 0,
           size = 1) +
  labs(title = "Bills EPA Per Carry by Hole", subtitle = "Through Wk. 5", x = "Hole", y = "Yards Per Carry", caption = "data via nflscrapR")
ggsave(filename = "~/Desktop/nflscrapR/Output/EPA_per_carry_by_hole_wk5.jpg", width = 8, height = 5, units = "in")

ggplot(bills_rush_2019) +
  geom_col(aes(x = factor(hole), y = yards_per_carry), fill = "#00338d", color = "#c60c30") +
  theme_BUF() +
  scale_x_discrete(limits = c("left end", "left tackle", "left guard", "middle", 
                              "right guard", "right tackle", "right end")) +
  geom_col(data = league_rush_2019, aes(x = factor(hole), y = yards_per_carry), color = "#c60c30", alpha = 0,
           size = 1) +
  labs(title = "Bills Yards Per Carry by Hole", x = "Hole", y = "Yards Per Carry", caption = "data via nflscrapR")
ggsave(filename = "~/Desktop/nflscrapR/Output/yards_per_carry_by_hole_wk5.jpg", width = 8, height = 5, units = "in")


nflteams
