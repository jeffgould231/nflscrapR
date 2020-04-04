library(tidyverse)


pbp_2019 <- readRDS(("~/Desktop/nflscrapR/data/reg_pbp_2019.rds")) %>%
  filter(complete_pass == 1|incomplete_pass == 1 & air_yards >= -5) %>%
  filter(!is.na(air_yards), !is.na(pass_location)) %>%
  select(complete_pass, desc, air_yards, pass_location, yards_gained, touchdown, 
         receiver_player_name, receiver_player_id, week) %>% 
  mutate(air_is_zero = ifelse(air_yards == 0, 1, 0))
  

passes <- readRDS("~/Desktop/nflscrapR/data/pbp_all.rds") %>%
  filter((complete_pass == 1|incomplete_pass == 1) & air_yards > -10 & !is.na(receiver_player_name)) %>%
  filter(!is.na(air_yards), season %in% c(2014:2018)) %>%
  select(complete_pass, desc, air_yards, pass_location) %>% 
  mutate(air_is_zero = ifelse(air_yards == 0, 1, 0))

#passes <- passes %>% mutate(air_is_zero = ifelse(air_yards == 0, 1, 0))

gam_y <- gam(complete_pass ~ s(air_yards) + air_is_zero + factor(pass_location), data = passes, method = "REML")
passes$r <- gam_y$residuals

save(gam_y, file = "~/Desktop/nflscrapR/Code/cpoe_model.rda")

expected_comp_2019 <- predict(gam_y, pbp_2019)
pbp_2019$exp_comp = expected_comp_2019

receiver_plays <- pbp_2019 %>%
  filter(!is.na(receiver_player_id), !is.na(receiver_player_name)) %>%
  mutate(expCompAirYards = air_yards * exp_comp) %>%
  group_by(receiver_player_id, receiver_player_name) %>%
  summarise(targets = n(), 
            Receptions = sum(complete_pass, na.rm = T),
            AirYards = sum(air_yards, na.rm = T), 
            ReceivingYards = sum(yards_gained, na.rm = T), 
            completeAirYards = sum(air_yards * complete_pass, na.rm = T),
            Touchdowns = sum(touchdown, na.rm = T),
            expReceptions = sum(exp_comp),
            expCompAirYards = sum(expCompAirYards)) %>%
  mutate(PPR_points = Receptions + 0.1 * ReceivingYards + 6 * Touchdowns) %>%
  arrange(desc(PPR_points))

receiver_plays_6_weeks <- pbp_2019 %>%
  filter(!is.na(receiver_player_id), !is.na(receiver_player_name)) %>%
  filter(week >= 4) %>%
  mutate(expCompAirYards = air_yards * exp_comp) %>%
  group_by(receiver_player_id, receiver_player_name) %>%
  summarise(targets = n(), 
            Receptions = sum(complete_pass, na.rm = T),
            AirYards = sum(air_yards, na.rm = T), 
            ReceivingYards = sum(yards_gained, na.rm = T), 
            completeAirYards = sum(air_yards * complete_pass, na.rm = T),
            Touchdowns = sum(touchdown, na.rm = T),
            expReceptions = sum(exp_comp),
            expCompAirYards = sum(expCompAirYards)) %>%
  mutate(PPR_points = Receptions + 0.1 * ReceivingYards + 6 * Touchdowns) %>%
  arrange(desc(PPR_points))


write_csv(receiver_plays, "~/Desktop/nflscrapR/Output/AYOCseason.csv")
write_csv(receiver_plays_6_weeks, "~/Desktop/nflscrapR/Output/AYOC_6weeks.csv")

