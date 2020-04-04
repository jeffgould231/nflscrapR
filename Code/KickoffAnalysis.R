library(nflscrapR)
library(tidyverse)
library(na.tools)
library(ggimage)

pbp_2018 <- read_rds("~/Desktop/nflscrapR/reg_pbp_2018.rds")
pbp_all <- read_rds("~/Desktop/nflscrapR/pbp_all.rds")

pbp_2016_2018 <- pbp_all %>%
  filter(season %in% 2016:2018)

rm(pbp_all)

kickoffs2018 <- pbp_2018 %>%
  filter(play_type == "kickoff")

kickoffs2016_2018 <- pbp_2016_2018 %>%
  filter(play_type == "kickoff")

endzoneReturns2018 <- kickoffs2018 %>%
  filter(touchback == 0, kickoff_in_endzone == 1)

sum(kickoffs2018$touchback)
sum(str_detect(endzoneReturns2018$desc, "PENALTY|Penalty|penalty"))

endzoneReturns2018 <- endzoneReturns2018 %>%
  mutate(catch_extract = str_extract(desc, "to [:upper:]{2,3} -[:digit:]|to [:upper:]{2,3} [:digit:]")) %>%
  mutate(catch_yard_line = as.numeric(str_extract(catch_extract, "-[:digit:]+"))) %>%
  mutate(catch_yard_line = ifelse(is.na(catch_yard_line), 0, catch_yard_line),
         return_end = return_yards + catch_yard_line) %>%
  map(~.x) %>%
  purrr::discard(~all(is.na(.x))) %>%
  map_df(~.x)

sum(endzoneReturns2018$return_end >= 25)
140 / 315

mean(endzoneReturns2018$return_end)

Roberts2018endzone <- endzoneReturns2018 %>%
  filter(kickoff_returner_player_name == "A.Roberts")

mean(Roberts2018endzone$return_end)
mean(Roberts2018endzone$epa)
sum(Roberts2018endzone$return_end >= 25)


Roberts2018all <- kickoffs2018 %>%
  filter(kickoff_returner_player_name == "A.Roberts") %>%
  mutate(catch_extract = str_extract(desc, "to [:upper:]{2,3} -[:digit:]|to [:upper:]{2,3} [:digit:]")) %>%
  mutate(catch_yard_line = as.numeric(str_extract(catch_extract, "-[:digit:]+"))) %>%
  mutate(catch_yard_line = ifelse(is.na(catch_yard_line), as.numeric(str_extract(catch_extract, "[:digit:]+")), catch_yard_line),
         return_end = return_yards + catch_yard_line) %>%
  map(~.x) %>%
  purrr::discard(~all(is.na(.x))) %>%
  map_df(~.x) %>%
  mutate(inside5 = ifelse(catch_yard_line %in% 1:5, 1, 0))

sum(Roberts2018all$inside5)

Roberts2018inside5 <- filter(Roberts2018all, inside5 == 1)

inside5All <- kickoffs2018 %>%
  mutate(catch_extract = str_extract(desc, "to [:upper:]{2,3} -[:digit:]|to [:upper:]{2,3} [:digit:]")) %>%
  mutate(catch_yard_line = as.numeric(str_extract(catch_extract, "-[:digit:]+"))) %>%
  mutate(catch_yard_line = ifelse(is.na(catch_yard_line), as.numeric(str_extract(catch_extract, "[:digit:]+")), catch_yard_line),
         return_end = return_yards + catch_yard_line) %>%
  map(~.x) %>%
  purrr::discard(~all(is.na(.x))) %>%
  map_df(~.x) %>%
  mutate(inside5 = ifelse(catch_yard_line %in% 1:5, 1, 0)) %>%
  filter(inside5 == 1)

mean(inside5All$return_end)  
median(inside5All$return_end)
mean(inside5All$epa)
median(inside5All$epa)

mean(Roberts2018inside5$return_end)
median(Roberts2018inside5$return_end)
mean(Roberts2018inside5$epa)
median(Roberts2018inside5$epa)


endzoneReturns_2016_2017 <- pbp_all %>%
  filter(season %in% 2016:2017, play_type == "kickoff", touchback == 0, kickoff_in_endzone == 1) %>%
  mutate(catch_extract = str_extract(desc, "to [:upper:]{2,3} -[:digit:]|to [:upper:]{2,3} [:digit:]")) %>%
  mutate(catch_yard_line = as.numeric(str_extract(catch_extract, "-[:digit:]+"))) %>%
  mutate(catch_yard_line = ifelse(is.na(catch_yard_line), 0, catch_yard_line),
         return_end = return_yards + catch_yard_line) %>%
  map(~.x) %>%
  purrr::discard(~all(is.na(.x))) %>%
  map_df(~.x)

Roberts2016endzone <- endzoneReturns_2016_2017 %>%
  filter(kickoff_returner_player_name == "A.Roberts")

sum(endzoneReturns_2016_2017$return_end >= 25) / length(endzoneReturns_2016_2017$return_end)
mean(endzoneReturns_2016_2017$return_end)
mean(endzoneReturns_2016_2017$catch_yard_line)

sum(Roberts2016endzone$return_end >= 25) / length(Roberts2016endzone$return_end)
mean(Roberts2016endzone$return_end)
mean(Roberts2016endzone$catch_yard_line)

sum(kickoffs2016_2018$touchback)
sum(str_detect(endzoneReturns_2016_2017$desc, "PENALTY|Penalty|penalty"))


