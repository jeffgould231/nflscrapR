library(nflscrapR)
library(tidyverse)

game_ids <- nflscrapR::scrape_game_ids(2019, type = "reg", weeks = c(17))
#billa_wk13 <- nflscrapR::scrape_game_play_by_play(2019112801, type = "reg", season = 2019)

games_2019 <- read_csv("~/Desktop/nflscrapR/data/games2019.csv")
games_2019 <- bind_rows(games_2019, game_ids %>% mutate(game_id = as.numeric(as.character(game_id)))) %>%
  filter(state_of_game == "POST") %>%
  distinct()
write_csv(games_2019, "~/Desktop/nflscrapR/data/games2019.csv")

week12_2019 <- nflscrapR::scrape_season_play_by_play(type = "reg", season = 2019, weeks = c(17))
#mnf <- nflscrapR::scrape_game_play_by_play(game_id = 2019112500, type = "reg", season = 2019)
write_csv(week12_2019, "~/Desktop/nflscrapR/data/new_data.csv")


new_data <- read_csv("~/Desktop/nflscrapR/data/new_data.csv") %>%
  mutate(fumble_recovery_2_yards = as.numeric(fumble_recovery_2_yards))
#bills_week12 <- nflscrapR::scrape_game_play_by_play(game_id = 2019112401, type = "reg", season = 2019)

pbp_2019 <- read_csv("~/Desktop/nflscrapR/data/reg_pbp_2019.csv", guess_max = 26644)
pbp_2019 <- bind_rows(pbp_2019, new_data) %>% distinct()
write_csv(pbp_2019, "~/Desktop/nflscrapR/data/reg_pbp_2019.csv")

data_all <- readRDS("~/Desktop/nflscrapR/data/pbp_all.rds")
year = 2019

  pbp <- read_csv(paste0("~/Desktop/nflscrapR/data/reg_pbp_",year,".csv"),
                  guess_max = 26644) %>%
    mutate(blocked_player_id = as.character(blocked_player_id),
           blocked_player_name = as.character(blocked_player_name),
           fumble_recovery_2_player_id = as.character(fumble_recovery_2_player_id)) %>% 
    mutate(
      pass = if_else(str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
      rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
      success = ifelse(epa>0, 1 , 0),
      passer_player_name = ifelse(play_type == "no_play" & pass == 1, 
                                  str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((pass)|(sack)|(scramble)))"),
                                  passer_player_name),
      receiver_player_name = ifelse(play_type == "no_play" & str_detect(desc, "pass"), 
                                    str_extract(desc, "(?<=to\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"),
                                    receiver_player_name),
      rusher_player_name = ifelse(play_type == "no_play" & rush == 1, 
                                  str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((left end)|(left tackle)|(left guard)|		      (up the middle)|(right guard)|(right tackle)|(right end)))"),
                                  rusher_player_name),
      name = ifelse(!is.na(passer_player_name), passer_player_name, rusher_player_name),
      yards_gained=ifelse(play_type=="no_play",NA,yards_gained),
      play=1
    ) 
  games <- read_csv(paste0("~/Desktop/nflscrapR/data/games",year,".csv"))
  pbp <- pbp %>% inner_join(games %>% distinct(game_id, week, season, home_score, away_score)) %>% 
    select(-fumble_recovery_2_yards)
  
  # if(year %in% 2016:2019){
  #   pbp <- pbp %>% select(-fumble_recovery_2_yards)
  # }
  saveRDS(pbp, file = paste0("~/Desktop/nflscrapR/data/reg_pbp_", year,".rds"))
  data_all <- bind_rows(data_all, pbp)
  rm(games)

View(pbp %>% distinct())
saveRDS(data_all %>% distinct(), "~/Desktop/nflscrapR/data/pbp_all.rds")



##############################
reg_current <- readRDS("~/Desktop/nflscrapR/data/reg_pbp_2019.rds")


reg_pbp_2019 <- nflscrapR::scrape_season_play_by_play(type = "reg", season = 2019, weeks = c(1,2,3,4,5,6))
#week1_games <- nflscrapR::season_games(2019)
jets_v_pats <- nflscrapR::scrape_game_play_by_play(game_id = 2019102100, type = "reg", season = 2019)

week8_clean <- week8_2019 %>% 
  mutate(play_id = as.numeric(play_id),
                               game_id = as.numeric(game_id),
                               down = as.numeric(down)) %>%
  left_join(game_ids %>% mutate(game_id = as.numeric(game_id)))

reg_2019 <- left_join(reg_pbp_2019, game_ids)


reg_2019 <- bind_rows(reg_current, week8_clean)


reg_clean <- reg_pbp_2019 %>%
  inner_join(game_ids %>% 
               distinct(game_id, week, season, home_score, away_score)) %>% 
  select(-fumble_recovery_2_yards) %>%
  mutate(play_id = as.numeric(play_id),
         game_id = as.numeric(game_id),
         down = as.numeric(down))
reg_clean2 <- bind_rows(reg_clean, jets)
year = 2019
week = 5
saveRDS(week1_clean, file = paste0("~/Desktop/nflscrapR/data/reg_pbp_", year, "_week", week, ".rds"))

game_ids <- nflscrapR::scrape_game_ids(2019, type = "reg", weeks = 2)
week2_2019 <- nflscrapR::scrape_season_play_by_play(type = "reg", season = 2019, weeks = 2)
week2_clean <- week2_2019 %>%
  inner_join(game_ids %>% 
               distinct(game_id, week, season, home_score, away_score)) %>% 
  select(-fumble_recovery_2_yards)
year = 2019
week = 2
saveRDS(week1_clean, file = paste0("~/Desktop/nflscrapR/data/reg_pbp_", year, "week", week, ".rds"))
week_1 <- readRDS("~/Desktop/nflscrapR/reg_pbp_2019week3.rds")

pbp_2019 <- readRDS("~/Desktop/nflscrapR/data/reg_pbp_2019.rds") %>%
  bind_rows(week1_clean) %>%
  mutate(play_id = as.numeric(play_id),
         game_id = as.numeric(game_id),
         down = as.numeric(down)) %>%
  distinct(.keep_all = TRUE)

reg_pbp_2019week1 <- reg_pbp_2019week1 %>%
  mutate(play_id = as.numeric(play_id),
         game_id = as.numeric(game_id),
         down = as.numeric(down),
         time = lubridate::ms(time))
reg_pbp_2019week2 <- reg_pbp_2019week2 %>%
  mutate(play_id = as.numeric(play_id),
         game_id = as.numeric(game_id),
         down = as.numeric(down),
         time = lubridate::ms(time))
reg_pbp_2019week3 <- reg_pbp_2019week3 %>%
  mutate(play_id = as.numeric(play_id),
         game_id = as.numeric(game_id),
         down = as.numeric(down),
         time = lubridate::ms(time))
reg_pbp_2019_week4 <- reg_pbp_2019_week4 %>%
  mutate(play_id = as.numeric(play_id),
         game_id = as.numeric(game_id),
         down = as.numeric(down),
         time = lubridate::ms(time))

reg_2019 <- reg_2019 %>%
  mutate(play_id = as.numeric(play_id),
         game_id = as.numeric(game_id),
         down = as.numeric(down))
pbp_2019 <- bind_rows(reg_pbp_2019week1, reg_pbp_2019week2, reg_pbp_2019week3, reg_pbp_2019_week4)

write_csv(reg_2019, path = "~/Desktop/nflscrapR/data/reg_pbp_2019.csv")
saveRDS(reg_2019, file = "~/Desktop/nflscrapR/data/reg_pbp_2019.rds")
