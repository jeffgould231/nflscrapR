library(parallel)
library(nflscrapR)
library(tidyverse)
library(na.tools)
library(ggimage)

# Initial code to download all data
season_scrape <- function(year){
   reg_pbp <- read_csv(url(paste0("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_",year,".csv")))
   write_csv(reg_pbp, path = paste0("~/Desktop/nflscrapR/data/reg_pbp_",year,".csv"))
 }
reg_pbp_2019 <- season_scrape(2019)
#seasons <- c(2009:2018)
# 
# for (year in 2009:2018) {
#   season_scrape(year = year)
# }
games_2019 <- nflscrapR::scrape_game_ids(season = 2019, type = "reg", weeks = c(1:11))
pbp_2019 <- nflscrapR::scrape_season_play_by_play(2019, type = "reg", weeks = c(1:11))
write_csv(pbp_2019, "~/Desktop/nflscrapr/data/reg_pbp_2019.csv")
write_csv(games_2019, "~/Desktop/nflscrapR/data/games2019.csv")

 game_scrape <- function(yr){
   games <- read_csv(url(paste0("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_", yr, ".csv")))
   write_csv(games, paste0("~/Desktop/nflscrapR/data/games",yr,".csv"))
 }
# 
# sapply(2009:2018, game_scrape)
games <- game_scrape(2019)
data_all = data.frame()
year = 2019
for (year in 2009:2019){
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
  #rm(pbp)
}


saveRDS(data_all, "~/Desktop/nflscrapR/data/pbp_all.rds")


datalist = list()
for (yr in 2009:2018) {
  if (yr<=2017) {
    roster <- read_csv(url(paste0("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/legacy_data/team_rosters/team_", yr, "_rosters.csv")))
    roster <- roster %>% mutate(
      season = Season,
      full_player_name = Player,
      abbr_player_name = name,
      position = Pos,
      team = Team,
      gsis_id = GSIS_ID
    ) %>%
      select(season,full_player_name,abbr_player_name,position,team,gsis_id)
  }
  else {
    roster <- read_csv(url(paste0("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/roster_data/regular_season/reg_roster_", yr, ".csv")))
    roster <- roster %>% select(-season_type)
  }
  
  datalist[[yr]] <- roster # add it to your list
}

rosters_all <- dplyr::bind_rows(datalist)

#fix the team name problems
rosters_all <- rosters_all %>% 
  mutate_at(vars(team), funs(case_when(
    . %in% "JAX" ~ "JAC",
    . %in% "STL" ~ "LA",
    . %in% "SD" ~ "LAC",
    TRUE ~ .
  ))) 

#save raw dataset
saveRDS(rosters_all, file="~/Desktop/nflscrapR/data/Rosters.rds")
