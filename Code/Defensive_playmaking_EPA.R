library(nflscrapR)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(tidyverse)

#LOAD SEASON DATA
#season_pbp_2018 <- readRDS("~/Desktop/nflscrapR/data/reg_pbp_2018.rds")
season_pbp_2018 <- readRDS(("~/Desktop/nflscrapR/data/pbp_all.rds"))
season_pbp_2018 <- readRDS(("~/Desktop/nflscrapR/data/reg_pbp_2019.rds"))
nfl_logos_df <- read_csv("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")

#CREATE DATAFRAME FOR LEAGUE
defense_pbp_2018 <- season_pbp_2018 %>%
  filter(play_type != "punt" & play_type != "kickoff" & !is.na(play_type)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, lateral_sack_player_id, lateral_sack_player_name, interception_player_id, interception_player_name, lateral_interception_player_id, lateral_interception_player_name, blocked_player_id, blocked_player_name, qb_hit_1_player_id, qb_hit_1_player_name, qb_hit_2_player_id, qb_hit_2_player_name, forced_fumble_player_1_player_id, forced_fumble_player_1_player_name, forced_fumble_player_2_player_id, forced_fumble_player_2_player_name, solo_tackle_1_player_id, solo_tackle_1_player_name, solo_tackle_2_player_id, solo_tackle_2_player_name, assist_tackle_1_player_id, assist_tackle_1_player_name, assist_tackle_2_player_id, assist_tackle_2_player_name, assist_tackle_3_player_id, assist_tackle_3_player_name, assist_tackle_4_player_id, assist_tackle_3_player_name, assist_tackle_4_player_id, assist_tackle_4_player_name, pass_defense_1_player_id, pass_defense_1_player_name, pass_defense_2_player_id, pass_defense_2_player_name, fumble_recovery_1_player_id, fumble_recovery_1_player_name, fumble_recovery_2_player_id, fumble_recovery_2_player_name)

#CREATE SUBDATAFRAMES FOR LEAGUE
interceptions_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(interception_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = interception_player_id, player_name = interception_player_name)

blocked_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(blocked_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = blocked_player_id, player_name = blocked_player_name)

qb_hit_1_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(qb_hit_1_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = qb_hit_1_player_id, player_name = qb_hit_1_player_name)

qb_hit_2_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(qb_hit_2_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = qb_hit_2_player_id, player_name = qb_hit_2_player_name)

forced_fumble_1_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(forced_fumble_player_1_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = forced_fumble_player_1_player_id, player_name = forced_fumble_player_1_player_name)

forced_fumble_2_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(forced_fumble_player_2_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = forced_fumble_player_2_player_id, player_name = forced_fumble_player_2_player_name)

solo_tackle_1_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(solo_tackle_1_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = solo_tackle_1_player_id, player_name = solo_tackle_1_player_name)

solo_tackle_2_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(solo_tackle_2_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = solo_tackle_2_player_id, player_name = solo_tackle_2_player_name)

assist_tackle_1_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(assist_tackle_1_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = assist_tackle_1_player_id, player_name = assist_tackle_1_player_name)

assist_tackle_2_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(assist_tackle_2_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = assist_tackle_2_player_id, player_name = assist_tackle_2_player_name)

assist_tackle_3_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(assist_tackle_3_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = assist_tackle_3_player_id, player_name = assist_tackle_3_player_name)

assist_tackle_4_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(assist_tackle_4_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = assist_tackle_4_player_id, player_name = assist_tackle_4_player_name)

pass_defense_1_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(pass_defense_1_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = pass_defense_1_player_id, player_name = pass_defense_1_player_name)

pass_defense_2_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(pass_defense_2_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = pass_defense_2_player_id, player_name = pass_defense_2_player_name)

fumble_recovery_1_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(fumble_recovery_1_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = fumble_recovery_1_player_id, player_name = fumble_recovery_1_player_name)

fumble_recovery_2_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(fumble_recovery_2_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = fumble_recovery_2_player_id, player_name = fumble_recovery_2_player_name)

#MERGE SUBDATAFRAMES including duplicates
defense_2018_df_merged <- rbind(interceptions_pbp_2018, 
                                blocked_pbp_2018, 
                                qb_hit_1_pbp_2018, qb_hit_2_pbp_2018, 
                                forced_fumble_1_pbp_2018, forced_fumble_2_pbp_2018,
                                solo_tackle_1_pbp_2018, solo_tackle_2_pbp_2018,
                                assist_tackle_1_pbp_2018, assist_tackle_2_pbp_2018, assist_tackle_3_pbp_2018, assist_tackle_4_pbp_2018,
                                pass_defense_1_pbp_2018, pass_defense_2_pbp_2018,
                                fumble_recovery_1_pbp_2018, fumble_recovery_2_pbp_2018
)

#remove duplicate plays for individual players BUT WITHOUT REMOVING SAME
#use code below, but qualify that you're removing duplicates of same play_id AND player_name 
defense_2018_df_merged <- defense_2018_df_merged %>%
  distinct(play_id, player_id, .keep_all = TRUE)

#CREATE FINAL CALCULATION
playmaking_epa_2018 <- defense_2018_df_merged %>%
  filter(epa < 0) %>%
  group_by(player_id) %>%
  summarise(
    player_name = paste(unique(player_name), collapse = ', '),
    team = paste(unique(defteam), collapse = ', '),
    plays_made = length(epa), 
    playmaking_epa = abs(sum(epa)),
    pass_playmaking_epa = abs(sum(epa[play_type == "pass"])),
    rush_playmaking_epa = abs(sum(epa[play_type == "run"])),
    games_played = length(unique(game_id))
  ) %>%
  filter(plays_made >= 5)

#Create Bills dataframe
bills_playmaking_2018 <- defense_2018_df_merged %>%
  filter(epa < 0 & defteam == "BUF") %>%
  mutate(season = str_sub(game_id,3,4)) %>%
  group_by(player_id, season) %>%
  summarise(
    player_name = paste(unique(player_name), collapse = ', '),
    team = paste(unique(defteam), collapse = ', '),
    plays_made = length(epa), 
    playmaking_epa = abs(sum(epa)),
    pass_playmaking_epa = abs(sum(epa[play_type == "pass"])),
    rush_playmaking_epa = abs(sum(epa[play_type == "run"]))
  ) %>%
  filter(plays_made >= 5) %>%
  left_join(nfl_logos_df, by = c("team" = "team_code")) %>%
  mutate(player_name = str_extract(player_name, "[:upper:]\\.[:alpha:]+")) %>%
  #mutate(player_name = str_c(player_name, season, sep = " '")) %>%
  ungroup()

top_10 <- bills_playmaking_2018 %>% dplyr::top_frac(0.25, playmaking_epa) #%>% filter(player_name != "T.Edmunds")
top_10_pass <- bills_playmaking_2018 %>% arrange(desc(pass_playmaking_epa)) %>%
  dplyr::top_frac(0.25, pass_playmaking_epa)
top_10_rush <- bills_playmaking_2018 %>% dplyr::top_frac(0.25, rush_playmaking_epa)

top_10 <- bind_rows(top_10, top_10_pass, top_10_rush) %>%
  distinct(player_id, season, .keep_all = TRUE)
source("~/Desktop/nflscrapR/Code/theme_BUF.R")

ggplot(data = bills_playmaking_2018) +
  geom_point(data = playmaking_epa_2018, 
             aes (x = pass_playmaking_epa, y = rush_playmaking_epa, size = plays_made), 
             color = "grey", alpha = 0.4) +
  geom_point(aes(x = pass_playmaking_epa, y = rush_playmaking_epa, size = plays_made), 
             color = "#00338d") +
  #geom_text_repel(aes(x = pass_playmaking_epa, y = rush_playmaking_epa, label = player_name), size = 4) +
  theme_BUF() + 
  labs(title = "Bills Defensive Playmaking 2019", x = "Pass Playmaking EPA", y = "Rush Playmaking EPA",
       caption = "Data from nflscrapR") +
  geom_image(aes(image = url, x = 0.5, y = max(playmaking_epa_2018$rush_playmaking_epa) - 2.5), size = 0.1) +
  geom_text_repel(data = top_10, 
                  aes(x = pass_playmaking_epa, y = rush_playmaking_epa, label = player_name), 
                  size = 4) +
  theme(legend.position = c(0.9, 0.7)) +
  scale_size_continuous(name = "Plays Made") #+ 
  scale_x_continuous(breaks = seq(0,50,10), minor_breaks = seq(5,55,10))

ggsave("~/Desktop/nflscrapR/Output/DefensivePlaymakingEPABills2019_week7.jpg", width = 8, height = 5, units = "in")

View(playmaking_epa_2018 %>%
  group_by(team) %>%
  summarise(team_playmaking_epa = sum(playmaking_epa, na.rm =T),
            team_pass_playmaking_epa = sum(pass_playmaking_epa, na.rm = T),
            team_rush_playmaking_epa = sum(rush_playmaking_epa, na.rm = T),
            games = max(games_played)))

View(playmaking_epa_2018 %>%
       group_by(team) %>%
       summarise(epa = sum(playmaking_epa), pass_epa = sum(pass_playmaking_epa), rush_epa = sum(rush_playmaking_epa)))
Edmunds <- defense_2018_df_merged %>%
  filter(player_name == "T.Edmunds", epa < 0, defteam == "BUF") %>%
  mutate(epa = abs(epa)) %>%
  arrange(desc(epa))

#Create Cowboys chart w/snap counts (must do this manually from FO data)
defense_chart_2018 <- ggplot(aes(bills_playmaking_2018$snap_count, bills_playmaking_2018$playmaking_epa)) + 
  geom_point(color = 'royalblue2') + 
  geom_text_repel(data = bills_playmaking_2018, aes(bills_playmaking_2018$snap_count, bills_playmaking_2018$playmaking_epa, label = bills_playmaking_2018$player_name), size = 3, color = 'royalblue4') +
  geom_abline(slope = 0.04660413, intercept = 0, linetype = "dashed", col = "gray") +
  labs(
    x = "Snap count",
    y = "Playmaking EPA",
    title = "Playmaking leaders on defense: Jaylon Smith is all the way back",
    subtitle = "Dashed line represents average Cowboys playmaking rate, Weeks 1-10",
    caption = "Data from nflscrapR, created by Ron Yurko, Sam Ventura and Maksim Horowitz"
  )

defense_chart_2018