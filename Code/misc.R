library(tidyverse)
data_directory <- "~/Desktop/nflscrapR/data/"
output_directory <- "~/Desktop/nflscrapR/Output/"

pbp_2018 <- readRDS(paste0(data_directory, "reg_pbp_2018.rds"))

pbp_test <- pbp_2018 %>%
  mutate(winning_team = ifelse(home_score > away_score, home_team, away_team),
         winning_epa = ifelse(posteam == winning_team, epa, -epa),
         winning_wpa = ifelse(posteam == winning_team, wpa, -wpa))

ggplot(pbp_test) +
  geom_density(aes(x = winning_epa))
sum(pbp_test$winning_epa, na.rm = T)

neutral_plays <- pbp_2018 %>%
  filter(play_type %in% c("pass", "run"), wp >= 0.25, wp <= 0.75, game_seconds_remaining >= 450) %>%
  group_by(posteam) %>%
  summarise(neut_rushing_plays = sum(rush_attempt), neut_passing_plays = sum(qb_dropback), neut_plays = n()) %>%
  mutate(neut_rushing_plays = neut_plays - neut_passing_plays,
         neut_pass_ratio = neut_passing_plays / neut_rushing_plays)
  

all_plays <- pbp_2018 %>%
  filter(play_type %in% c("pass", "run")) %>%
  group_by(posteam) %>%
  summarise(rushing_plays = sum(rush_attempt), passing_plays = sum(qb_dropback), plays = n()) %>%
  mutate(rushing_plays = plays - passing_plays,
         pass_ratio = passing_plays / rushing_plays)
script <- left_join(neutral_plays, all_plays)

ggplot(neutral_plays) + 
  geom_text(aes(x = posteam, y = pass_ratio, label = posteam))

ggplot(script, aes(x = neut_pass_ratio, y = pass_ratio)) + 
  geom_text(aes(label = posteam)) +
  geom_abline(slope = 1, intercept = c(0,0)) +
  theme_CC() +
  scale_y_continuous(breaks = seq(1.0, 2.5, 0.25)) +
  scale_x_continuous(breaks = seq(1.0,2.5, 0.25)) +
  labs(x = "Neutral Game Script Pass/Rush Ratio", y = "Overall Pass/Rush Ratio", 
       caption = "Neutral = wp between 0.25 and 0.75 \n data from @nflscrapR") +
  geom_image(aes(x = 1, y = 2.4, image = "~/Desktop/nflscrapR/CClogo.jpg"), size = 0.125)

pbp_18 <- read_csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2018.csv")
defense_epa_18 <- pbp_18 %>%
  filter(!is.na(defteam)) %>%
  group_by(defteam) %>%
  # Create a variable that denotes if it is either a pass or run:
  mutate(pass_run_ind = ifelse(play_type %in% c("pass", "run"),
                               1, 0),
         # Punts without penalties:
         punt_ind = ifelse(play_type == "punt", 1, 0),
         # Kickoffs without penalties:
         kickoff_ind = ifelse(play_type == "kickoff", 1, 0),
         # Designed run indicator:
         designed_run_ind = ifelse(play_type == "run" & qb_dropback == 0, 1, 0),
         # no fumble indicator:
         no_fumble_ind = ifelse(fumble_lost == 0, 1, 0)) %>%
  summarise(# Variables with counts:
    n_games = length(unique(game_id)),
    n_dropbacks = sum(qb_dropback, na.rm = TRUE),
    n_all_runs = sum(rush_attempt * pass_run_ind, na.rm = TRUE),
    n_designed_runs = sum(designed_run_ind, na.rm = TRUE),
    n_pass_attempts = sum(pass_attempt * pass_run_ind, na.rm = TRUE),
    n_completions = sum(complete_pass, na.rm = TRUE),
    n_sacks = sum(sack, na.rm = TRUE),
    n_sacks_no_fumbles = sum(sack * no_fumble_ind, na.rm = TRUE),
    n_punts = sum(punt_ind, na.rm = TRUE),
    n_kickoffs = sum(kickoff_ind, na.rm = TRUE),
    n_pass_run_plays = sum(pass_run_ind, na.rm = TRUE),
    # Variables with total EPA and WPA for various types of plays:
    def_total_epa = sum(epa, na.rm = TRUE),
    def_total_wpa = sum(wpa, na.rm = TRUE),
    def_pass_run_epa = sum(epa * pass_run_ind, na.rm = TRUE),
    def_pass_run_wpa = sum(wpa * pass_run_ind, na.rm = TRUE),
    def_dropback_epa = sum(epa * qb_dropback * pass_run_ind, na.rm = TRUE),
    def_dropback_wpa = sum(wpa * qb_dropback * pass_run_ind, na.rm = TRUE),
    def_pass_epa = sum(epa * pass_attempt * pass_run_ind, na.rm = TRUE),
    def_pass_wpa = sum(wpa * pass_attempt * pass_run_ind, na.rm = TRUE),
    def_all_run_epa = sum(epa * rush_attempt * pass_run_ind, na.rm = TRUE),
    def_all_run_wpa = sum(wpa * rush_attempt * pass_run_ind, na.rm = TRUE),
    def_des_run_epa = sum(epa * designed_run_ind, na.rm = TRUE),
    def_des_run_wpa = sum(wpa * designed_run_ind, na.rm = TRUE),
    def_total_comp_air_epa = sum(complete_pass * air_epa, na.rm = TRUE),
    def_total_comp_air_wpa = sum(complete_pass * air_wpa, na.rm = TRUE),
    def_total_comp_yac_epa = sum(complete_pass * yac_epa, na.rm = TRUE),
    def_total_comp_yac_wpa = sum(complete_pass * yac_wpa, na.rm = TRUE),
    def_punt_epa = sum(epa * punt_ind, na.rm = TRUE),
    def_punt_wpa = sum(wpa * punt_ind, na.rm = TRUE),
    def_kickoff_epa = sum(epa * kickoff_ind, na.rm = TRUE),
    def_kickoff_wpa = sum(wpa * kickoff_ind, na.rm = TRUE),
    def_sacks_epa = sum(epa * sack, na.rm = TRUE),
    def_sacks_wpa = sum(wpa * sack, na.rm = TRUE),
    def_sacks_no_fumbles_epa = sum(epa * sack * no_fumble_ind,
                                   na.rm = TRUE) / n_sacks_no_fumbles,
    # Success rates:
    def_pass_run_sr = length(which(epa < 0 & pass_run_ind == 1)) / n_pass_run_plays,
    def_all_runs_sr = length(which(epa < 0 & 
                                     rush_attempt == 1 & 
                                     pass_run_ind == 1)) / n_all_runs,
    def_des_runs_sr = length(which(epa < 0 & 
                                     designed_run_ind == 1)) / n_designed_runs,
    def_dropback_sr = length(which(epa < 0 & qb_dropback == 1 &
                                     pass_run_ind == 1)) / n_dropbacks) %>%
  mutate(# Now the rate level statistics, first EPA based:
    def_epa_per_game = -1 * def_total_epa / n_games,
    def_epa_per_pass_run = -1 * def_pass_run_epa / n_pass_run_plays,
    def_epa_per_dropback = -1 * def_dropback_epa / n_dropbacks,
    def_epa_per_pass = -1 * def_pass_epa / n_pass_attempts,
    def_epa_per_all_run = -1 * def_all_run_epa / n_all_runs,
    def_epa_per_des_run = -1 * def_des_run_epa / n_designed_runs,
    def_air_epa_per_comp = -1 * def_total_comp_air_epa / n_completions,
    def_yac_epa_per_comp = -1 * def_total_comp_yac_epa / n_completions,
    def_epa_per_punt = -1 * def_punt_epa / n_punts,
    def_epa_per_ko = -1 * def_kickoff_epa / n_kickoffs,
    def_epa_per_sack = -1 * def_sacks_epa / n_sacks,
    def_epa_per_sack_no_fumbles = -1 * def_sacks_no_fumbles_epa / n_sacks_no_fumbles,
    # Now WPA based:
    def_wpa_per_game = -1 * def_total_wpa / n_games,
    def_wpa_per_pass_run = -1 * def_pass_run_wpa / n_pass_run_plays,
    def_wpa_per_dropback = -1 * def_dropback_wpa / n_dropbacks,
    def_wpa_per_pass = -1 * def_pass_wpa / n_pass_attempts,
    def_wpa_per_all_run = -1 * def_all_run_wpa / n_all_runs,
    def_wpa_per_des_run = -1 * def_des_run_wpa / n_designed_runs,
    def_air_wpa_per_comp = -1 * def_total_comp_air_wpa / n_completions,
    def_yac_wpa_per_comp = -1 * def_total_comp_yac_wpa / n_completions,
    def_wpa_per_punt = -1 * def_punt_wpa / n_punts,
    def_wpa_per_ko = -1 * def_kickoff_wpa / n_kickoffs,
    def_wpa_per_sack = -1 * def_sacks_wpa / n_sacks)


library(ggimage)

nfl_logos_df <- read_csv("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")

buf_nh <- pbp_2019 %>%
  filter(posteam == "BUF", no_huddle == 1) %>%
  filter(passer_player_name %in% c("J.Allen", NA)) %>%
  mutate(success = if_else(epa >=0, 1, 0)) %>%
  group_by(play_type) %>%
  summarise(plays = n(), epa_per_play = mean(epa, na.rm = T), yards_per_play = mean(yards_gained, na.rm = T),
            success_rate = mean(success, na.rm = T),
            comp_pct = mean(complete_pass, na.rm = T))

