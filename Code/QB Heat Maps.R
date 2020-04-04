library(tidyverse)

output_dir <- "~/Desktop/nflscrapR/Output/"

qb_heat_map_comp_perc <- function(reg_pbp, quarterback_string, qb_title, year = 2019) {
  
  source("~/Desktop/nflscrapR/Code/theme_BUF.R")
  
  avg_comp_by_location <- reg_pbp %>%
    filter(play_type == "pass", !is.na(pass_length) == 1, !is.na(air_yards) == 1) %>%
    filter(receiver_player_name != "K.Benjamin")%>%
    select(posteam, yardline_100, play_type, yards_gained, shotgun, no_huddle, pass_length, pass_location, air_yards, passer_player_name,
           ydstogo, receiver_player_id, receiver_player_name, pass_attempt, complete_pass, interception) %>%
    mutate(air_yards_to_sticks = air_yards - ydstogo) %>%
    #mutate(depth_of_target = cut(air_yards, breaks = seq(-10, 60, by = 10), by = FALSE)) %>%
    mutate(depth_of_target = cut(air_yards, breaks = c(-10, 0, 10, 20, 30, 70), by = FALSE)) %>%
    #mutate(depth_of_target = cut(air_yards_to_sticks, breaks =  c(-20, -10, -5,-2.5, 2.5, 5, 10, 20, 60), by = FALSE)) %>%
    filter(!is.na(depth_of_target) == 1) %>%
    group_by(pass_location, depth_of_target) %>%
    summarise(comp_pct_league = sum(complete_pass) / n(), attempts_league = n())
  
  reg_pbp %>%
    filter(passer_player_name == quarterback_string, season %in% year ) %>%
    filter(play_type == "pass", !is.na(pass_length) == 1, !is.na(air_yards) == 1) %>%
    filter(receiver_player_name != "K.Benjamin") %>%
    mutate(throw_away = ifelse(str_detect(desc, "thrown away") ==1, 1, 0)) %>%
    filter(throw_away == 0) %>%
    mutate(air_yards_to_sticks = air_yards - ydstogo) %>%
    #mutate(depth_of_target = cut(air_yards, breaks = seq(-10, 60, by = 10), by = FALSE)) %>%
    mutate(depth_of_target = cut(air_yards, breaks =  c(-10, 0, 10, 20, 30, 70), by = FALSE)) %>%
    #mutate(depth_of_target = cut(air_yards_to_sticks, breaks =  c(-20, -10, -5,-2.5, 2.5, 5, 10, 20, 60), by = FALSE)) %>%
    filter(!is.na(depth_of_target) == 1) %>%
    group_by(pass_location, depth_of_target) %>%
    summarise(comp_pct = sum(complete_pass) / n(), attempts = n())%>%
    left_join(avg_comp_by_location) %>%
    mutate(difference = comp_pct - comp_pct_league) %>%
    mutate(difference = ifelse(difference >0.25, 0.25, difference))%>%
    ggplot(aes(x = pass_location, y = depth_of_target)) +
      geom_tile(aes(fill = difference, alpha = attempts)) +
      geom_text(aes(label = paste(round(100 * comp_pct, 1),"%, League:", round(100 * comp_pct_league, 1), "%")), nudge_y = .135) +
      geom_text(aes(label = paste("Attempts:",attempts)), nudge_y = -.135)+
      theme_BUF()+
      scale_fill_gradient2(low = "#CC0000", mid = "white", high = "#33CC33", midpoint = 0, guide = FALSE) +
      scale_alpha(range = c(0.75, 1), guide = FALSE) +
      ggtitle(paste0(qb_title," ", year, " Heat Map"))+
      labs(y = "Target Depth", 
           x = "Target Location",
           caption = "Graph by @BufBillsStats\nData from @nflscrapR")+
      geom_hline(aes(yintercept = 2.5), color = "yellow", size = 0.75) +
    geom_hline(aes(yintercept = 1.5), color = "black", size = 0.75)
  
  ggsave(paste0(output_dir, quarterback_string,"Heat Map.jpg"), width = 8, height = 5, units=c("in"))
  
}
qb_heat_map_comp_perc(pbp_all, "J.Garoppolo", "JimmyG")


qb_heat_map_epa <- function(reg_pbp_2018, quarterback_string, qb_title, year = 2019) {
  
  source("~/Desktop/nflscrapR/Code/theme_BUF.R")
  
  avg_epa_by_location <- reg_pbp_2018 %>%
    filter(play_type == "pass", !is.na(pass_length) == 1, !is.na(air_yards) == 1, !is.na(epa) == 1) %>%
    filter(receiver_player_name != "K.Benjamin")%>%
    select(posteam, yardline_100, play_type, yards_gained, shotgun, no_huddle, pass_length, pass_location, air_yards, passer_player_name,
           ydstogo, epa, wpa, receiver_player_id, receiver_player_name, pass_attempt, complete_pass, interception, desc) %>%
    mutate(air_yards_to_sticks = air_yards - ydstogo) %>%
    #mutate(depth_of_target = cut(air_yards, breaks = seq(-10, 60, by = 10), by = FALSE)) %>%
    mutate(depth_of_target = cut(air_yards, breaks = c(-10, 0, 10, 20, 30, 70), by = FALSE)) %>%
    #mutate(depth_of_target = cut(air_yards_to_sticks, breaks =  c(-20, -10, -5,-2.5, 2.5, 5, 10, 20, 60), by = FALSE)) %>%
    filter(!is.na(depth_of_target) == 1) %>%
    group_by(pass_location, depth_of_target) %>%
    summarise(league_avg_epa = mean(epa), attempts_league = n())
  
  reg_pbp_2018 %>%
    filter(passer_player_name == quarterback_string, season %in% year) %>%
    filter(play_type == "pass", !is.na(pass_length) == 1, !is.na(air_yards) == 1, !is.na(epa) == 1) %>%
    filter(receiver_player_name != "K.Benjamin") %>%
    mutate(throw_away = ifelse(str_detect(desc, "thrown away") ==1, 1, 0)) %>%
    filter(throw_away == 0) %>%
    mutate(air_yards_to_sticks = air_yards - ydstogo) %>%
    #mutate(depth_of_target = cut(air_yards, breaks = seq(-10, 60, by = 10), by = FALSE)) %>%
    mutate(depth_of_target = cut(air_yards, breaks = c(-10, 0, 10, 20, 30, 70), by = FALSE)) %>%
    #mutate(depth_of_target = cut(air_yards_to_sticks, breaks =  c(-20, -10, -5,-2.5, 2.5, 5, 10, 20, 60), by = FALSE)) %>%
    filter(!is.na(depth_of_target) == 1) %>%
    group_by(pass_location, depth_of_target) %>%
    summarise(avg_epa = mean(epa), attempts = n())%>%
    left_join(avg_epa_by_location) %>%
    mutate(difference = avg_epa - league_avg_epa) %>%
    mutate(difference = ifelse(difference >= 1, 1, difference))%>%
    mutate(difference = ifelse(difference <= -1, -1, difference))%>%
    ggplot(aes(x = pass_location, y = depth_of_target)) +
      geom_tile(aes(fill = difference, alpha = attempts), color = "black") +
      geom_text(aes(label = paste(round(avg_epa, 3)," League:", round(league_avg_epa, 3))), nudge_y = .135) +
      geom_text(aes(label = paste("Attempts:",attempts)), nudge_y = -.135)+
      theme_BUF()+
      scale_fill_gradient2(low = "#CC0000", mid = "white", high = "#33CC33", midpoint = 0, guide = FALSE) +
      scale_alpha(range = c(0.75, 1), guide = FALSE) +
      ggtitle(paste(qb_title, " ", year, " EPA Heat Map"))+
      labs(y = "Target Depth", x = "Pass Location",
          caption = "Graph by @BufBillsStats \n data from @nflscrapR") +
      geom_hline(aes(yintercept = 2.5), color = "yellow", size = 1) +
      geom_hline(aes(yintercept = 1.5), color = "black")
  
  ggsave(paste0(output_dir, quarterback_string, " " ,year, " EPA Heat Map.jpg"), 
         width = 8, height = 5, units = c("in"))
  
}
qb_heat_map_epa(pbp_all, "J.Garoppolo", "JimmyG")




### Defensive heat maps

def_heat_map_comp_perc <- function(reg_pbp, team_abbr, team_name, year = 2019) {
  
  source("~/Desktop/nflscrapR/Code/theme_BUF.R")
  
  avg_comp_by_location <- reg_pbp %>%
    filter(play_type == "pass", !is.na(pass_length) == 1, !is.na(air_yards) == 1) %>%
    filter(receiver_player_name != "K.Benjamin")%>%
    select(posteam, yardline_100, play_type, yards_gained, shotgun, no_huddle, pass_length, pass_location, air_yards, passer_player_name,
           ydstogo, receiver_player_id, receiver_player_name, pass_attempt, complete_pass, interception) %>%
    mutate(air_yards_to_sticks = air_yards - ydstogo) %>%
    #mutate(depth_of_target = cut(air_yards, breaks = seq(-10, 60, by = 10), by = FALSE)) %>%
    mutate(depth_of_target = cut(air_yards, breaks = c(-10, 0, 10, 20, 30, 70), by = FALSE)) %>%
    #mutate(depth_of_target = cut(air_yards_to_sticks, breaks =  c(-20, -10, -5,-2.5, 2.5, 5, 10, 20, 60), by = FALSE)) %>%
    filter(!is.na(depth_of_target) == 1) %>%
    group_by(pass_location, depth_of_target) %>%
    summarise(comp_pct_league = sum(complete_pass) / n(), attempts_league = n())
  
  reg_pbp %>%
    filter(defteam == team_abbr, season %in% year ) %>%
    filter(play_type == "pass", !is.na(pass_length) == 1, !is.na(air_yards) == 1) %>%
    filter(receiver_player_name != "K.Benjamin") %>%
    mutate(throw_away = ifelse(str_detect(desc, "thrown away") ==1, 1, 0)) %>%
    filter(throw_away == 0) %>%
    mutate(air_yards_to_sticks = air_yards - ydstogo) %>%
    #mutate(depth_of_target = cut(air_yards, breaks = seq(-10, 60, by = 10), by = FALSE)) %>%
    mutate(depth_of_target = cut(air_yards, breaks =  c(-10, 0, 10, 20, 30, 70), by = FALSE)) %>%
    #mutate(depth_of_target = cut(air_yards_to_sticks, breaks =  c(-20, -10, -5,-2.5, 2.5, 5, 10, 20, 60), by = FALSE)) %>%
    filter(!is.na(depth_of_target) == 1) %>%
    group_by(pass_location, depth_of_target) %>%
    summarise(comp_pct = sum(complete_pass) / n(), attempts = n())%>%
    left_join(avg_comp_by_location) %>%
    mutate(difference = comp_pct - comp_pct_league) %>%
    mutate(difference = ifelse(difference >0.25, 0.25, difference))%>%
    ggplot(aes(x = pass_location, y = depth_of_target)) +
      geom_tile(aes(fill = difference, alpha = attempts)) +
      geom_text(aes(label = paste(round(100 * comp_pct, 1),"%, League:", round(100 * comp_pct_league, 1), "%")), nudge_y = .135) +
      geom_text(aes(label = paste("Attempts:",attempts)), nudge_y = -.135)+
      theme_BUF()+
      scale_fill_gradient2(low = "#CC0000", mid = "white", high = "#33CC33", midpoint = 0, guide = FALSE) +
      scale_alpha(range = c(0.75, 1), guide = FALSE) +
      ggtitle(paste0(team_name," Def. ", year, " Heat Map"))+
      labs(y = "Target Depth", 
          x = "Target Location",
          caption = "Graph by @BufBillsStats\nData from @nflscrapR")+
      geom_hline(aes(yintercept = 2.5), color = "yellow", size = 0.75) +
      geom_hline(aes(yintercept = 1.5), color = "black", size = 0.75)
  
  ggsave(paste0(output_dir, team_abbr,"Heat Map.jpg"), width = 8, height = 5, units=c("in"))
  
}
def_heat_map_comp_perc(pbp_all, "DAL", "Cowboys")


def_heat_map_epa <- function(reg_pbp_2018, team_abbr, team_name, year = 2019) {
  
  source("~/Desktop/nflscrapR/Code/theme_BUF.R")
  
  avg_epa_by_location <- reg_pbp_2018 %>%
    filter(play_type == "pass", !is.na(pass_length) == 1, !is.na(air_yards) == 1, !is.na(epa) == 1) %>%
    filter(receiver_player_name != "K.Benjamin")%>%
    select(posteam, yardline_100, play_type, yards_gained, shotgun, no_huddle, pass_length, pass_location, air_yards, passer_player_name,
           ydstogo, epa, wpa, receiver_player_id, receiver_player_name, pass_attempt, complete_pass, interception, desc) %>%
    mutate(air_yards_to_sticks = air_yards - ydstogo) %>%
    #mutate(depth_of_target = cut(air_yards, breaks = seq(-10, 60, by = 10), by = FALSE)) %>%
    mutate(depth_of_target = cut(air_yards, breaks = c(-10, 0, 10, 20, 30, 70), by = FALSE)) %>%
    #mutate(depth_of_target = cut(air_yards_to_sticks, breaks =  c(-20, -10, -5,-2.5, 2.5, 5, 10, 20, 60), by = FALSE)) %>%
    filter(!is.na(depth_of_target) == 1) %>%
    group_by(pass_location, depth_of_target) %>%
    summarise(league_avg_epa = mean(epa), attempts_league = n())
  
  reg_pbp_2018 %>%
    filter(defteam == team_abbr, season %in% year) %>%
    filter(play_type == "pass", !is.na(pass_length) == 1, !is.na(air_yards) == 1, !is.na(epa) == 1) %>%
    filter(receiver_player_name != "K.Benjamin") %>%
    mutate(throw_away = ifelse(str_detect(desc, "thrown away") ==1, 1, 0)) %>%
    filter(throw_away == 0) %>%
    mutate(air_yards_to_sticks = air_yards - ydstogo) %>%
    #mutate(depth_of_target = cut(air_yards, breaks = seq(-10, 60, by = 10), by = FALSE)) %>%
    mutate(depth_of_target = cut(air_yards, breaks = c(-10, 0, 10, 20, 30, 70), by = FALSE)) %>%
    #mutate(depth_of_target = cut(air_yards_to_sticks, breaks =  c(-20, -10, -5,-2.5, 2.5, 5, 10, 20, 60), by = FALSE)) %>%
    filter(!is.na(depth_of_target) == 1) %>%
    group_by(pass_location, depth_of_target) %>%
    summarise(avg_epa = mean(epa), attempts = n())%>%
    left_join(avg_epa_by_location) %>%
    mutate(difference = avg_epa - league_avg_epa) %>%
    mutate(difference = ifelse(difference >= 1, 1, difference))%>%
    mutate(difference = ifelse(difference <= -1, -1, difference))%>%
    ggplot(aes(x = pass_location, y = depth_of_target)) +
      geom_tile(aes(fill = difference, alpha = attempts), color = "black") +
      geom_text(aes(label = paste(round(avg_epa, 3)," League:", round(league_avg_epa, 3))), nudge_y = .135) +
      geom_text(aes(label = paste("Attempts:",attempts)), nudge_y = -.135)+
      theme_BUF()+
      scale_fill_gradient2(low = "#CC0000", mid = "white", high = "#33CC33", midpoint = 0, guide = FALSE) +
      scale_alpha(range = c(0.75, 1), guide = FALSE) +
      ggtitle(paste(team_name, " Def. ", year, " EPA Heat Map"))+
      labs(y = "Target Depth", x = "Pass Location",
          caption = "Graph by @BufBillsStats \n data from @nflscrapR") +
      geom_hline(aes(yintercept = 2.5), color = "yellow", size = 1) +
      geom_hline(aes(yintercept = 1.5), color = "black")
  
  ggsave(paste0(output_dir, team_abbr, " " ,year, " EPA Heat Map.jpg"), 
         width = 8, height = 5, units = c("in"))
  
}
def_heat_map_epa(pbp_all, "DAL", "Cowboys")




allen_2019 <- pbp_all %>%
  filter(passer_player_name == "J.Allen", season == 2019, !is.na(air_yards)) 

sum(allen_2019$air_yards >= 30) / nrow(allen_2019)

pass_all <- pbp_all %>% filter(!is.na(air_yards))

sum(pass_all$air_yards >= 30) / nrow(pass_all)

ggplot(data = allen_2019, aes(x = air_yards)) +
  geom_density()










