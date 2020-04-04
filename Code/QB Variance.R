library(tidyverse)
library(nflscrapR)
library(ggimage)
library(stringi)

setwd("~/Desktop/nflscrapR")

proj_dir <- getwd()
nfl_logos_df <- read_csv("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv") %>%
  mutate(url = ifelse(team_code == "TEN", "http://habitatring.com/titans.png", url))

pbp_2019 <- readRDS("data/reg_pbp_2019.rds")
rosters <- read_csv("data/rosters.csv")
quarterbacks <- rosters %>% filter(position == "QB", season == 2019)

qb_plays <- pbp_2019 %>%
  filter(pass == 1 | rush == 1) %>%
  mutate(quarterback = ifelse(
    !is.na(passer_player_name),
    passer_player_name,
    rusher_player_name
  )) %>%
  mutate(quarterback = ifelse(
    quarterback == "Jos.Allen" & posteam == "BUF",
    "J.Allen",
    ifelse(quarterback == "G.Minshew II", "G.Minshew", quarterback)
  )) %>%
  dplyr::filter(quarterback %in% quarterbacks$abbr_player_name) %>%
  mutate(epa = ifelse(fumble_lost == 1 &
                        complete_pass == 1, comp_air_epa, epa)) %>%
  filter(!is.na(epa)) %>%
  group_by(quarterback) %>%
  mutate(play = cumsum(play))
  
  
quarterbacks_fun_input <- qb_plays %>%
  group_by(quarterback, posteam) %>%
  summarise(plays = n()) %>%
  filter(plays > 200) %>%
  select(quarterback, posteam) 

roll_averages <- function(qb, data = qb_plays){
  
  qbdata <- dplyr::filter(data, quarterback == qb)
  qbdata <- qbdata %>%
    arrange(game_date, desc(game_seconds_remaining))
 qbdata$rolling_epa_sd = zoo::rollapplyr(qb_data$epa, width = 40,fill = NA, FUN = sd)
 
 return(qb_data)
  
}
allen <- roll_averages(qb = "J.Allen")


intra_game_vol <- qb_plays %>%
  group_by(quarterback, game_id, posteam, week) %>%
  summarise(epa_var = sd(epa, na.rm = T)^2,
            plays = n()) %>%
  left_join(nflteams, by = c("posteam" = "abbr")) %>% 
  filter(!is.na(epa_var), 
         plays >= 15,
         quarterback %in% quarterbacks_fun_input$quarterback) %>%
  left_join(nfl_logos_df, by = c("team" = "team", "posteam" = "team_code")) 


rolling_data <- map_dfr(quarterbacks_fun_input$quarterback, roll_averages) 



quantiles <- quantile(intra_game_vol$epa_var, probs = c(0.1,  0.5,  0.9))

ggplot(data = intra_game_vol, 
       aes(x = week, y = epa_var)) +
  geom_line(aes(color = primary, group = quarterback)) +
  geom_point(aes(color = secondary, group = quarterback)) +
  theme_bw() +
  labs(title = "Intra-Game Volatility",
       subtitle = "2019 Regular Season",
       x = "Week",
       y = "Variance (log-scaled)",
       caption = "Data from @nflscrapR
       Graph by @BufBillsStats") +
  facet_wrap(~division, ncol = 4) +
  geom_image(data = intra_game_vol %>% group_by(quarterback) %>% top_n(1, week),
             aes(image = url), size = 0.10) + 
  scale_color_identity() +
  scale_x_continuous(breaks = c(0,4,8,12,16)) +
  #scale_y_log10() +
  geom_hline(yintercept = quantiles, linetype = 2, color = "black") +
  annotate("text", x = 1, y = quantiles[1]-.075, label = "10%") +
  annotate("text", x = 1, y = quantiles[2]+.08, label = "50%") +
  annotate("text", x = 1, y = quantiles[3]+.1, label = "90%")

ggsave(proj_dir %s+% "/Output/QB_variance_2019.jpg", width = 16.1, height = 8.0, units = "in")


player = "J.Allen"
team_var = "BUF"

make_boxplot <- function(player, team_var){
  
  player_data <- intra_game_vol %>% filter(quarterback == player, posteam == team_var)
  wks <- nrow(player_data)
  
  graph <- ggplot(data = intra_game_vol, aes(y = epa_var)) +
    geom_boxplot() +
    geom_point(data = player_data, 
               aes(x = (week - 8) / 25, color = primary), size = 3) +
    geom_line(data = player_data, 
              aes(x = (week - 8) / 25, color = secondary), size = 0.5) +
    scale_color_identity() +
    scale_y_log10() +
    theme_bw() +
    theme(axis.ticks.x = element_blank())
  
  player_name <- rosters %>% 
    filter(season == 2019, abbr_player_name == player, team == team_var) %>%
    select(full_player_name, team) %>%
    left_join(nflteams, by = c("team" = "abbr"))

  
    graph <- graph +
      labs(title = "QB Variance, 2019", x = "", y = "Variance", 
           subtitle = paste0(player_name$full_player_name[1], ", ", player_name$team.y[1])) +
    scale_x_continuous(labels = NULL)
  
  
  
  
  ggsave(proj_dir %s+% "/Output/Variance Graphs/" %s+% player_name$full_player_name[1] %s+% "_variance_boxplot_2019.jpg", 
         width = 4, height = 6, units = "in")
  
}

mapply(make_boxplot, quarterbacks_fun_input$quarterback, quarterbacks_fun_input$posteam)

# for (i in 1:nrow(quarterbacks_fun_input)) {
#   make_boxplot(quarterbacks_fun_input$quarterback[i], quarterbacks_fun_input$posteam[i])
# }
# 
# i <- 1
# 
# acf(intra_game_vol$epa_var, lag.max = 16)
# 
# ggplot(data = intra_game_vol, aes(y = epa_var)) +
#   geom_boxplot() +
#   geom_point(data = intra_game_vol %>% filter(quarterback %in% c("J.Allen")), 
#                                                aes(x = (week - 8) / 25, color = primary), size = 3) +
#   geom_line(data = intra_game_vol %>% filter(quarterback %in% c("J.Allen")), 
#              aes(x = (week - 8) / 25, color = secondary), size = 0.5) +
#   scale_color_identity() +
#   scale_y_log10() +
#   theme_bw() +
#   labs(title = "QB Variance, 2019", x = "", y = "Variance", subtitle = "Josh Allen, Buffalo Bills") +
#   scale_x_continuous(labels = NULL)
# ggsave(proj_dir %s+% "/Output/Variance Graphs/Allen_variance_boxplot_2019.jpg", width = 3, height = 5, units = "in")



View(qb_plays_all %>%
  select(quarterback, quarterback_id) %>%
  distinct() %>%
  arrange(quarterback))
