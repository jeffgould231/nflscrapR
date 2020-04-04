library(tidyverse)
library(nflscrapR)
library(lubridate)
library(ggimage)

pbp_all <- data.frame()
nfl_logos_df <- read_csv("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")

pbp_all <- readRDS("~/Desktop/nflscrapR/data/pbp_all.rds")
seasons <- c(2009:2019)
for (i in 2015:2019) {
  data <- readRDS(paste0("~/Desktop/nflscrapR/data/reg_pbp_", i, ".rds")) 
  pbp_all <- bind_rows(pbp_all, data)
}
#pbp_all <- bind_rows(pbp_all, reg_pbp_2019 %>% mutate(game_date = as.Date(game_date),time = lubridate::hms(time)))
pbp_test <- pbp_all %>%
  select(play_id, game_id, home_team, away_team, posteam, defteam, yardline_100, game_date, game_seconds_remaining,
         qtr, down, time, ydstogo, desc, play_type, yards_gained, qb_dropback, qb_kneel, qb_spike, 
         qb_scramble, pass_location, air_yards, epa, air_epa, wpa, complete_pass, incomplete_pass, rush_attempt, pass_attempt,
         sack, fumble_lost, comp_air_epa, passer_player_id, passer_player_name, rusher_player_id, rusher_player_name,
         week, season
         )

allen_test <- pbp_test %>% filter(posteam == "BUF", week == 9, season == 2019)

pbp_passes <- pbp_test %>%
  filter(complete_pass == 1|incomplete_pass == 1 & air_yards >= -5) %>%
  filter(!is.na(air_yards), !is.na(pass_location)) %>%
  #select(complete_pass, desc, air_yards, pass_location) %>% 
  mutate(air_is_zero = ifelse(air_yards == 0, 1, 0))

pbp_passes$exp_comp <- predict(gam_y, pbp_passes)

pbp_test <- pbp_test %>% left_join(pbp_passes)
pbp_test <- pbp_test %>%
  mutate(cpoe = ifelse(!is.na(exp_comp), complete_pass - exp_comp, 0))



quarterbacks <- pbp_all %>%
  group_by(passer_player_id, passer_player_name) %>%
  summarise(attempts = n()) %>%
  filter(attempts > 140, !is.na(passer_player_name))

qb_plays_all <- pbp_all %>%
  filter(pass ==1|rush ==1) %>%
  filter(pass ==1 |rusher_player_id%in%quarterbacks$passer_player_id) %>%
  mutate(quarterback = ifelse(!is.na(passer_player_name), passer_player_name, rusher_player_name)) %>%
  mutate(quarterback = ifelse(quarterback == "Jos.Allen" & posteam == "BUF", "J.Allen", quarterback)) %>%
  select(-passer_player_id, -passer_player_name, -rusher_player_id, -rusher_player_name) %>%
  #arrange(quarterback, season, week, desc(game_seconds_remaining)) %>%
  mutate(epa = ifelse(fumble_lost ==1&complete_pass==1, comp_air_epa, epa)) %>%
  filter(quarterback %in% quarterbacks$passer_player_name, !is.na(epa), !is.na(yards_gained), !is.na(epa)) %>%
  mutate(adjYds = yards_gained + 20 * touchdown * complete_pass - 45 * interception)

ma <- function(x, n = 1000){stats::filter(x, rep(1 / n, n), sides = 1)}
qb_extract <- function(qb, data = qb_plays_all){
  #require(zoo)
  qbdata <- filter(data, quarterback == qb)
  plays <- nrow(qbdata)
  qbdata$dropback = 1:plays
  qbdata <- qbdata %>%
    mutate(rollingEPA300 = ifelse(dropback >= 300, ma(epa, 300), NA),
           rollingEPA500 = ifelse(dropback >= 500, ma(epa, 500), NA),
           rollingEPA1000 = ifelse(dropback >= 1000, ma(epa, 1000), NA),
           rollingEPA150 = ifelse(dropback >= 150, ma(epa, 150), NA),
           #rollingEPA = ma(epa, dropback - 50),
           rollingAdjYds350 = ifelse(dropback >= 350, ma(adjYds, 350), NA),
           rollingAdjYds500 = ifelse(dropback >= 500, ma(adjYds, 500), NA),
           rollingAdjYds1000 = ifelse(dropback >= 1000, ma(adjYds), NA),
           rollingAdjYds150 = ifelse(dropback >= 100, ma(adjYds, 150), NA)) %>%
    mutate(seconds_elapsed = 3600 - game_seconds_remaining) %>%
    mutate()
  qb <- qbdata
  return(qb)
}

passers <- quarterbacks$passer_player_name


#NFLcolors <- read_csv("footballcolors.csv")
quarterback_averages <- map_dfr(passers, qb_extract)


testQBs <- c("A.Dalton", "C.Newton", "A.Luck", "R.Wilson", "R.Tannehill", "D.Carr", "K.Cousins", "B.Bortles", "J.Winston", "C.Kaepernick",
             "M.Mariota", "N.Foles", "B.Gabbert", "D.Prescott", "C.Wentz", "J.Goff", "T.Taylor", "R.Griffin", "B.Osweiler", "C.Ponder",
             "B.Weeden", "G.Smith", "T.Bridgewater", "T.Siemian", "M.Glennon", "M.Trubisky", "D.Watson", "J.Locker", "P.Mahomes", "E.Manuel",
             "J.Brissett", "D.Kizer", "B.Mayfield", "S.Darnold", "J.Rosen", "J.Garoppolo", "Z.Mettenberger", "J.Allen")
controlAvg <- quarterback_averages %>%
  filter(quarterback %in% testQBs) %>%
  group_by(dropback) %>%
  summarise(EPA350 = mean(rollingEPA350), EPA500 = mean(rollingEPA500), EPA1000 = mean(rollingEPA1000), EPA100 = mean(rollingEPA100),
            AdjYds100 = mean(rollingAdjYds100), AdjYds350 = mean(rollingAdjYds350), AdjYds500 = mean(rollingAdjYds500), AdjYds1000 = mean(rollingEPA1000))

QBs_2018 <- c("B.Mayfield", "J.Allen", "L.Jackson", "S.Darnold", "J.Garoppolo")
QBs_2017 <- c("M.Trubisky", "P.Mahomes", "D.Watson")
QBs_2016 <- c("C.Wentz", "J.Goff", "D.Prescott")
QBs_2019 <- c("K.Murray", "D.Jones", "G.Minshew", "D.Haskins")

plot_data <- quarterback_averages %>%
  filter(quarterback %in% c("D.Prescott")) %>%
  #filter(season != 2015|quarterback != "P.Manning") %>%
  #filter(dropback %% 5 == 0) %>%
  mutate(posteam = if_else(posteam == "NE", "SF", posteam)) %>%
  left_join(nflteams, by = c("posteam" = "abbr")) %>%
  group_by(quarterback) %>%
  mutate(alpha = dropback / max(dropback)) %>%
  ungroup() 

date_data <- plot_data %>%
  mutate(season = lubridate::year(game_date), month = lubridate::month(game_date)) %>%
  filter(month != 1) %>%
  group_by(quarterback, season) %>%
  slice(1)
game_points <- plot_data %>%
  mutate(season = lubridate::year(game_date), month = lubridate::month(game_date)) %>%
  #filter(month != 1) %>%
  group_by(quarterback, season, game_id) %>%
  slice(1)
# %>%
#   ungroup()%>%
#   mutate(month = month(month))

color_data <- plot_data %>%
  #filter(quarterback %in% QBs_2017) %>%
  distinct(quarterback, .keep_all = TRUE) %>%
  arrange(quarterback)


logo_plot <- plot_data %>%
  group_by(quarterback) %>%
  top_n(1, dropback) %>%
  select(245:280) %>%
  left_join(nfl_logos_df, by = c())

source("~/Desktop/nflscrapR/Code/theme_BUF.R")
cols = color_data$primary
cols[2] = color_data$secondary[2]
cols2 = color_data$secondary
cols[3] = color_data$primary[3]

get_quantiles <-  qb_plays_all %>%
  group_by(quarterback, game_id) %>%
  summarise(total_epa = sum(epa, na.rm = T), plays = n(), epa_per_play = mean(epa, na.rm = T)) %>%
  filter(plays > 10)

quantile(quarterback_averages$rollingEPA150, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = T)

quants <- tibble(x = c(300,300,300,300,300),
                 y = c(-0.05901271,  0.01307063,  0.09525126,  0.18738247,  0.26951500),
                 text = c("10%", "25%", "50%", "75%", "90%"))

quants2 <- tibble(x = c(150, 150, 150, 150, 150),
                 y = c(-0.12204072, -0.02234865,  0.08591105,  0.19485147,  0.30143034),
                 text = c("10%", "25%", "50%", "75%", "90%"))


ggplot(data = plot_data, aes(x = dropback, y = rollingEPA300)) +
  geom_line(aes(color = quarterback, alpha = alpha), size = 1) +
  geom_point(data = date_data, aes(color = quarterback), size = 3) +
  geom_text(data = date_data, aes(label = season),  
            color = "black", nudge_y = 0.02)+
  geom_point(data = game_points, aes(), size = .75, color = "black", fill = "black") +
  theme_BUF() +
  #transition_reveal(dropback) +
  theme(legend.position = c(0.9, .8), legend.direction = "vertical", legend.background = element_blank(),
        legend.title = element_blank()) +
  scale_color_manual(values = cols) +
  geom_image(data = logo_plot, aes(image = url), size = 0.05) +
  labs(title = "300 Play Moving Average QB Efficiency", subtitle = "Class 2017: Week 17, 2019",
       x = "Play", y = "EPA/Play",
       caption = "Play is defined as a dropback or QB Run \n Data from nflscrapR") +
  scale_x_continuous(limits = c(300, max(plot_data$dropback) + 25)) +
  scale_y_continuous(limits = c(-0.225, 0.6)) +
  guides(alpha = F, size = F) +
  geom_hline(yintercept = 0, color = "#c60c30", size = 0.35) +
  geom_hline(yintercept = quants$y, color = "black", linetype = 3) +
  geom_text(data = quants, aes(x = x, y = y,
                label = text), nudge_y = 0.005) #+
  #ggrepel::geom_text_repel(data = game_points, aes(x = dropback, y = rollingEPA300, label = defteam))
  geom_text(data = game_points, aes(x = dropback, y = rollingEPA300, label = defteam), nudge_y = 0.005, nudge_x = 10)#+
  geom_text(data = data.frame(handle = c("@BufBillsStats")), aes(label = handle, x = max(plot_data$dropback), y = 0.05), color = "#c60c30")

ggsave(filename = "~/Desktop/nflscrapR/Output/Class_2017_300_play_MA_wk17.jpg", width = 8, height = 5, units = "in")  


View(pbp_2019 %>%
  filter(wp<=0.5, wp >= 0.2, qtr == 4) %>%
  group_by(game_date, posteam, defteam) %>%
  summarise(plays = n())) %>%
  ungroup() %>%
  arrange(posteam)

#######################
View(reg_2019 %>%
  filter(defteam == "BUF", complete_pass == 1) %>%
  arrange(desc(air_yards)))


pbp_all %>%
  filter(passer_player_name == "J.Allen", posteam == "BUF", air_yards >= -5) %>%
  mutate(season = as.character(season)) %>%
  ggplot(aes(x = air_yards)) +
  geom_density(aes(color = season, fill = season), alpha = 0.5) +
  scale_fill_manual(values = c("#00338d", "#c60c30")) +
  scale_color_manual(values = c("#c60c30", "#00338d")) +
  theme_BUF() +
  labs(title = "Josh Allen Pass Distribution", subtitle = "2018 v 2019", x = "Target Depth", y = "",
       caption = "data via nflscrapR") +
  scale_x_continuous(breaks = seq(0,60,10)) +
  geom_density(data = pbp_all %>% filter(!is.na(air_yards), air_yards >= -5), aes(x = air_yards), color = "black")

league <- pbp_all %>%
  filter(!is.na(air_yards), !is.na(complete_pass)) %>%
  mutate(air_yards_to_sticks = air_yards - ydstogo) %>%
  group_by(air_yards_to_sticks, air_yards) %>%
  summarise(league_completion_pct = mean(complete_pass))

allen <- pbp_all %>%
  filter(!is.na(air_yards), !is.na(complete_pass), passer_player_name == "J.Allen", posteam == "BUF") %>%
  mutate(air_yards_to_sticks = air_yards - ydstogo) %>%
  group_by(air_yards_to_sticks, air_yards, passer_player_name) %>%
  summarise(completion_pct = mean(complete_pass)) %>% 
  left_join(league) %>%
  ungroup() %>%
  mutate(cpoe = completion_pct - league_completion_pct) %>%
  filter(air_yards_to_sticks >= -15, air_yards >= -5)

ggplot(data = allen) +
  geom_tile(aes(x = air_yards, y = air_yards_to_sticks, fill = cpoe)) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white")




quarterbacks <- pbp_test %>%
  group_by(passer_player_id, passer_player_name) %>%
  summarise(attempts = n()) %>%
  filter(attempts > 150, !is.na(passer_player_name))

qb_plays_all <- pbp_test %>%
  filter(play_type=="pass"|play_type == "run") %>%
  filter(qb_dropback == 1|rusher_player_id%in%quarterbacks$passer_player_id) %>%
  mutate(quarterback = ifelse(!is.na(passer_player_name), passer_player_name, rusher_player_name)) %>%
  mutate(quarterback = ifelse(quarterback == "Jos.Allen" & posteam == "BUF", "J.Allen", quarterback)) %>%
  select(-passer_player_id, -passer_player_name, -rusher_player_id, -rusher_player_name) %>%
  arrange(quarterback, season, week, desc(game_seconds_remaining)) %>%
  mutate(epa = ifelse(fumble_lost ==1&complete_pass==1, comp_air_epa, epa)) %>%
  mutate(epa = ifelse(epa < -4.5, 4.5, epa)) %>%
  filter(quarterback %in% quarterbacks$passer_player_name, !is.na(epa), !is.na(yards_gained), !is.na(epa)) #%>%
  mutate(adjYds = yards_gained + 20 * touchdown * complete_pass - 45 * interception)
  
  
load("~/Desktop/nflscrapR/Code/cpoe_model.rda")  
dak <- qb_plays_all %>% filter(quarterback == "D.Prescott")

qb_plays2019 <- pbp_all %>%
  filter(!is.na(epa), !is.na(posteam), play_type=="no_play"|play_type=="pass"|play_type=="run") %>%
  mutate(
    pass = if_else(str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0)
  ) %>%
  mutate(
    passer_player_name = ifelse(play_type == "no_play" & pass == 1, 
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((pass)|(sack)|(scramble)))"),
                                passer_player_name),
    receiver_player_name = ifelse(play_type == "no_play" & str_detect(desc, "pass"), 
                                  str_extract(desc, 
                                              "(?<=to\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"),
                                  receiver_player_name),
    rusher_player_name = ifelse(play_type == "no_play" & rush == 1, 
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((left end)|(left tackle)|(left guard)|		(up the middle)|(right guard)|(right tackle)|(right end)))"),
                                rusher_player_name),
    quarterback = ifelse(!is.na(passer_player_name), passer_player_name, rusher_player_name)
  ) %>%
  filter(season == 2019) %>%
  #filter(pass == 1|rush == 1) %>%
  filter(pass == 1|rusher_player_id%in%quarterbacks$passer_player_id) %>%
  #mutate(quarterback = ifelse(!is.na(passer_player_name), passer_player_name, rusher_player_name)) %>%
  mutate(quarterback = ifelse(quarterback == "Jos.Allen" & posteam == "BUF", "J.Allen", quarterback)) %>%
  select(-passer_player_id, -passer_player_name, -rusher_player_id, -rusher_player_name) %>%
  arrange(quarterback, season, week, desc(game_seconds_remaining)) %>%
  mutate(epa = ifelse(fumble_lost ==1&complete_pass==1, comp_air_epa, epa)) %>%
  mutate(epa = ifelse(epa < -4.5, -4.5, epa),
         air_is_zero = ifelse(air_yards == 0, 1, 0)) %>%
  filter(quarterback %in% quarterbacks$passer_player_name) 

passes_2019 <- qb_plays2019 %>%
  filter((complete_pass == 1|incomplete_pass == 1) & air_yards >= -5) %>%
  filter(!is.na(receiver_player_name)) %>%
  select(complete_pass, game_id, play_id, air_yards, pass_location, quarterback) %>% 
  mutate(air_is_zero = ifelse(air_yards == 0, 1, 0))

passes_2019$exp_comp <- predict(gam_y, passes_2019)


qb_plays2019 <- left_join(qb_plays2019, passes_2019)

qb_plays2019 <- qb_plays2019 %>%
  mutate(epa = ifelse(epa < -4.5, -4.5, epa),
         cpoe = complete_pass - exp_comp)

DAKOTA_2019 <- qb_plays2019 %>%
  group_by(quarterback) %>%
  summarise(plays = n(),
            total_epa = sum(epa, na.rm = T),
            epa_play = mean(epa, na.rm = T),
            CPOE = 100 * mean(cpoe, na.rm = T)) %>%
  mutate(DAKOTA = 0.2 * epa_play + .009 * CPOE + 0.08) %>%
  arrange(desc(DAKOTA)) %>%
  filter(plays >=140)


ma <- function(x, n = 1000){stats::filter(x, rep(1 / n, n), sides = 1)}

qb <- "D.Prescott"
qb_DAKOTA <- function(qb, data = qb_plays_all){
  #require(zoo)
  load("~/Desktop/nflscrapR/Code/cpoe_model.rda")
  qbdata <- filter(data, quarterback == qb)
  plays <- nrow(qbdata)
  qbdata$dropback = 1:plays
  
  passes <- qbdata %>%
    filter((complete_pass == 1|incomplete_pass == 1) & air_yards > -5) %>%
    filter(!is.na(air_yards), !is.na(pass_location), !is.na(receiver_player_name)) %>%
    select(complete_pass, game_id, play_id, air_yards, pass_location) %>% 
    mutate(air_is_zero = ifelse(air_yards == 0, 1, 0))
  pass_plays <- nrow(passes)
  passes$play = 1:pass_plays
  
  passes$exp_comp <- predict(gam_y, passes)
  passes <- passes %>%
    mutate(cpoe = complete_pass - exp_comp) %>%
    mutate(rollingCPOE300 = ifelse(play >=250, 100 * ma(cpoe, 250), NA))
  
  qbdata <- left_join(qbdata, passes) #%>%
    #mutate(cpoe = complete_pass - exp_comp) #%>%
    #mutate(cpoe = ifelse(is.na(cpoe), 0, cpoe))
  
  qbdata <- qbdata %>%
    mutate(epa = ifelse(epa < -4.5, -4.5, epa)) %>%
    mutate(rollingEPA300 = ifelse(dropback >= 300, ma(epa, 300), NA),
           rollingEPA500 = ifelse(dropback >= 500, ma(epa, 500), NA),
           rollingEPA1000 = ifelse(dropback >= 1000, ma(epa, 1000), NA),
           rollingEPA150 = ifelse(dropback >= 150, ma(epa, 150), NA)
           #rollingEPA = ma(epa, dropback - 50),
           # rollingAdjYds350 = ifelse(dropback >= 350, ma(adjYds, 350), NA),
           # rollingAdjYds500 = ifelse(dropback >= 500, ma(adjYds, 500), NA),
           # rollingAdjYds1000 = ifelse(dropback >= 1000, ma(adjYds), NA),
           # rollingAdjYds150 = ifelse(dropback >= 100, ma(adjYds, 150), NA),
           ) %>%
    fill(rollingCPOE300) %>%
    mutate(rollingDAKOTA = 0.20 * rollingEPA300 + 0.009 * rollingCPOE300 + 0.09)
  qb <- qbdata
  return(qb)
}

qb_plays_all <- pbp_all %>%
  filter(!is.na(epa), !is.na(posteam), play_type=="no_play"|play_type=="pass"|play_type=="run") %>%
  mutate(
    pass = if_else(str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0)
  ) %>%
  mutate(
    passer_player_name = ifelse(play_type == "no_play" & pass == 1, 
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((pass)|(sack)|(scramble)))"),
                                passer_player_name),
    receiver_player_name = ifelse(play_type == "no_play" & str_detect(desc, "pass"), 
                                  str_extract(desc, 
                                              "(?<=to\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"),
                                  receiver_player_name),
    rusher_player_name = ifelse(play_type == "no_play" & rush == 1, 
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((left end)|(left tackle)|(left guard)|		(up the middle)|(right guard)|(right tackle)|(right end)))"),
                                rusher_player_name),
    quarterback = ifelse(!is.na(passer_player_name), passer_player_name, rusher_player_name)
  ) %>%
  #filter(pass == 1|rush == 1) %>%
  filter(pass == 1|rusher_player_id%in%quarterbacks$passer_player_id) %>%
  #mutate(quarterback = ifelse(!is.na(passer_player_name), passer_player_name, rusher_player_name)) %>%
  mutate(quarterback = ifelse(quarterback == "Jos.Allen" & posteam == "BUF", "J.Allen", quarterback)) %>%
  select(-passer_player_id, -passer_player_name, -rusher_player_id, -rusher_player_name) %>%
  arrange(quarterback, season, week, desc(game_seconds_remaining)) %>%
  mutate(epa = ifelse(fumble_lost ==1&complete_pass==1, comp_air_epa, epa)) %>%
  mutate(epa = ifelse(epa < -4.5, -4.5, epa),
         air_is_zero = ifelse(air_yards == 0, 1, 0)) %>%
  filter(quarterback %in% quarterbacks$passer_player_name) 

passers <- quarterbacks$passer_player_name


#NFLcolors <- read_csv("footballcolors.csv")
load("~/Desktop/nflscrapR/Code/cpoe_model.rda")
DAKOTA_averages <- map_dfr(passers, qb_DAKOTA)


QBs_2018 <- c("B.Mayfield", "J.Allen", "L.Jackson", "S.Darnold")
QBs_2017 <- c("M.Trubisky", "P.Mahomes", "D.Watson")
QBs_2016 <- c("C.Wentz", "J.Goff", "D.Prescott")
QBs_2019 <- c("K.Murray", "D.Jones", "G.Minshew")

plot_data <- DAKOTA_averages %>%
  filter(quarterback %in% QBs_2016) %>%
  #mutate(posteam = if_else(posteam == "ARI", "MIA", posteam)) %>%
  left_join(nflteams, by = c("posteam" = "abbr")) %>%
  group_by(quarterback) %>%
  mutate(alpha = dropback / max(dropback)) %>%
  ungroup()

date_data <- plot_data %>%
  mutate(season = lubridate::year(game_date), month = month(game_date)) %>%
  filter(month != 1) %>%
  group_by(quarterback, season) %>%
  slice(1)
game_points <- plot_data %>%
  mutate(season = lubridate::year(game_date), month = month(game_date)) %>%
  #filter(month != 1) %>%
  group_by(quarterback, season, game_id) %>%
  slice(1)
# %>%
#   ungroup()%>%
#   mutate(month = month(month))

color_data <- plot_data %>%
  arrange(quarterback, desc(dropback)) %>%
  distinct(quarterback, .keep_all = TRUE) %>%
  arrange(quarterback)

logo_plot <- plot_data %>%
  group_by(quarterback) %>%
  top_n(1, dropback) %>%
  #select(256:273) %>%
  left_join(nfl_logos_df)

source("~/Desktop/nflscrapR/Code/theme_BUF.R")
cols = color_data$primary
cols[3] = color_data$secondary[3]
cols2 = color_data$secondary
cols[3] = color_data$primary[3]


quantile(DAKOTA_averages$rollingDAKOTA, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = T)

quants <- tibble(x = c(300,300,300,300,300),
                 y = c(0.05999826, 0.08994015, 0.12484444, 0.16182765, 0.19362955),
                 text = c("10%", "25%", "50%", "75%", "90%"))


ggplot(data = plot_data, aes(x = dropback, y = rollingDAKOTA)) +
  geom_line(aes(color = quarterback, alpha = alpha), size = 1) +
  geom_point(data = date_data, aes(color = quarterback), size = 3) +
  geom_text(data = date_data, aes(label = season, color = quarterback),  
            nudge_y = 0.002) +
  geom_point(data = game_points, aes(), size = .75, color = "black", fill = "black") +
  theme_BUF() +
  #transition_reveal(dropback) +
  theme(legend.position = c(0.9, .6), legend.direction = "vertical", legend.background = element_blank(),
        legend.title = element_blank()) +
  scale_color_manual(values = cols) +
  geom_image(data = logo_plot, aes(image = url), size = 0.05) +
  labs(title = "300 Play Moving Average - DAKOTA", subtitle = "Class 2016 Quarterbacks", 
       x = "Play", y = "DAKOTA",
       caption = "Play is defined as a dropback or QB Run \n Data from nflscrapR") +
  scale_x_continuous(limits = c(300, max(plot_data$dropback) + 25)) +
  guides(alpha = F, size = F) +
  #geom_hline(yintercept = 0, color = "#c60c30", size = 0.35) +
  geom_hline(yintercept = quants$y, color = "black", linetype = 3) +
  geom_text(data = quants, aes(x = x, y = y,
                               label = text), nudge_y = 0.00075) #+
#geom_text(data = data.frame(handle = c("@BufBillsStats")), aes(label = handle, x = max(plot_data$dropback), y = 0.05), color = "#c60c30")

ggsave(filename = "~/Desktop/nflscrapR/Output/Class_2018_300_play_DAK_wk10.jpg", width = 8, height = 5, units = "in") 



