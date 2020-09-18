library(tidyverse)
library(nflfastR)
library(nflscrapR)
library(glue)

input_dir <- "~/Documents/nflscrapR/Data/"

seasons <- c(2014:2019)
pbp_all <- map_dfr(seasons, function(x)read_rds(glue::glue("{input_dir}pbp_{x}.rds"))) %>%
  mutate(air_yards = case_when(
             penalty_type == "Defensive Pass Interference" ~ penalty_yards,
             TRUE ~ air_yards
           ))

game_numbers <- pbp_all %>%
  filter(pass == 1, season_type == "REG", sack == 0) %>%
  arrange(game_date, game_id, -game_seconds_remaining) %>%
  group_by(posteam, season, game_id, week) %>%
  summarise(plays = n()) %>%
  ungroup() %>%
  group_by(posteam, season) %>%
  mutate(game_number = rank(week)) %>%
  ungroup() %>%
  select(-plays)

pbp_sample_1 <- pbp_all %>%
  filter(pass == 1, season_type == "REG", sack == 0) %>%
  left_join(game_numbers) %>%
  filter(game_number %in% c(1:5))

pbp_sample_2 <- pbp_all %>%
  filter(pass == 1, season_type == "REG", sack == 0) %>%
  left_join(game_numbers) %>%
  filter(game_number %in% c(6:10))

pbp_sample_3 <- pbp_all %>%
  filter(pass == 1, season_type == "REG", sack == 0) %>%
  left_join(game_numbers) %>%
  filter(game_number %in% c(11:15))

make_team_stats <- function(pbp, sample_set){
  data <- pbp %>%
    group_by(posteam, season) %>%
    summarise(Attempts = n(),
              Completions = sum(complete_pass, na.rm = T),
              TeamYards = sum(yards_gained, na.rm = T),
              TeamEPA = sum(epa, na.rm = T),
              TeamAirYards = sum(air_yards, na.rm = T),
              TeamTDs = sum(touchdown, na.rm = T),
              TeamFirstDowns = sum(first_down, na.rm = T),
              RedZonePlays = sum(yardline_100 <= 20, na.rm = T),
              EndZoneTargets = sum(air_yards >= yardline_100, na.rm = T))
  
  #new_names <- str_c(colnames(data)[3:9], glue::glue("_{sample_set}"))
  #colnames(data)[3:9] <- new_names
  
  return(data)
}

make_player_stats <- function(pbp, sample_set) {

  data <- pbp %>%
    group_by(receiver, receiver_id, posteam, season) %>%
    summarise(PlayerYards = sum(yards_gained, na.rm = T),
              Receptions = sum(complete_pass, na.rm = T),
              PlayerTDs = sum(touchdown, na.rm = T),
              PlayerEPA = sum(epa, na.rm = T),
              PlayerFirstDowns = sum(first_down, na.rm = T),
              Targets = n(),
              PlayerAirYards = sum(air_yards, na.rm = T),
              PlayerRedZoneTargets = sum(yardline_100 <= 20, na.rm = T),
              PlayerEndZoneTargets = sum(air_yards >= yardline_100, na.rm = T),
              games = length(unique(game_id))) %>%
    filter(Targets >= 15, !is.na(receiver), PlayerAirYards > 15, games >= 3) %>%
    mutate(EPA_per_catch = PlayerEPA / Receptions,
           Yards_per_target = PlayerYards / Targets,
           FirstDown_rate = PlayerFirstDowns / Targets) %>%
    mutate(fantasy_points = 0.1 * PlayerYards + 0.5 * Receptions + 6 * PlayerTDs)
  
  #new_names <- str_c(colnames(data)[5:18], glue::glue("_{sample_set}"))
  #colnames(data)[5:18] <- new_names
  return(data)
}

team_stats_1 <- make_team_stats(pbp_sample_1, 1)
player_stats_1 <- make_player_stats(pbp_sample_1, 1)

team_stats_2 <- make_team_stats(pbp_sample_2, 2)
player_stats_2 <- make_player_stats(pbp_sample_2, 2)

team_stats_3 <- make_team_stats(pbp_sample_3, 3)
player_stats_3 <- make_player_stats(pbp_sample_3, 3)

merge_player_team <- function(player_data, team_data, sample_set = 1){
  data<- player_data %>%
    left_join(team_data) %>%
    mutate(TargetShare = Targets / Attempts,
           YardageShare = PlayerYards / TeamYards,
           AirYardShare = PlayerAirYards / TeamAirYards,
           RedZoneShare = PlayerRedZoneTargets / RedZonePlays,
           TDshare = PlayerTDs / TeamTDs,
           EndZoneShare = PlayerEndZoneTargets / EndZoneTargets,
           FirstDownShare = PlayerFirstDowns / TeamFirstDowns) %>%
    mutate(Dominator = 100 * (2/3 * YardageShare + 1/3 * TDshare))
  return(data)
}

sample_1 <- merge_player_team(player_stats_1, team_stats_1) %>%
  left_join(player_stats_2 %>% select(receiver, receiver_id, season, fantasy_points, games) %>%
              mutate(fan_ppg = fantasy_points / games) %>% select(-fantasy_points, -games))
sample_2 <- merge_player_team(player_stats_2, team_stats_2) %>%
  left_join(player_stats_3 %>% select(receiver, receiver_id, season, fantasy_points, games) %>%
              mutate(fan_ppg = fantasy_points / games) %>% select(-fantasy_points, -games))

sample_3 <- merge_player_team(player_stats_3, team_stats_3)

train_sample <- bind_rows(sample_1, sample_2) %>%
  arrange(receiver, season) %>%
  filter(!is.na(fan_ppg), PlayerAirYards >= 100)

test_sample <- bind_rows(sample_1, sample_2) %>%
  arrange(receiver, season) %>%
  filter(!is.na(fan_ppg), season == 2019)

#View(cor(train_sample[,5:36]))

wr_ids <- read_csv("~/Documents/nflscrapR/Data/id_database.csv") %>%
  select(pos, id_nfl_new)
train_sample <- train_sample %>%
  left_join(wr_ids, by = c("receiver_id" = "id_nfl_new")) %>%
  filter(pos == "WR")
test_sample <- test_sample %>%
  left_join(wr_ids, by = c("receiver_id" = "id_nfl_new")) %>%
  filter(pos == "WR")

require(pls)

pcr_model <- pcr(fan_ppg ~., data = train_sample[,c("PlayerFirstDowns", "PlayerAirYards", "PlayerRedZoneTargets", "AirYardShare", "Targets", "TargetShare", "fan_ppg")],
                 scale = TRUE, validation = "CV")
summary(pcr_model)
# Plot the root mean squared error
validationplot(pcr_model)
validationplot(pcr_model, val.type="MSEP")
validationplot(pcr_model, val.type = "R2")
predplot(pcr_model)
coefplot(pcr_model)

test_sample$pcr_pred <- predict(pcr_model, test_sample, ncomp = 2)
cor(test_sample$pcr_pred, test_sample$fan_ppg)^2


library(splines)
model_1 <- glm(fan_ppg ~ bs(Targets) + bs(TargetShare) + bs(sqrt(PlayerAirYards)) + bs(AirYardShare) +
                       bs(PlayerRedZoneTargets), 
                     family = "Gamma",
                     data = train_sample)
summary(model_1)
plot(model_1,pages=1)
gam.check(model_1,pch=19,cex=.3)
gam.check(model_1)

model_2 <- nnet(formula = fan_ppg ~ Targets + TargetShare + PlayerAirYards + AirYardShare +
                  PlayerRedZoneTargets + PlayerFirstDowns,
                data = train_sample,
                size = 5)
summary(model_2)
#model_2 <- lm(fan_ppg ~ bs(Targets) + bs(TargetShare) + bs(PlayerAirYards) + bs(AirYardShare) + 
#                       bs(PlayerRedZoneTargets) + bs(PlayerFirstDowns) + FirstDown_rate, data = train_sample)
#summary(model_2)

test_sample$projected <- 1 / predict(model_1, test_sample)
cor(test_sample$fan_ppg, test_sample$projected) ^2

test_sample$projected <- predict(model_2, test_sample, type = "class")
cor(test_sample$fan_ppg, test_sample$projected) ^2

ggplot(data = test_sample, aes(x = projected, y = fan_ppg)) +
  geom_point() +
  theme_bw() +
  geom_smooth() +
  scale_x_continuous(breaks = seq(0,20,5), limits = c(5,20)) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20))

saveRDS(ROC, "~/Documents/nflscrapR/models/ROC_old.rds")
saveRDS(model_1, "~/Documents/nflscrapR/models/ROC.rds")
ROC_model <- pcr_model
save(ROC_model, file = "~/Documents/nflscrapR/shiny/ROC_app/ROC_model.rda")





