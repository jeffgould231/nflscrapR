

make_team_stats <- function(pbp, weeks = c(1:16), szn = 2019){
  data <- pbp %>%
    filter(play_type == "pass", week %in% weeks, pass_attempt == 1, season %in% szn, !is.na(air_yards)) %>%
    group_by(posteam, season) %>%
    summarise(Attempts = n(),
              Completions = sum(complete_pass),
              TeamYards = sum(yards_gained, na.rm = T),
              TeamEPA = sum(epa, na.rm = T),
              TeamAirYards = sum(air_yards, na.rm = T),
              TeamTDs = sum(touchdown),
              TeamFirstDowns = sum(first_down),
              RedZonePlays = sum(yardline_100 <= 20),
              EndZoneTargets = sum(air_yards >= yardline_100, na.rm = T),
              TeamGames = length(unique(game_id)))
  
  return(data)
}

make_player_stats <- function(pbp, weeks = c(1:16), szn = 2019) {
  
  data <- pbp %>%
    filter(play_type == "pass", week %in% weeks, pass_attempt == 1, season %in% szn) %>%
    group_by(receiver, receiver_id, posteam, season, teamPlayers.position) %>%
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
    filter(!is.na(receiver)) %>%
    mutate(EPA_per_catch = PlayerEPA / Receptions,
           Yards_per_target = PlayerYards / Targets,
           FirstDown_rate = PlayerFirstDowns / Targets) %>%
    mutate(fantasy_points = 0.1 * PlayerYards + 0.5 * Receptions + 6 * PlayerTDs)
  
  return(data)
}

merge_player_team <- function(player_data, team_data){
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

# data <- make_ROC(c(1:10), pbp_all) %>% filter(TeamGames == 10)
# for(i in 2:10){
#   data <- bind_rows(data, make_ROC(c(i:(i+9)), pbp_all) %>% filter(TeamGames == 10))
# }

make_ROC <- function(weeks, pbp_data, positions = c("RB", "WR", "TE"), reception_min = 10, szn = 2019){
  
  load("ROC_model.rda")
  
  team_stats <- make_team_stats(pbp_data, weeks = weeks, szn)
  player_stats <- make_player_stats(pbp_data, weeks = weeks, szn)
  
  joined_data <- merge_player_team(player_stats, team_stats) %>%
    mutate(Targets = Targets * (5 / TeamGames),
           PlayerAirYards = PlayerAirYards * (5 / TeamGames),
           PlayerRedZoneTargets = PlayerRedZoneTargets * (5 / TeamGames),
           PlayerFirstDowns = PlayerFirstDowns * (5 / TeamGames)
    ) %>%
    ungroup() %>%
    filter(Receptions >= reception_min)
  
  joined_data$projected = 1 / predict(ROC_model, joined_data)
  
  # joined_data <- joined_data %>%
  #   mutate(ROC = case_when(
  #     TeamGames %in% c(1) ~ scales::rescale(projected, to = c(0,100), from = c(4,60)),
  #     TeamGames %in% c(2) ~ scales::rescale(projected, to = c(0,100), from = c(4,34)),
  #     TeamGames %in% c(3) ~ scales::rescale(projected, to = c(0,100), from = c(4,25)),
  #     TeamGames %in% c(4) ~ scales::rescale(projected, to = c(0,100), from = c(4,21)),
  #     TeamGames %in% c(5,6,7) ~ scales::rescale(projected, to = c(0,100), from = c(4,19)),
  #     TeamGames %in% c(8) ~ scales::rescale(projected, to = c(0,100), from = c(4,18)),
  #     TeamGames >= 9 ~ scales::rescale(projected, to = c(0,100), from = c(4,18))
  #   ))
  joined_data$ROC = scales::rescale(joined_data$projected, to = c(10,100), from = c(2,21))
  
  
  joined_data <- joined_data %>%
    mutate(Targets = Targets * (TeamGames / 5),
           PlayerAirYards = PlayerAirYards * (TeamGames / 5),
           PlayerRedZoneTargets = PlayerRedZoneTargets * (TeamGames / 5),
           PlayerFirstDowns = PlayerFirstDowns * (TeamGames / 5)) %>%
    mutate_if(is.numeric, round, 2) %>%
    filter(teamPlayers.position %in% positions)
  
  return(joined_data)
}

get_ROC_sells <- function(data, weeks, n = 7){
  require(kableExtra)
  
  sells <- data %>%
    filter(teamPlayers.position != "RB") %>%
    select(receiver, posteam, season, ROC, fantasy_points, projected, TeamGames, games) %>%
    mutate(fantasy_point_diff = fantasy_points  - projected * TeamGames,
           fantasy_percent_diff = round(100 * fantasy_point_diff / fantasy_points, 2),
           projected = projected * TeamGames) %>%
    top_n(20, fantasy_point_diff) %>%
    top_n(n, fantasy_percent_diff) %>%
    arrange(desc(fantasy_percent_diff)) %>%
    rename("Player" = receiver, Team = posteam, Season = season, 
           `Fantasy Points` = fantasy_points, `Exp. Points` = projected,
           `Points Over Exp` = fantasy_point_diff, `%-Over Exp` = fantasy_percent_diff) %>%
    select(-games, -TeamGames)
  
  # buys <- data %>%
  #   filter(projected >= 8, teamPlayers.position != "RB") %>%
  #   select(receiver, posteam, season, ROC, fantasy_points, projected, TeamGames, games) %>%
  #   mutate(fantasy_point_diff = fantasy_points  - projected * TeamGames,
  #          fantasy_percent_diff = round(100 * fantasy_point_diff / fantasy_points, 2),
  #          projected = projected * TeamGames) %>%
  #   top_n(20, -fantasy_point_diff) %>%
  #   top_n(n, -fantasy_percent_diff) %>%
  #   arrange(fantasy_percent_diff) %>%
  #   rename("Player" = receiver, Team = posteam, Season = season, 
  #          `Fantasy Points` = fantasy_points, `Exp. Points` = projected,
  #          `Points Under Expected` = fantasy_point_diff, `%-Under Expected` = fantasy_percent_diff) %>%
  #   select(-games, -TeamGames)
  
  
  return(
    sells %>%
      #cbind(buys) %>%
      knitr::kable() %>%
      kable_styling(c("striped", "hover"), full_width = F) %>%
      add_header_above(c("Over Producers" = 8), align = "l")
  )
  
}

get_ROC_buys <- function(data, weeks, n = 7){
  require(kableExtra)
  
  # sells <- data %>%
  #   filter(teamPlayers.position != "RB") %>%
  #   select(receiver, posteam, season, ROC, fantasy_points, projected, TeamGames, games) %>%
  #   mutate(fantasy_point_diff = fantasy_points  - projected * TeamGames,
  #          fantasy_percent_diff = round(100 * fantasy_point_diff / fantasy_points, 2),
  #          projected = projected * TeamGames) %>%
  #   top_n(20, fantasy_point_diff) %>%
  #   top_n(n, fantasy_percent_diff) %>%
  #   arrange(desc(fantasy_percent_diff)) %>%
  #   rename("Player" = receiver, Team = posteam, Season = season, 
  #          `Fantasy Points` = fantasy_points, `Exp. Points` = projected,
  #          `Points Over Expected` = fantasy_point_diff, `%-Over Expected` = fantasy_percent_diff) %>%
  #   select(-games, -TeamGames)
  
  buys <- data %>%
    filter(projected >= 8, teamPlayers.position != "RB") %>%
    select(receiver, posteam, season, ROC, fantasy_points, projected, TeamGames, games) %>%
    mutate(fantasy_point_diff = fantasy_points  - projected * TeamGames,
           fantasy_percent_diff = round(100 * fantasy_point_diff / fantasy_points, 2),
           projected = projected * TeamGames) %>%
    top_n(20, -fantasy_point_diff) %>%
    top_n(n, -fantasy_percent_diff) %>%
    arrange(fantasy_percent_diff) %>%
    rename("Player" = receiver, Team = posteam, Season = season, 
           `Fantasy Points` = fantasy_points, `Exp. Points` = projected,
           `Points Under Exp` = fantasy_point_diff, `%-Under Exp` = fantasy_percent_diff) %>%
    select(-games, -TeamGames)
  
  
  return(
    buys %>%
      #cbind(buys) %>%
      knitr::kable() %>%
      kable_styling(c("striped", "hover"), full_width = F) %>%
      add_header_above(c("Under Producers" = 8), align = "l")
  )
  
}


ROC_table <- function(data, teams){
  
  if(teams != "All"){data <- data %>% filter(posteam == teams)}
  return(data %>%
           select("Receiver" = receiver, "Team" = posteam, "Season" = season, ROC, "Fantasy Points" = fantasy_points,
                  "Yards" = PlayerYards,
                  Receptions, "TDs" = PlayerTDs, Targets, "Target Share" = TargetShare, "Air Yards" = PlayerAirYards, 
                  "Air Yard Share" = AirYardShare, "Yards/Target" = Yards_per_target, "First Downs" = PlayerFirstDowns,
                  "Red Zone Targets" = PlayerRedZoneTargets, Dominator) %>%
           arrange(desc(ROC)))
  
  
}
