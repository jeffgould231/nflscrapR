library(tidyverse)
library(nflfastR)
library(nflscrapR)
library(lubridate)
library(ggimage)
setwd("~/Documents/nflscrapR/")
team_ids <- teams_colors_logos %>% pull(team_id)

# quarterbacks <- nflfastR::fast_scraper_roster(team_ids, as.character(c(2001:2019)), pp = T) %>%
#   filter(teamPlayers.position == "QB") %>% 
#   mutate_at(vars(team.abbr), funs(case_when(
#     . %in% "JAC" ~ "JAX",
#     . %in% "STL" ~ "LA",
#     . %in% "SL" ~ "LA",
#     . %in% "ARZ" ~ "ARI",
#     . %in% "BLT" ~ "BAL",
#     . %in% "CLV" ~ "CLE",
#     . %in% "HST" ~ "HOU",
#     . %in% "SD" ~ "LAC",
#     TRUE ~ .
#   ))) 
# saveRDS(quarterbacks, "Data/QBsAll.rds")

quarterbacks <- readRDS("Data/QBsAll.rds") %>%
  mutate(name = str_c(str_sub(teamPlayers.firstName, 1, 1), teamPlayers.lastName, sep = ".")) %>%
  select(team.season, team.abbr, teamPlayers.position, teamPlayers.firstName, 
         teamPlayers.lastName, name, teamPlayers.gsisId) %>%
  distinct()

pbp_all <- readRDS("Data/pbp_all.rds")

penalty_list <- c(
  NA, "Offensive Pass Interference", "Defensive Pass Interference","Defensive Holding", "Roughing the Passer",
                  "Offensive Holding", "Defensive Offside", "Personal Foul", "Encroachment", "Intentional Grounding",
                  "Unnecessary Roughness", "Ineligible Downfield Pass", "Neutral Zone Infraction", "Defensive Too Many Men on Field"
                  )

pbp_work <- pbp_all %>%
  distinct() %>%
  filter((pass == 1 | rush == 1), 
         !is.na(epa), 
         name %in% quarterbacks$name, 
         game_type == "REG") %>%
  left_join(quarterbacks, by = c("name" = "name", "posteam" = "team.abbr", "season" = "team.season")) %>%
  filter(teamPlayers.position == "QB") %>%
  mutate(epa = ifelse(complete_pass == 1 & fumble_lost == 1, comp_air_epa, epa)) %>%
  rename(Id = teamPlayers.gsisId) %>%
  arrange(name, Id, game_date, desc(game_seconds_remaining))

# quarterbacks_work <- pbp_work %>% 
#   group_by(name, Id) %>% 
#   summarise(passes = sum(pass_attempt, na.rm = T)) %>% 
#   filter(passes >= 300) %>%
#   select(name, Id)

pbp_work <- pbp_work %>%
  group_by(name, Id) %>%
  mutate(rolling_epa = zoo::rollapply(epa, 300, mean, na.rm = T, align = "right", fill = NA),
         rolling_cpoe = zoo::rollapply(cpoe, 300, mean, na.rm = T, align = "right", fill = NA),
         play = row_number()) %>%
  ungroup() %>%
  mutate(DAKOTA = 0.20 * rolling_epa + 0.009 * rolling_cpoe + 0.09)

pbp_work <- pbp_work %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))


rolling_epa_graph <- function(plot_quarterbacks, Subtitle = NULL){
  
  chart_data <- pbp_work %>%
    filter(name %in% plot_quarterbacks, !is.na(rolling_epa), play %% 5 == 0) %>%
    group_by(name) %>%
    mutate(alpha = play / max(play)) %>%
    ungroup() %>%
    mutate(plot_color = ifelse(posteam %in% c("LA", "NE", "DEN", "CHI"), team_color2, team_color))
  
  logo_plot <- chart_data %>%
    group_by(name, posteam) %>%
    top_n(1, play) 
  
  date_data <- chart_data %>%
    group_by(name, season) %>%
    slice(1)
  
  game_points <- chart_data %>%
    group_by(name, season, game_id) %>%
    slice(1)
  
  quantiles <- tibble(x = 1.1 * rep(max(chart_data$play), 5),
                      y = quantile(pbp_work$rolling_epa, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = T),
                      text = c("10%", "25%", "50%", "75%", "90%"))
  
  graph <- ggplot(data = chart_data, aes(x = play, y = rolling_epa)) +
    geom_hline(yintercept = 0, color = "#c60c30", size = 0.35) +
    geom_line(aes(color = plot_color, alpha = alpha), size = 1, show.legend = F) +
    scale_color_identity() +
    geom_point(data = date_data, aes(color = team_color), size = 3) +
    ggrepel::geom_text_repel(data = date_data, aes(label = season),  
              color = "black", nudge_y = 0.02)+
    geom_point(data = game_points, aes(), size = .75, color = "black", fill = "black") +
    theme_BUF() +
    geom_image(data = logo_plot, aes(image = team_logo_espn), size = 0.05) +
    labs(title = "300 Play Moving Average QB Efficiency",
         x = "Play", y = "EPA/Play",
         caption = "@BufBillsStats
         Data from nflfastR",
         subtitle = glue::glue("{Subtitle}")) +
    scale_x_continuous(limits = c(300, 1.1 * max(chart_data$play))) +
    scale_y_continuous(limits = c(min(-0.2, min(chart_data$rolling_epa, na.rm = T)), 
                                  max(0.5, max(chart_data$rolling_epa, na.rm = T)))) +
    geom_hline(yintercept = quantiles$y, color = "black", linetype = 3) +
    geom_text(data = quantiles, aes(x = x, y = y,
                                 label = text), nudge_y = 0.005) 
  
  
  return(graph)
}

rolling_DAK_graph <- function(plot_quarterbacks, Subtitle = NULL){
  
  chart_data <- pbp_work %>%
    filter(name %in% plot_quarterbacks, !is.na(DAKOTA), play %% 5 == 0) %>%
    group_by(name) %>%
    mutate(alpha = play / max(play)) %>%
    ungroup() %>%
    mutate(plot_color = ifelse(posteam %in% c("LA", "NE", "DEN", "CHI"), team_color2, team_color))
  
  logo_plot <- chart_data %>%
    group_by(name, posteam) %>%
    top_n(1, play) 
  
  date_data <- chart_data %>%
    group_by(name, season) %>%
    slice(1)
  
  game_points <- chart_data %>%
    group_by(name, season, game_id) %>%
    slice(1)
  
  quantiles <- tibble(x = 1.1* rep(max(chart_data$play), 5),
                      y = quantile(pbp_work$DAKOTA, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = T),
                      text = c("10%", "25%", "50%", "75%", "90%"))
  
  graph <- ggplot(data = chart_data, aes(x = play, y = DAKOTA)) +
    geom_hline(yintercept = 0, color = "#c60c30", size = 0.35) +
    geom_line(aes(color = plot_color, alpha = alpha), size = 1, show.legend = F) +
    scale_color_identity() +
    geom_point(data = date_data, aes(color = team_color), size = 3) +
    ggrepel::geom_text_repel(data = date_data, aes(label = season),  
              color = "black", nudge_y = 0.02)+
    geom_point(data = game_points, aes(), size = .75, color = "black", fill = "black") +
    theme_BUF() +
    geom_image(data = logo_plot, aes(image = team_logo_espn), size = 0.05) +
    labs(title = "300 Play Moving Average QB Efficiency",
         x = "Play", y = "DAKOTA",
         caption = "@BufBillsStats
         Data from nflfastR",
         subtitle = glue::glue("{Subtitle}")) +
    scale_x_continuous(limits = c(300, 1.1 * max(chart_data$play))) +
    scale_y_continuous(limits = c(min(0, min(chart_data$DAKOTA, na.rm = T)), 
                                  max(0.3, max(chart_data$DAKOTA, na.rm = T)))) +
    geom_hline(yintercept = quantiles$y, color = "black", linetype = 3) +
    geom_text(data = quantiles, aes(x = x, y = y,
                                    label = text), nudge_y = 0.005)

  return(graph)
  
  
  
}

rolling_epa_facet <- function(plot_quarterbacks, 
                              first_season = NULL, 
                              last_season = NULL,
                              Subtitle = NULL){
  
  chart_data <- pbp_work %>%
    filter(name %in% plot_quarterbacks)
  
  if(is.null(first_season)){first_season <- min(chart_data$season)}
  if(is.null(last_season)){last_season <- max(chart_data$season)}
  
  chart_data <- chart_data %>%
    group_by(name, season) %>%
    mutate(play = row_number(),
           alpha = 0.9) %>%
    ungroup() %>%
    filter(season >= first_season, season <= last_season,
           !is.na(rolling_epa), play %% 3 == 0) %>%
    mutate(plot_color = ifelse(posteam %in% c("LA", "NE", "DEN", "CHI", "LAC"), team_color2, team_color))
  
  logo_plot <- chart_data %>%
    group_by(name, posteam, season) %>%
    top_n(1, play) 
  
  date_data <- chart_data %>%
    group_by(name, season) %>%
    slice(1)
  
  game_points <- chart_data %>%
    group_by(name, season, game_id) %>%
    slice(1)
  
  quantiles <- tibble(x = 1.1* rep(max(chart_data$play), 5),
                      y = quantile(pbp_work$rolling_epa, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = T),
                      text = c("10%", "25%", "50%", "75%", "90%"))
  
  graph <- ggplot(data = chart_data, aes(x = play, y = rolling_epa)) + 
    facet_wrap(~season, nrow = 2) +
    geom_hline(yintercept = 0, color = "#c60c30", size = 0.35) +
    geom_line(aes(color = plot_color, alpha = alpha), size = 1, show.legend = F) +
    scale_color_identity() +
    geom_point(data = game_points, aes(), size = .75, color = "black", fill = "black") +
    theme_BUF() +
    theme(strip.background = element_blank()) +
    geom_image(data = logo_plot, aes(image = team_logo_espn), size = 0.15) +
    labs(title = "300 Play Moving Average QB Efficiency",
         x = "Play", y = "EPA/Play",
         caption = "@BufBillsStats
         Data from nflfastR",
         subtitle = glue::glue("{Subtitle}")) +
    scale_x_continuous(limits = c(0, 1.1 * max(chart_data$play))) +
    scale_y_continuous(limits = c(min(0, min(chart_data$rolling_epa, na.rm = T)), 
                                  max(0.3, max(chart_data$rolling_epa, na.rm = T)))) +
    geom_hline(yintercept = quantiles$y, color = "black", linetype = 3) +
    geom_text(data = quantiles, aes(x = x, y = y,
                                    label = text), nudge_y = 0.005)
  
  return(graph)
}

plot_quarterbacks <- c("P.Manning", "T.Romo")

rolling_DAK_facet <- function(plot_quarterbacks, 
                              first_season = NULL, 
                              last_season = NULL,
                              Subtitle = NULL){
  
  chart_data <- pbp_work %>%
    filter(name %in% plot_quarterbacks)
  
  if(is.null(first_season)){first_season <- min(chart_data$season)}
  if(is.null(last_season)){last_season <- max(chart_data$season)}
  
  chart_data <- chart_data %>%
    group_by(name, season) %>%
    mutate(play = row_number(),
           alpha = 0.9) %>%
    ungroup() %>%
    filter(season >= first_season, season <= last_season,
           !is.na(DAKOTA), play %% 3 == 0) %>%
    mutate(plot_color = ifelse(posteam %in% c("LA", "NE", "DEN", "CHI"), team_color2, team_color))
  
  logo_plot <- chart_data %>%
    group_by(name, posteam, season) %>%
    top_n(1, play) 
  
  date_data <- chart_data %>%
    group_by(name, season) %>%
    slice(1)
  
  game_points <- chart_data %>%
    group_by(name, season, game_id) %>%
    slice(1)
  
  quantiles <- tibble(x = 1.1* rep(max(chart_data$play), 5),
                      y = quantile(pbp_work$DAKOTA, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = T),
                      text = c("10%", "25%", "50%", "75%", "90%"))
  
  graph <- ggplot(data = chart_data, aes(x = play, y = DAKOTA)) + 
    facet_wrap(~season, nrow = 2) +
    geom_hline(yintercept = 0, color = "#c60c30", size = 0.35) +
    geom_line(aes(color = plot_color, alpha = alpha), size = 1, show.legend = F) +
    scale_color_identity() +
    geom_point(data = game_points, aes(), size = .75, color = "black", fill = "black") +
    theme_BUF() +
    theme(strip.background = element_blank()) +
    geom_image(data = logo_plot, aes(image = team_logo_espn), size = 0.15) +
    labs(title = "300 Play Moving Average QB Efficiency",
         x = "Play", y = "DAKOTA",
         caption = "@BufBillsStats
         Data from nflfastR",
         subtitle = glue::glue("{Subtitle}")) +
    scale_x_continuous(limits = c(0, 1.1 * max(chart_data$play))) +
    scale_y_continuous(limits = c(min(0, min(chart_data$DAKOTA, na.rm = T)), 
                                  max(0.3, max(chart_data$DAKOTA, na.rm = T)))) +
    geom_hline(yintercept = quantiles$y, color = "black", linetype = 3) +
    geom_text(data = quantiles, aes(x = x, y = y,
                                    label = text), nudge_y = 0.005)
  
  return(graph)
}

rolling_cpoe_graph <- function(plot_quarterbacks, Subtitle = NULL){
  
  chart_data <- pbp_work %>%
    filter(name %in% plot_quarterbacks, !is.na(rolling_cpoe), play %% 5 == 0) %>%
    group_by(name) %>%
    mutate(alpha = play / max(play)) %>%
    ungroup() %>%
    mutate(plot_color = ifelse(posteam %in% c("LA", "NE", "DEN", "CHI"), team_color2, team_color))
  
  logo_plot <- chart_data %>%
    group_by(name, posteam) %>%
    top_n(1, play) 
  
  date_data <- chart_data %>%
    group_by(name, season) %>%
    slice(1)
  
  game_points <- chart_data %>%
    group_by(name, season, game_id) %>%
    slice(1)
  
  quantiles <- tibble(x = rep(1.1 * max(chart_data$play), 5),
                      y = quantile(pbp_work$rolling_cpoe, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = T),
                      text = c("10%", "25%", "50%", "75%", "90%"))
  
  graph <- ggplot(data = chart_data, aes(x = play, y = rolling_cpoe)) +
    geom_hline(yintercept = 0, color = "#c60c30", size = 0.35) +
    geom_line(aes(color = plot_color, alpha = alpha), size = 1, show.legend = F) +
    scale_color_identity() +
    geom_point(data = date_data, aes(color = team_color), size = 3) +
    ggrepel::geom_text_repel(data = date_data, aes(label = season),  
              color = "black", nudge_y = 0.02)+
    geom_point(data = game_points, aes(), size = .75, color = "black", fill = "black") +
    theme_BUF() +
    geom_image(data = logo_plot, aes(image = team_logo_espn), size = 0.05) +
    labs(title = "300 Play Moving Average QB Efficiency",
         x = "Play", y = "CPOE",
         caption = "@BufBillsStats
         Data from nflfastR",
         subtitle = glue::glue("{Subtitle}")) +
    scale_x_continuous(limits = c(300, 1.1 * max(chart_data$play))) +
    scale_y_continuous(limits = c(min(-7.5, min(chart_data$rolling_cpoe, na.rm = T)), 
                                  max(10, max(chart_data$rolling_cpoe, na.rm = T)))) +
    geom_hline(yintercept = quantiles$y, color = "black", linetype = 3) +
    geom_text(data = quantiles, aes(x = x, y = y,
                                    label = text), nudge_y = 0.005) 
  
  return(graph)
}


qbs_2016 <- c("J.Goff", "C.Wentz", "D.Prescott")  
qbs_2017 <- c("M.Trubisky", "P.Mahomes", "D.Watson")
qbs_2018 <- c("B.Mayfield", "S.Darnold", "J.Allen", "L.Jackson", "J.Rosen")

rolling_epa_graph(c("L.Jackson", "P.Mahomes", "D.Watson"))

rolling_epa_graph(qbs_2016, Subtitle = "Class of 2016 Quarterbacks")
rolling_epa_graph(qbs_2017, Subtitle = "Class of 2017 Quarterbacks")
rolling_epa_graph(qbs_2018, Subtitle = "Class of 2018 Quarterbacks")

rolling_DAK_graph(qbs_2016, Subtitle = "Class of 2016 Quarterbacks")
rolling_DAK_graph(qbs_2017, Subtitle = "Class of 2017 Quarterbacks")
rolling_DAK_graph(qbs_2018, Subtitle = "Class of 2018 Quarterbacks")

rolling_cpoe_graph(qbs_2016, Subtitle = "Class of 2016 Quarterbacks")
rolling_cpoe_graph(qbs_2017, Subtitle = "Class of 2017 Quarterbacks")
rolling_cpoe_graph(qbs_2018, Subtitle = "Class of 2018 Quarterbacks")

rolling_epa_facet(plot_quarterbacks = c("P.Rivers", "E.Manning", "B.Roethlisberger"), 
                  Subtitle = "Peyton Manning and Some Scrub", first_season = 2006,
                  last_season = 2019)

rolling_DAK_facet(plot_quarterbacks = c("P.Manning", "T.Romo"), 
                  Subtitle = "Peyton Manning and Tony Romo",
                  last_season = 2015)


