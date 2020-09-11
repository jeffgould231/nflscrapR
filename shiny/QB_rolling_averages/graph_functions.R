make_roll_averages <- function(seasons){
  quarterbacks <- readRDS("quarterbacks.rds")
  teams_colors_logos <- readRDS("teams_colors_logos.rds")
  purrr::map_df(seasons, function(x) {
    readRDS(
      url(
        glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
      )
    ) %>% filter(name %in% quarterbacks$passer, id %in% quarterbacks$passer_id, 
                 season_type == "REG", !is.na(epa)) %>%
      select(season, game_id, game_date, game_seconds_remaining, posteam, epa, cpoe, 
             passer, passer_id, name, id, fumble_lost, complete_pass, penalty, comp_air_epa) %>%
      arrange(game_date, game_id, game_seconds_remaining) %>%
      mutate(epa = ifelse(complete_pass == 1 & fumble_lost == 1, comp_air_epa, epa)) %>%
      group_by(name, id) %>%
      mutate(rolling_epa = zoo::rollapply(epa, 300, mean, na.rm = T, align = "right", fill = NA),
             rolling_cpoe = zoo::rollapply(cpoe, 300, mean, na.rm = T, align = "right", fill = NA),
             play = row_number()) %>%
      ungroup() %>%
      mutate(DAKOTA = 0.20 * rolling_epa + 0.009 * rolling_cpoe + 0.09) %>%
      left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))
    
  })
}



rolling_epa_graph <- function(data, plot_quarterbacks, seasons, Subtitle = NULL){
  source("theme_BUF.R")
  teams_colors_logos <- readRDS("teams_colors_logos.rds")
  chart_data <- data %>%
    filter(name %in% plot_quarterbacks, !is.na(rolling_epa), play %% 5 == 0, season %in% seasons) %>%
    arrange(game_date, game_seconds_remaining, play) %>%
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
                      y = quantile(data$rolling_epa, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = T),
                      text = c("10%", "25%", "50%", "75%", "90%"))
  
  graph <- ggplot(data = chart_data, aes(x = play, y = rolling_epa)) +
    geom_hline(yintercept = 0, color = "#c60c30", size = 0.35) +
    geom_line(aes(color = posteam, alpha = alpha), size = 1, show.legend = F) +
    scale_color_manual(values = teams_colors_logos$team_color, breaks = teams_colors_logos$team_abbr) +
    geom_point(data = date_data, aes(color = team_color), size = 3, show.legend = F) +
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

rolling_DAK_graph <- function(data, plot_quarterbacks, seasons, Subtitle = NULL){
  source("theme_BUF.R")
  chart_data <- data %>%
    filter(name %in% plot_quarterbacks, !is.na(DAKOTA), play %% 5 == 0, season %in% seasons) %>%
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
                      y = quantile(data$DAKOTA, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = T),
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

rolling_epa_facet <- function(data, plot_quarterbacks, 
                              first_season = NULL, 
                              last_season = NULL,
                              Subtitle = NULL){
  
  chart_data <- data %>%
    filter(name %in% plot_quarterbacks)
  source("theme_BUF.R")
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
                      y = quantile(data$rolling_epa, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = T),
                      text = c("10%", "25%", "50%", "75%", "90%"))
  
  graph <- ggplot(data = chart_data, aes(x = play, y = rolling_epa)) + 
    facet_wrap(~season, ncol = min(4, length(unique(chart_data$season)))) +
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

rolling_DAK_facet <- function(data, plot_quarterbacks, 
                              first_season = NULL, 
                              last_season = NULL,
                              Subtitle = NULL){
  
  chart_data <- data %>%
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
                      y = quantile(data$DAKOTA, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = T),
                      text = c("10%", "25%", "50%", "75%", "90%"))
  
  graph <- ggplot(data = chart_data, aes(x = play, y = DAKOTA)) + 
    facet_wrap(~season, ncol = min(4, length(unique(chart_data$season)))) +
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

rolling_cpoe_graph <- function(data, plot_quarterbacks, seasons, Subtitle = NULL){
  source("theme_BUF.R")
  chart_data <- data %>%
    filter(name %in% plot_quarterbacks, !is.na(rolling_cpoe), play %% 5 == 0, season %in% seasons) %>%
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
                      y = quantile(data$rolling_cpoe, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = T),
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
    geom_image(data = logo_plot, aes(image = team_logo_espn), size = 0.1) +
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

rolling_cpoe_facet <- function(data, plot_quarterbacks, 
                              first_season = NULL, 
                              last_season = NULL,
                              Subtitle = NULL){
  
  source("theme_BUF.R")
  chart_data <- data %>%
    filter(name %in% plot_quarterbacks)
  source("theme_BUF.R")
  if(is.null(first_season)){first_season <- min(chart_data$season)}
  if(is.null(last_season)){last_season <- max(chart_data$season)}
  
  chart_data <- chart_data %>%
    group_by(name, season) %>%
    mutate(play = row_number(),
           alpha = 0.9) %>%
    ungroup() %>%
    filter(season >= first_season, season <= last_season,
           !is.na(rolling_cpoe), play %% 3 == 0) %>%
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
                      y = quantile(data$rolling_cpoe, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = T),
                      text = c("10%", "25%", "50%", "75%", "90%"))
  
  graph <- ggplot(data = chart_data, aes(x = play, y = rolling_cpoe)) +
    facet_wrap(~season, ncol = min(4, length(unique(chart_data$season)))) +
    geom_hline(yintercept = 0, color = "#c60c30", size = 0.35) +
    geom_line(aes(color = plot_color, alpha = alpha), size = 1, show.legend = F) +
    scale_color_identity() +
    geom_point(data = date_data, aes(color = team_color), size = 3) +
    ggrepel::geom_text_repel(data = date_data, aes(label = season),  
                             color = "black", nudge_y = 0.02)+
    geom_point(data = game_points, aes(), size = .75, color = "black", fill = "black") +
    theme_BUF() +
    theme(strip.background = element_blank()) +
    geom_image(data = logo_plot, aes(image = team_logo_espn), size = 0.1) +
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