library(nflfastR)
library(tidyverse)
library(nflscrapR)

setwd("~/Documents/nflscrapR/")
input_dir <- "Data/"
output_dir <- "Output/"

schedule_2019 <- fast_scraper_schedules(2019)
pbp_2019 <- readRDS(glue::glue("{input_dir}pbp_2019.rds"))

big_data_predictions <- read_csv(glue::glue("{input_dir}predictions_2019.csv")) %>%
  mutate(PlayId = as.character(PlayId)) %>%
  select(-X1)

calc_exp_yards <- function(x){
  yards_pdf <- x[2:199] - x[1:198]
  exp_yards <- sum(yards_pdf * c(-98:99))
  return(exp_yards)
}
calc_percentile <- function(x){
  yds_gained <- x[1]
  cdf <- x[-1]
  percentile <- cdf[yds_gained+100]
  return(percentile)
}

runs_2019 <- pbp_2019 %>%
  filter(play_type == "run", !is.na(yards_gained)) %>%
  arrange(game_date, game_id, -game_seconds_remaining) %>%
  left_join(schedule_2019[,c(1,17)]) %>%
  mutate(play_id = str_pad(as.character(play_id), 4, side = "left", pad = "0")) %>%
  mutate(PlayId = str_c(old_game_id, play_id)) %>%
  select(PlayId, rusher_player_name, rusher, rusher_id, posteam, defteam, week, series,
         yards_gained, yardline_100, epa, wpa, roof, surface, run_location, run_gap, touchdown, first_down) %>%
  inner_join(big_data_predictions) %>%
  rowwise() %>%
  mutate(first_quartile_yards_gained = min(which(c_across(`Yards-99`:Yards99) >= 0.25)) - 100,
         median_yards_gained = min(which(c_across(`Yards-99`:Yards99) >= 0.5)) - 100,
         third_quartile_yards_gained = min(which(c_across(`Yards-99`:Yards99) >= 0.75)) - 100,
         yards_gained_over_median = yards_gained - median_yards_gained,
         exp_yards = calc_exp_yards(c_across(`Yards-99`:Yards99)),
         yards_gained_over_expected = yards_gained - exp_yards) %>%
  mutate(percentile_result = calc_percentile(c_across(c(yards_gained, `Yards-99`:Yards99)))) %>%
  ungroup() %>%
  mutate(quartile_result = ifelse(yards_gained <= first_quartile_yards_gained, 1,
                                  ifelse(yards_gained <= median_yards_gained, 2,
                                         ifelse(yards_gained <= third_quartile_yards_gained, 3, 4))))



rushers_2019 <- runs_2019 %>%
  group_by(rusher, rusher_id) %>%
  summarise(attempts = n()) %>%
  filter(attempts >= 69)

afc_teams <- c("BUF", "MIA", "NE", "NYJ", "PIT", "BAL", "CLE", "CIN", "LAC", "DEN", 
               "KC", "LV", "IND", "HOU", "TEN", "JAC", "JAX")
nfc_teams <- c("DAL", "PHI", "NYG", "WAS", "CHI", "GB", "DET", "MIN", "LA", "SEA", "ARI", "SF", 
               "ATL", "NO", "TB", "CAR")

runs_2019 %>%
  filter(rusher_id %in% rushers_2019$rusher_id, posteam %in% afc_teams) %>%
  group_by(rusher) %>%
  mutate(med_exp_yards = median(exp_yards)) %>%
  ungroup() %>%
  mutate(rusher = forcats::fct_reorder(rusher, med_exp_yards)) %>%
  left_join(nflfastR::teams_colors_logos, by = c("posteam" = "team_abbr")) %>%
  ggplot() +
  geom_boxplot(aes(x = rusher, y = exp_yards, color = team_color, fill = team_color2)) +
  scale_color_identity() +
  scale_fill_identity() +
  coord_flip() + 
  scale_y_continuous(breaks = c(-5,seq(0,10,2),15,20)) +
  theme_bw() +
  labs(y = "Expected Yards Gained At Time Of Handoff", x = NULL)


runs_2019 %>%
  filter(rusher_id %in% rushers_2019$rusher_id, posteam %in% nfc_teams) %>%
  group_by(rusher) %>%
  mutate(med_exp_yards = median(exp_yards)) %>%
  ungroup() %>%
  mutate(rusher = forcats::fct_reorder(rusher, med_exp_yards)) %>%
  left_join(nflfastR::teams_colors_logos, by = c("posteam" = "team_abbr")) %>%
  ggplot() +
  geom_boxplot(aes(x = rusher, y = exp_yards, color = team_color, fill = team_color2)) +
  scale_color_identity() +
  scale_fill_identity() +
  coord_flip() + 
  scale_y_continuous(breaks = c(-5,seq(0,10,2),15,20)) +
  theme_bw() +
  labs(y = "Expected Yards Gained At Time Of Handoff", x = NULL)

runs_2019 %>%
  filter(rusher_id %in% rushers_2019$rusher_id, posteam %in% nfc_teams|rusher == "K.Drake") %>%
  mutate(posteam = ifelse(rusher == "K.Drake", "ARI", posteam)) %>%
  group_by(rusher) %>%
  mutate(med_percentile = median(percentile_result),
         player_percentile_play = percent_rank(percentile_result)) %>%
  ungroup() %>%
  mutate(rusher = forcats::fct_reorder(rusher, med_percentile)) %>%
  left_join(nflfastR::teams_colors_logos, by = c("posteam" = "team_abbr")) %>%
  ggplot(aes(x = 100 * percentile_result, y = rusher)) +
  geom_boxplot(aes(fill = team_color, color = team_color2)) +
  scale_color_identity() +
  scale_fill_identity() +
  #viridis::scale_fill_viridis(option = "C") +
  theme_bw() +
  scale_x_continuous(limits = c(0,100), breaks = seq(0,100,20)) +
  labs(x = "Percentile of Play Result Based on Expectations at Hand-Off")

runs_2019 %>%
  filter(rusher_id %in% rushers_2019$rusher_id, posteam %in% afc_teams, rusher != "K.Drake") %>%
  group_by(rusher) %>%
  mutate(med_percentile = median(percentile_result),
         player_percentile_play = percent_rank(percentile_result)) %>%
  ungroup() %>%
  mutate(rusher = forcats::fct_reorder(rusher, med_percentile)) %>%
  left_join(nflfastR::teams_colors_logos, by = c("posteam" = "team_abbr")) %>%
  ggplot(aes(x = 100 * percentile_result, y = rusher)) +
  ggridges::geom_density_ridges(aes(fill = team_color, color = team_color2), scale = 1.25) +
  scale_color_identity() +
  scale_fill_identity() +
  #viridis::scale_fill_viridis(option = "C") +
  theme_bw() +
  scale_x_continuous(limits = c(0,100), breaks = seq(0,100,20)) +
  labs(x = "Percentile of Play Result Based on Expectations at Hand-Off")

runs_2019 %>%
  filter(rusher_id %in% rushers_2019$rusher_id, posteam %in% afc_teams, rusher != "K.Drake") %>%
  group_by(rusher) %>%
  mutate(med_percentile = median(percentile_result),
         player_percentile_play = percent_rank(percentile_result)) %>%
  ungroup() %>%
  mutate(rusher = forcats::fct_reorder(rusher, med_percentile)) %>%
  left_join(nflfastR::teams_colors_logos, by = c("posteam" = "team_abbr")) %>%
  ggplot(aes(x = 100 * percentile_result, y = rusher)) +
  geom_boxplot(aes(fill = team_color, color = team_color2)) +
  scale_color_identity() +
  scale_fill_identity() +
  #viridis::scale_fill_viridis(option = "C") +
  theme_bw() +
  scale_x_continuous(limits = c(0,100), breaks = seq(0,100,20)) +
  labs(x = "Percentile of Play Result Based on Expectations at Hand-Off")


Dsing <- runs_2019 %>%
  filter(rusher == "D.Singletary") %>%
  dplyr::select(-starts_with("Yards")) %>%
  mutate(rush_gap = ifelse(!is.na(run_gap), str_c(run_location, "_", run_gap), run_location)) %>%
  mutate(rush_gap = factor(rush_gap, ordered = T, levels = c( "left_end", "left_tackle","left_guard", 
                                                             "middle", "right_guard", "right_tackle", "right_end"))) %>%
  left_join(nflfastR::teams_colors_logos, by = c("posteam" = "team_abbr")) %>%
  group_by(rush_gap) %>%
  mutate(attempts = n(), med_val = median(percentile_result))

ggplot(data = Dsing, aes(x = rush_gap, y = percentile_result)) +
  geom_boxplot(aes(color = team_color, fill = team_color2)) +
  scale_fill_identity() +
  scale_color_identity() +
  theme_bw() +
  geom_text(aes(y = med_val, label = attempts, color = team_color), nudge_y = 0.025, size = 6)


plot_data1 <- runs_2019 %>%
  dplyr::filter(yardline_100 >= 15) %>%
  ungroup() %>%
  group_by(rusher, posteam) %>%
  dplyr::summarise(avg_exp_yards = mean(exp_yards),
            breakaway_rate = 100 * mean(percentile_result >= 0.8),
            attempts = n())  %>%
  filter(attempts >= 15) %>%
  left_join(nflfastR::teams_colors_logos, by = c("posteam" = "team_abbr")) %>%
  ungroup()

plot_data_labels <- plot_data1 %>% filter(rusher %in% rushers_2019$rusher)
source("Code/theme_BUF.R")

ggplot(data = plot_data1,
       aes(x = avg_exp_yards, y = breakaway_rate, color = team_color, size = attempts)) +
  geom_point(show.legend = F) +
  scale_color_identity() +
  theme_BUF() +
  ggrepel::geom_text_repel(data = plot_data_labels, aes(label = rusher, color = team_color2), show.legend = F) +
  labs(title = "Breakaway Rush Rate Using Big Data Bowl", 
       subtitle = "Breakaway rush is defined as rush attempts that reach the upper 20th percentile of predicted outcomes, attemtps from outside the 15 yardline",
       x = "Average Expected Yards Gained At Time of Hand-Off", y = "Breakaway Rush Rate", caption = "@BufBillsStats")
ggsave(glue::glue("{output_dir}breakaway_rate_vs_exp_yards.jpg"), width = 19, height = 9)

set.seed(420)
sample1 <- runs_2019 %>%
  filter(yardline_100 >= 15, rusher != "K.Drake") %>%
  group_by(rusher, rusher_id, posteam) %>%
  sample_frac(0.5) %>%
  dplyr::select(-starts_with("Yards")) %>%
  ungroup()

sample2 <- runs_2019 %>%
  filter(yardline_100 >= 15, rusher != "K.Drake") %>%
  filter(!(PlayId %in% sample1$PlayId)) %>%
  dplyr::select(-starts_with("Yards"))

sample_1_test <- sample1 %>%
  group_by(rusher, rusher_id, posteam) %>%
  dplyr::summarise(avg_exp_yards_1 = mean(exp_yards),
                   med_percentile_1 = median(percentile_result),
                   breakaway_rate_1 = 100 * mean(percentile_result >= 0.8),
                   attempts_1 = n())  %>%
  filter(attempts_1 >= 30)

sample_2_test <- sample2 %>%
  group_by(rusher, rusher_id,  posteam) %>%
  dplyr::summarise(avg_exp_yards_2 = mean(exp_yards),
                   med_percentile_2 = median(percentile_result),
                   breakaway_rate_2 = 100 * mean(percentile_result >= 0.8),
                   attempts_2 = n())  %>%
  filter(attempts_2 >= 30)

joined_samples <- inner_join(sample_1_test, sample_2_test) %>%
  left_join(nflfastR::teams_colors_logos, by = c("posteam" = "team_abbr"))

ggplot(data = joined_samples, aes(x = breakaway_rate_1, breakaway_rate_2)) +
  geom_point(aes(color = team_color, size = attempts_1), show.legend = F) +
  ggrepel::geom_text_repel(aes(label = rusher, color = team_color2)) +
  scale_color_identity() +
  theme_BUF() +
  labs(x = "Breakaway Rush Rate Sample 1", y = "Breakaway Rush Rate Sample 2",
       title = "Running Stability With Big Data Bowl", caption = "@BufBillsStats") +
  geom_smooth(method = "lm", alpha = 0, linetype = 3, color = "red") +
  scale_x_continuous(limits = c(5,40)) +
  scale_y_continuous(limits = c(5,40))
ggsave(glue::glue("{output_dir}breakaway_rushing_stability.jpg"), width = 8.5, height = 8)

ggplot(data = joined_samples, aes(x = avg_exp_yards_1, avg_exp_yards_2)) +
  geom_point(aes(color = team_color, size = attempts_1), show.legend = F) +
  ggrepel::geom_text_repel(aes(label = rusher, color = team_color2)) +
  scale_color_identity() +
  theme_BUF() +
  labs(x = "Average Expected Yards At Hand-Off Sample 1", y = "Average Expected Yards At Hand-Off Sample 2",
       title = "Running Stability With Big Data Bowl", caption = "@BufBillsStats") +
  geom_smooth(method = "lm", alpha = 0, linetype = 3, color = "red") +
  scale_x_continuous(limits = c(3.5, 6.5)) +
  scale_y_continuous(limits = c(3.5, 6.5))

ggsave(glue::glue("{output_dir}exp_yards_stability.jpg"), width = 8.5, height = 8)

ggplot(data = joined_samples, aes(x = med_percentile_1, med_percentile_2)) +
  geom_point(aes(color = team_color, size = attempts_1), show.legend = F) +
  ggrepel::geom_text_repel(aes(label = rusher, color = team_color2)) +
  scale_color_identity() +
  labs(x = "Median Percentile Result Sample 1", y = "Median Percentile Result Sample 2",
       title = "Running Stability With Big Data Bowl", caption = "@BufBillsStats") +
  geom_smooth(method = "lm", alpha = 0, linetype = 3, color = "red") +
  scale_x_continuous(limits = c(0.3, 0.75)) +
  scale_y_continuous(limits = c(0.3, 0.75)) +
  theme_BUF()

ggsave(glue::glue("{output_dir}median_result_stability.jpg"), width = 11, height = 10)



