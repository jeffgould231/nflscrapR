library(tidyverse)
library(nflscrapR)
source("~/Desktop/nflscrapR/Code/theme_CC.R")

pbp_2018 <- read_rds("~/Desktop/nflscrapR/data/reg_pbp_2018.rds")
# rosters <- nflscrapR::get_season_rosters(season = 2018, teams = nflteams$abbr, positions = c("RUNNING_BACK", "WIDE_RECEIVER", "TIGHT_END"))
# quarterbacks <- nflscrapR::get_season_rosters(season = 2018, teams = nflteams$abbr, positions = c("QUARTERBACK"))
# rosters <- rbind(rosters, quarterbacks)
# 
# saveRDS(rosters, file = "~/Desktop/nflscrapR/data/2018rosters.Rds")
rosters <- read_rds("~/Desktop/nflscrapR/data/2018rosters.Rds")
ppfd_fantasy_game <- pbp_2018 %>%
  filter(play_type %in% c("pass", "run")) %>%
  mutate(fantasy_player_name = if_else(play_type == "pass", receiver_player_name, rusher_player_name),
         fantasy_player_id = if_else(play_type == "pass", receiver_player_id, rusher_player_id)) %>%
  group_by(fantasy_player_name, fantasy_player_id, game_id) %>%
  summarise(yards = sum(yards_gained, na.rm = T), 
            first_down = sum(first_down_pass, na.rm = T) + sum(first_down_rush, na.rm = T),
            TDs = sum(touchdown, na.rm = T),
            Receptions = sum(complete_pass, na.rm = T)) %>%
  mutate(half_ppr = 0.1 * yards + 6 * TDs + 0.5 * Receptions,
         half_ppfd_ppr = 0.1 * yards + 6 * TDs + 0.5 * first_down + 0.5 * Receptions,
         half_ppfd = 0.1 * yards + 6 * TDs + 0.5 * first_down,
         standardPoints = 0.1 * yards + 6* TDs,
         PPRpoints = 0.1 * yards + 6 * TDs + 1 * Receptions) %>%
  ungroup()

ppfd_season <- ppfd_fantasy_game %>%
  group_by(fantasy_player_name, fantasy_player_id) %>%
  summarise(Yards = sum(yards),
            FirstDowns = sum(first_down),
            Touchdowns = sum(TDs),
            Receptions = sum(Receptions),
            Standard = sum(standardPoints, na.rm = T),
            HalfPPR = sum(half_ppr),
            HalfPPR_PPFD = sum(half_ppfd_ppr),
            HalfPPFD = sum(half_ppfd),
            FullPPR = sum(PPRpoints)) %>%
  arrange(desc(HalfPPR_PPFD)) %>%
  left_join(rosters, by = c("fantasy_player_id" = "gsis_id")) %>%
  ungroup() %>%
  select(full_player_name, team, position, Yards, Receptions, Touchdowns, 
         FirstDowns, Standard, HalfPPR, HalfPPFD, HalfPPR_PPFD, FullPPR) %>%
  group_by(position) %>%
  mutate(PosRank = as.integer(rank(desc(HalfPPR_PPFD))),
         StandardRank = as.integer(rank(desc(Standard))),
         HalfPPRrank = as.integer(rank(desc(HalfPPR))),
         HalfPPFDrank = as.integer(rank(desc(HalfPPFD))))

ggplot(data = filter(ppfd_season, position %in% c("RB", "WR", "TE")), 
       aes(x = HalfPPRrank, y = FullPPR, color = position)) +
  geom_line()+
  geom_point() +
  scale_x_continuous(limits = c(0,50)) +
  theme_CC() +
  labs(title = "0.5 PPR Fantasy Scoring and Position Ranks", x = "Position Ranks", y = "Points")

write_csv(ppfd_season, path = "~/Desktop/nflscrapR/Output/ppfd.csv")
