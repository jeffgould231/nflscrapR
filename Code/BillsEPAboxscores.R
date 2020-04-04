library(nflscrapR)
library(tidyverse)
library(na.tools)
library(gt)

#PICK THE GAME YOU WANT BY ENTERING THESE FIELDS
season <- 2018
type <- "reg" #reg, pre, post
h <- "BAL"
a <- "BUF"

#NOW YOU DON'T NEED TO DO ANYTHING ELSE EXCEPT CHANGE THE PATHS AT THE VERY BOTTOM
create_boxscore <- function(season = 2018, type = "reg", h, a, id = NA) {
  if(is.na(id) == T) {
    ids <- scrape_game_ids(season, teams = h, type = type)
    id <- ids %>% filter(away_team == a) %>% pull(game_id)
  }
  
  pbp_data <- scrape_json_play_by_play(id)
  #pbp_data <- readRDS("~/Desktop/nflscrapR/data/reg_pbp_2018.rds")
  #clean it up
  pbp <- pbp_data %>%
    filter(
      !is_na(epa),
      !is_na(posteam),
      play_type == "no_play" | play_type == "pass" |
        play_type == "run"
    ) %>%
    mutate(
      pass = if_else(str_detect(desc, "(pass)|(sacked)|(scramble)"), 1, 0),
      rush = if_else(
        str_detect(
          desc,
          "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)"
        ) & pass == 0,
        1,
        0
      ),
      success = ifelse(epa > 0, 1 , 0),
      passer_player_name = ifelse(
        play_type == "no_play" & pass == 1,
        str_extract(
          desc,
          "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((pass)|(sack)|(scramble)))"
        ),
        passer_player_name
      ),
      receiver_player_name = ifelse(
        play_type == "no_play" & str_detect(desc, "pass"),
        str_extract(
          desc,
          "(?<=to\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"
        ),
        receiver_player_name
      ),
      rusher_player_name = ifelse(
        play_type == "no_play" & rush == 1,
        str_extract(
          desc,
          "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)))"
        ),
        rusher_player_name
      ),
      name = ifelse(
        !is_na(passer_player_name),
        passer_player_name,
        rusher_player_name
      ),
      yards_gained = ifelse(play_type == "no_play", NA, yards_gained),
      play = 1
    ) %>%
    mutate(receiver_player_name = if_else(receiver_player_name == "K.Benjamin", "[redacted]", receiver_player_name)) %>%
    filter(pass == 1 | rush == 1)
  
  home <-
    pbp %>% select(home_team, away_team) %>% slice(1) %>% pull(home_team)
  away <-
    pbp %>% select(home_team, away_team) %>% slice(1) %>% pull(away_team)
  
  #do stuff for the team summary table
  all <- pbp %>% group_by(posteam) %>% summarize(
    epa = mean(epa),
    success = mean(success),
    p = mean(pass),
    play = n()
  ) %>%
    mutate(rowname = "All plays", type = 1)
  
  early <-
    pbp %>% filter(down == 1 |
                     down == 2) %>% group_by(posteam) %>% summarize(
                       epa = mean(epa),
                       success = mean(success),
                       p = mean(pass),
                       play = n()
                     ) %>%
    mutate(rowname = "Early downs (1st & 2nd)", type = 4)
  
  earlyr <-
    pbp %>% filter((down == 1 |
                      down == 2) & rush == 1) %>% group_by(posteam) %>% summarize(
                        epa = mean(epa),
                        success = mean(success),
                        p = mean(pass),
                        play = n()
                      ) %>%
    mutate(rowname = "Early rush", type = 5)
  
  earlyp <-
    pbp %>% filter((down == 1 |
                      down == 2) & pass == 1) %>% group_by(posteam) %>% summarize(
                        epa = mean(epa),
                        success = mean(success),
                        p = mean(pass),
                        play = n()
                      ) %>%
    mutate(rowname = "Early pass", type = 6)
  
  late <-
    pbp %>% filter(down == 3  |
                     down == 4) %>% group_by(posteam) %>% summarize(
                       epa = mean(epa),
                       success = mean(success),
                       p = mean(pass),
                       play = n()
                     ) %>%
    mutate(rowname = "Late downs (3rd & 4th)", type = 7)
  
  type <- pbp %>% group_by(posteam, pass) %>% summarize(
    epa = mean(epa),
    success = mean(success),
    p = mean(pass),
    play = n()
  ) %>%
    mutate(rowname = if_else(pass == 1, "Pass", "Rush"),
           type = 2)
  
  bound <- bind_rows(all, early, earlyr, earlyp, late, type) %>%
    mutate(
      home = ifelse(posteam == home, 1, 0),
      p = round(100 * p),
      epa = round(epa, digits = 2),
      success = round(success, digits = 2)
    ) %>%
    arrange(home, type) %>% select(-pass,-type,-home)
  
  #team summary table
  table <-
    bound %>%  select(posteam, rowname, epa, success, play) %>% group_by(posteam) %>% gt() %>%
    cols_label(
      epa = md("**EPA/<br>play**"),
      success = md("**Success<br>rate**"),
      play = md("**Plays**")
    ) %>%
    cols_align(align = "center") %>%
    tab_source_note(source_note = "Table: @BufBillsStats | Data: @nflscrapR") %>%
    tab_header(title = paste("Game Summary,", away, "@", home)) %>%
    tab_style(style = list(cell_text(weight = "bold")),
              locations = cells_group(groups = TRUE)) %>%
    tab_style(style = list(cell_text(style = "italic", align = "center")),
              locations = cells_stub(rows = c(2, 3, 9, 10, 5, 6, 12, 13)))
  
  ##### do stuff for player summary table
  
  rushers <-
    pbp %>% filter(rush == 1) %>% group_by(rusher_player_name, posteam) %>%
    summarize(
      tot_epa = sum(epa),
      epa = mean(epa),
      success = mean(success),
      play = n()
    ) %>%
    mutate(
      rowname = "Rush attempts",
      type = 1,
      p = "Rush attempts",
      rowname = rusher_player_name
    ) %>% ungroup()
  
  receivers <-
    pbp %>% filter(rush == 0 &
                     !is.na(receiver_player_name)) %>% group_by(receiver_player_name, posteam) %>%
    summarize(
      tot_epa = sum(epa),
      epa = mean(epa),
      success = mean(success),
      play = n()
    ) %>%
    mutate(
      rowname = "Targets",
      type = 2,
      p = "Targets",
      rowname = receiver_player_name
    ) %>% ungroup()
  
  passers <-
    pbp %>% filter(rush == 0 &
                     !is.na(name)) %>% group_by(name, posteam) %>%
    summarize(
      tot_epa = sum(epa),
      epa = mean(epa),
      success = mean(success),
      play = n()
    ) %>%
    mutate(
      rowname = "Dropbacks",
      type = 0,
      p = "Dropbacks",
      rowname = name
    ) %>% ungroup()
  
  
  rp <- bind_rows(passers, rushers, receivers) %>%
    mutate(
      home = ifelse(posteam == home, 1, 0),
      epa = round(epa, digits = 2),
      tot_epa = round(tot_epa, digits = 1),
      success = round(success, digits = 2)
    ) %>%
    arrange(type, home, desc(play)) %>% select(-type,-rusher_player_name,-receiver_player_name,-name)
  
  #player summary as one big table
  t2 <-
    rp %>% select(posteam, rowname, epa, tot_epa, success, play, p) %>% group_by(p) %>%
    gt() %>%
    cols_label(
      posteam = md("**Team**"),
      success = md("**Success<br>rate**"),
      play = md("**Plays**"),
      epa = md("**EPA/<br>play**"),
      tot_epa = md("**Total<br>EPA**")
    ) %>%
    cols_align(align = "center") %>%
    tab_source_note(source_note = "Table: @BufBillsStats | Data: @nflscrapR") %>%
    tab_header(title = paste("Game Summary,", away, "@", home)) %>%
    tab_style(style = list(cell_text(weight = "bold")),
              locations = cells_group(groups = TRUE))
  
  #player summary for away team
  t2a <-
    rp %>% filter(home == 0) %>% select(posteam, rowname, epa, tot_epa, success, play, p) %>% group_by(p) %>%
    gt() %>%
    cols_label(
      posteam = md("**Team**"),
      success = md("**Success<br>rate**"),
      play = md("**Plays**"),
      epa = md("**EPA/<br>play**"),
      tot_epa = md("**Total<br>EPA**")
    ) %>%
    cols_align(align = "center") %>%
    tab_source_note(source_note = "Table: @BufBillsStats | Data: @nflscrapR") %>%
    tab_header(title = paste("Game Summary,", away, "@", home)) %>%
    tab_style(style = list(cell_text(weight = "bold")),
              locations = cells_group(groups = TRUE))
  
  #player summary for hom team
  t2h <-
    rp %>% filter(home == 1) %>% select(posteam, rowname, epa, tot_epa, success, play, p) %>% group_by(p) %>%
    gt() %>%
    cols_label(
      posteam = md("**Team**"),
      success = md("**Success<br>rate**"),
      play = md("**Plays**"),
      epa = md("**EPA/<br>play**"),
      tot_epa = md("**Total<br>EPA**")
    ) %>%
    cols_align(align = "center") %>%
    tab_source_note(source_note = "Table: @BufBillsStats | Data: @nflscrapR") %>%
    tab_header(title = paste("Game Summary,", away, "@", home)) %>%
    tab_style(style = list(cell_text(weight = "bold")),
              locations = cells_group(groups = TRUE))
  
  output_dir <- "~/Desktop/nflscrapR/Output/Boxscores/"
  #export all the tables
  
  #team summary
  table %>% gtsave(paste0(output_dir, a, "at", h, "game_summary.png"))
  
  #player summary
  t2 %>% gtsave(paste0(output_dir, a, "at", h, "game_players.png"))
  
  #away player summary
  t2a %>% gtsave(paste0(output_dir, a, "players_at", h, ".png"))
  
  #home player summary
  t2h %>% gtsave(paste0(output_dir, h, "players_vs", a, ".png"))
  
}

ids <- nflscrapR::scrape_game_ids(season = 2018, type = "reg", teams = "BUF")
home <- ids$home_team
away <- ids$away_team
gameids <- ids$game_id

mapply(create_boxscore, h = home[8:16], a = away[8:16], id = gameids[8:16])

create_boxscore(2018, "reg", "BAL", "BUF", id = "2018090900")
