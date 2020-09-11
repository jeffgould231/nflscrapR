#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
#library(nflfastR)
#library(nflscrapR)
library(ggimage)
library(lubridate)

source("graph_functions.R")

seasons <- 2011:2019
teams_colors_logos <- readRDS("teams_colors_logos.rds")
quarterbacks <- readRDS("quarterbacks.rds")

pbp_work <- purrr::map_df(seasons, function(x) {
  readr::read_csv(
    glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.csv.gz")
  ) %>% filter(name %in% quarterbacks$passer, id %in% quarterbacks$passer_id,
               season_type == "REG", !is.na(epa)) %>%
    select(season, game_id, game_date, game_seconds_remaining, play_id, posteam, epa, cpoe,
           passer, passer_id, name, id, fumble_lost, complete_pass, penalty, comp_air_epa) %>%
    arrange(game_date, game_id, game_seconds_remaining) %>%
    distinct() %>%
    mutate(epa = ifelse(complete_pass == 1 & fumble_lost == 1, comp_air_epa, epa))

}) %>% group_by(name) %>%
  mutate(rolling_epa = zoo::rollapply(epa, 300, mean, na.rm = T, align = "right", fill = NA),
         rolling_cpoe = zoo::rollapply(cpoe, 300, mean, na.rm = T, align = "right", fill = NA),
         play = row_number()) %>%
  ungroup() %>%
  mutate(DAKOTA = 0.20 * rolling_epa + 0.009 * rolling_cpoe + 0.09) %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

#
# penalty_list <- c(
#   NA, "Offensive Pass Interference", "Defensive Pass Interference","Defensive Holding", "Roughing the Passer",
#   "Offensive Holding", "Defensive Offside", "Personal Foul", "Encroachment", "Intentional Grounding",
#   "Unnecessary Roughness", "Ineligible Downfield Pass", "Neutral Zone Infraction", "Defensive Too Many Men on Field"
# )
#
# pbp_work <- pbp_data %>%
#   distinct() %>%
#   filter((pass == 1 | rush == 1),
#          !is.na(epa),
#          season_type == "REG") %>%
#   mutate(Id = ifelse(!is.na(passer_id), passer_id,
#                      ifelse(!is.na(rusher_id), rusher_id,
#                             NA)),
#          name = ifelse(!is.na(passer), passer,
#                        ifelse(!is.na(rusher), rusher, NA))) %>%
#   filter(Id %in% quarterbacks$passer_id) %>%
#   mutate(epa = ifelse(complete_pass == 1 & fumble_lost == 1, comp_air_epa, epa)) %>%
#   arrange(name, Id, game_date, desc(game_seconds_remaining))
#
# rm(pbp_data)
#
# pbp_work <- pbp_work %>%
#   group_by(name, Id) %>%
#   mutate(rolling_epa = zoo::rollapply(epa, 300, mean, na.rm = T, align = "right", fill = NA),
#          rolling_cpoe = zoo::rollapply(cpoe, 300, mean, na.rm = T, align = "right", fill = NA),
#          play = row_number()) %>%
#   ungroup() %>%
#   mutate(DAKOTA = 0.20 * rolling_epa + 0.009 * rolling_cpoe + 0.09)
#
# pbp_work <- pbp_work %>%
#   left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Bills Stats & Graphs QB Charting (beta)"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("seasons",
                        "Choose Seasons",
                        min = 2011,
                        max = 2019,
                        value = c(2018, 2019), sep = NULL),
            selectInput("plotQuarterbacks", "Choose Quarterbacks", multiple = TRUE, choices = quarterbacks$passer, 
                        selected = c("J.Allen", "L.Jackson", "S.Darnold", "B.Mayfield")),
            selectInput("graphFormat", "Choose Graph Style", choices = c("Continuous", "Seasonal"), selected = "Continuous"),
            submitButton("Update"),
            width = 2
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("rollingEPA"),
           br(),
           plotOutput("rollingCPOE"),
           br(),
           plotOutput("rollingDAK")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
  #pbp_work <- reactive(return(make_roll_averages(seasons = c(input$seasons[1]:input$seasons[2]))))
  
    output$rollingEPA <- renderPlot({
        if(input$graphFormat == "Continuous"){
            return(rolling_epa_graph(data = pbp_work, input$plotQuarterbacks, 
                                     seasons = c(input$seasons[1]:input$seasons[2])))
        }else if(input$graphFormat == "Seasonal"){
            return(rolling_epa_facet(data = pbp_work, input$plotQuarterbacks, 
                                     first_season = input$seasons[1], last_season = input$seasons[2]))
        }
    })
    
    output$rollingCPOE <- renderPlot({
        if(input$graphFormat == "Continuous"){
            return(rolling_cpoe_graph(data = pbp_work, input$plotQuarterbacks, 
                                      seasons = c(input$seasons[1]:input$seasons[2])))
        }else if(input$graphFormat == "Seasonal"){
            return(rolling_cpoe_facet(data = pbp_work, input$plotQuarterbacks, 
                                      first_season = input$seasons[1], last_season = input$seasons[2]))
        }
    })
    
    output$rollingDAK <- renderPlot({
        if(input$graphFormat == "Continuous"){
            return(rolling_DAK_graph(data = pbp_work, input$plotQuarterbacks, 
                                     seasons = c(input$seasons[1]:input$seasons[2])))
        }else if(input$graphFormat == "Seasonal"){
            return(rolling_DAK_facet(data = pbp_work, input$plotQuarterbacks, 
                                     first_season = input$seasons[1], last_season = input$seasons[2]))
        }
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
