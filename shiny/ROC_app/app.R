
library(shiny)
library(tidyverse)
library(plotly)
library(mgcv)
library(pls)

teams_colors_logos <- readRDS("teams_colors_logos.rds")

# seasons <- c(2014:2019)
# rosters <- readRDS("rosters.rds")
# 
# pbp_all <- map_dfr(seasons, function(x){read_rds(glue::glue("pbp_{x}.rds"))}) %>%
#   left_join(rosters, by = c("receiver" = "receiver", "season" = "team.season", "posteam" = "team.abbr")) %>%
#   select(play_type, week, pass_attempt, season, receiver, receiver_id, posteam, season, teamPlayers.position,
#          yards_gained, complete_pass, touchdown, epa, first_down, air_yards, yardline_100, game_id)
#  saveRDS(pbp_all, "pbp_all.rds") 
pbp_all <- readRDS("pbp_all.rds") %>% distinct() #%>% select(-teamPlayers.position)
player_ids <- read.csv("id_database.csv") %>% select(pos, id_nfl_new) %>% distinct() %>% filter(!is.na(id_nfl_new))
pbp_all <- pbp_all %>%
  left_join(player_ids, by = c("receiver_id" = "id_nfl_new")) %>%
  rename(teamPlayers.position = pos) %>%
  mutate(air_yards = ifelse(air_yards == -82, 18, air_yards))
 # ROC_model <- readRDS("ROC_model.rds")


source("ROC_functions.R")
source("theme_CC.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Contested Catch WR Opportunity (beta)"),
    
    fluidRow(column(1, img(src = "ContestedCatchLogo.png", height = 125)),
             column(2, offset = 1, selectInput("seasons", "Choose Seasons to include:",
                                   selected = 2020, multiple = T, choices = c(2014:2020))),
             column(2, selectInput("weeks", "Choose weeks to include:", 
                                   selected = 1, multiple = T, choices = c(1:21))),
             column(1, numericInput("target_floor", "Target Minimum", 
                                    min = 0, max = 150, value = 3, step = 5)),
             column(1, selectInput("team", "Select Team for table", 
                                   selected = "All", choices = c("All", 
                                                                 teams_colors_logos$team_abbr[
                                                                   !(teams_colors_logos$team_abbr %in% c(
                                                                     "OAK", "STL", "SD", "LAR"))
                                                                   ]))),
             column(2, checkboxGroupInput("positions", "Choose Positions (partially working)", 
                                       choices = c("RB", "WR", "TE"), 
                                       selected = c("RB", "WR", "TE"), inline = T))
             
                    ),
    br(),

           plotlyOutput("ROC_plot", height = 700),
           br(),
          includeMarkdown("ROC.md"),
           br(),
           br(),
    splitLayout(DT::dataTableOutput("buys"), DT::dataTableOutput("sells")),
    
           #htmlOutput("outliers"),
           br(),
           br(),
           DT::dataTableOutput("ROC_table"),
           br(),
           br(),
          includeMarkdown("glossary.md")
           
        #)
   # )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  observeEvent(input$seasons, {
    if(input$seasons == 2020){
      weeks_available <- c(1,2)
      preset <- 1
      updateNumericInput(session, inputId = "target_floor", value = 3)
    }else if(any(input$seasons %in% c(2014:2019))){
      weeks_available <- c(1:21)
      preset <- c(1:16)
    }
    updateSelectInput(session, inputId = "weeks", choices = weeks_available, selected = preset)
  })
  #output$logo <- renderImage("ContestedCatchLogo.png")
  ROC_data <- reactive(make_ROC(pbp_data = pbp_all, weeks = input$weeks, positions = c(input$positions, NA),
                                szn = input$seasons, target_min = input$target_floor))
  
  output$ROC_plot <- renderPlotly({
    
    return(
      plotly::ggplotly(ggplot(ROC_data() %>% rename(Receiver = receiver), 
                              aes(x = ROC, y = fantasy_points, label1 = TargetShare, 
                                  label2 = Targets,
                                  label3 = PlayerAirYards, 
                                  label4 = AirYardShare, 
                                  label5 = Receiver,
                                  label6 = season)) +
                         geom_point(aes(color = posteam), show.legend = F) +
                         scale_color_manual(values = teams_colors_logos$team_color, breaks = teams_colors_logos$team_abbr) +
                         scale_x_continuous(limits = c(0,max(100, max(ROC_data()$ROC))), breaks = seq(0,200,20)) +
                         labs(y = "0.5 PPR Fantasy Receiving Points", 
                              title = "Contested Catch: Receiver Opportunity Composite (ROC Score)", 
                              x = "ROC Score") +
                         theme_CC(),
                       tooltip = c("Receiver", "ROC", "fantasy_points", "PlayerAirYards", "Targets", 
                                   "TargetShare", "AirYardShare", "season"))
    )
  })
  
  # output$outliers <- renderText({
  #   get_ROC_outliers(ROC_data(), input$weeks,  n = 7)
  # })
  
  output$buys <- DT::renderDataTable({get_ROC_buys(ROC_data(), input$weeks)}, rownames = FALSE)
  output$sells <- DT::renderDataTable({get_ROC_sells(ROC_data(), input$weeks)}, rownames = FALSE)
  
  output$ROC_table <- DT::renderDataTable({
    ROC_table(ROC_data(), input$team[1])
  })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
