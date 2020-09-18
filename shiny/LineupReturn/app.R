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

wks <- c(2)

#all_lineups <- map_dfr(wks, function(x){read_csv(glue::glue("week_{x}_lineups_800_proj.csv")) %>%
 #   mutate(week = x)})

all_lineups <- read_csv("week_2_2020_proj.csv") %>%
  bind_rows(read_csv("week_2_2020_4_2_proj.csv")) %>%
  #bind_rows(read_csv("week_1_2020_500_A_proj.csv")) %>%
  #bind_rows(read_csv("week_1_2020_500_B_proj.csv")) %>%
  rename(PROJ = P) %>%
  select(QB, RB, RB_1, WR, WR_1, WR_2, TE, FLEX, DST, PROJ) %>%
  mutate(week = 2)

disp_lineups <- function(lineups, wk, qb = "None", rb = "None", wr = "None", wr2 = "None"){
  
lineups <- lineups %>% filter(week == wk)

  if(qb == "None")qb<- NULL
  if(rb == "None")rb<- NULL
  if(wr == "None")wr<- NULL
  if(wr2 == "None")wr2 <- NULL
  
  if(!is.null(qb)){
    lineups <- lineups %>%
      filter(QB == qb)
  }
  
  if(!is.null(rb)){
    lineups <- lineups %>%
      filter(RB == rb | RB_1 == rb | FLEX == rb)
  }
  
  if(!is.null(wr)){
    lineups <- lineups %>%
      filter(WR == wr | WR_1 == wr | WR_2 == wr | FLEX == wr)
  }
  if(!is.null(wr2)){
    lineups <- lineups %>%
      filter(WR == wr2 | WR_1 == wr2 | WR_2 == wr2 | FLEX == wr2)
  }
  
  # lineups <- lineups %>%
  #   top_n(15, PROJ)
  return(lineups)
  
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Contested Catch DraftKings Lineups"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          includeMarkdown("instructions.md"),
          selectInput("week", "Week", choices = c(wks), selected = 2),
            selectInput("quarterback", "Choose QB:", choices = c("None", unique(all_lineups$QB))),
            selectInput("runningback", "Choose RB:", choices = c("None", unique(c(all_lineups$RB, all_lineups$RB_1)))),
            selectInput("wide_receiver", "Choose WR:", choices = c("None",
                                                                   unique(c(all_lineups$WR,
                                                                            all_lineups$WR_1,
                                                                            all_lineups$WR_2)))),
          selectInput("wide_receiver2", "Choose WR:", choices = c("None",
                                                                 unique(c(all_lineups$WR,
                                                                          all_lineups$WR_1,
                                                                          all_lineups$WR_2)))),
            width = 2
        ),

        # Show a plot of the generated distribution
        mainPanel(
           DT::dataTableOutput("lineup_table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
avail_lineups <- reactive(disp_lineups(all_lineups, 
                                       input$week, 
                                       input$quarterback, 
                                       input$runningback, 
                                       input$wide_receiver,
                                       input$wide_receiver2))

observeEvent({
  input$week
  input$runningback
  input$wide_receiver
  input$wide_receiver2
},{
  temp_lineups <- disp_lineups(all_lineups, 
                               input$week, 
                               "None", 
                               input$runningback, 
                               input$wide_receiver,
                               input$wide_receiver2)
  quarterbacks <- c("None", unique(temp_lineups$QB))
  updateSelectInput(session, "quarterback", choices = quarterbacks, selected = input$quarterback)
})

observeEvent({
  input$week
  input$quarterback
  input$wide_receiver
  input$wide_receiver2
},{
  temp_lineups <- disp_lineups(all_lineups, 
                               input$week, 
                               input$quarterback, 
                               "None", 
                               input$wide_receiver,
                               input$wide_receiver2)
  running_backs <- c("None", unique(c(temp_lineups$RB, temp_lineups$RB_1)))
  updateSelectInput(session, "runningback", choices = running_backs, selected = input$runningback)
})

observeEvent({
  input$week
  input$quarterback
  input$runningback
  input$wide_receiver2
},{
  temp_lineups <- disp_lineups(all_lineups, 
                               input$week, 
                               input$quarterback, 
                               input$runningback,
                               wr2 = input$wide_receiver2)
  
  wide_receivers <- c("None", unique(c(temp_lineups$WR,
                                       temp_lineups$WR_1,
                                       temp_lineups$WR_2)))
  updateSelectInput(session, "wide_receiver", choices = wide_receivers, selected = input$wide_receiver)
})

observeEvent({
  input$week
  input$quarterback
  input$runningback
  input$wide_receiver
},{
  temp_lineups <- disp_lineups(all_lineups, 
                               input$week, 
                               input$quarterback, 
                               input$runningback, 
                               input$wide_receiver)
  
  wide_receivers <- c("None", unique(c(temp_lineups$WR,
                                       temp_lineups$WR_1,
                                       temp_lineups$WR_2)))
  updateSelectInput(session, "wide_receiver2", choices = wide_receivers, selected = input$wide_receiver2)
})
    
  output$lineup_table <- DT::renderDataTable(avail_lineups() %>% top_n(25, PROJ) %>% select(-week))
  
}

# Run the application 
shinyApp(ui = ui, server = server)
