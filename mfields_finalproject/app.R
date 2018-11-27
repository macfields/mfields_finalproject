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
library(datavolley)
library(shinythemes)
library(tools)

dataset <- read_rds("example_file.rds")
summary_dataset <- summary(dataset)

CU_PENN_dataset <- read_rds("CU_PENN.rds")

# Define UI for application that draws a histogram
ui <- fluidPage( theme = shinytheme("slate"),
   
   # Application title
   titlePanel("Harvard Women's Volleyball 2018"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel( 
         selectInput("data",
                     "Choose Match:",
                     choices = c("CU vs. Penn", "Nova KBM Branik vs. Braslovče"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel( type = "tabs", 
                     tabPanel(title = "Match Information", value = 1, 
                              htmlOutput("matchsummary")), 
                     tabPanel(title = "Stastistics"),
                     tabPanel(title = "Serving", 
                              plotOutput("serve_plot")), 
                     tabPanel(title = "Data"),
                     tabPanel(title = "References", 
                              "This Shiny App was built by Maclaine Fields using DataVolley files from the Harvard Women's Volleyball 2018 Season. All data was obtained with permission of coaching staff.",
                              br(), br(), 
                              "Original code can be found on", 
                              a("Github", href = "https://github.com/macfields/mfields_finalproject")) #unable to add image here 
          
        )
         
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  datasetInput <- reactive({
    switch(input$data, 
           "CU vs. Penn" = CU_PENN_dataset, 
           "Nova KBM Branik vs. Braslovče" = dataset)
  })
  
## Should I create serving data here? 
  serves <- reactive ({
    plays(datasetInput()) %>% 
      filter(skill == "Serve") %>% 
      select(point_id, code, team, player_number, skill_type, evaluation_code, 
             evaluation, start_zone, end_zone, special_code, home_team_score, 
             visiting_team_score, home_setter_position, visiting_setter_position, 
             point_won_by, serving_team)
  })
  
   output$serve_plot <- renderPlot({
      # generate bins based on input$bins from ui.R
     serves() %>% 
     ggplot(aes(x = evaluation, fill = team)) + geom_bar(position = "dodge") + coord_flip()
   })
   
   output$serve_map <- renderPlot({
     serves() %>% 
       
   })
   
   output$matchsummary <- renderText({
     paste(h1("Match Summary"), br(), 
     "The duration of this game was", strong(summary(datasetInput())[[5]]), "minutes.",
     "The game was played in the", strong(summary(datasetInput())[[2]]), "league", 
     "on", strong(summary(datasetInput())[[1]]), "between", strong(summary(datasetInput())[[3]][[1]][[1]]), "and",
     strong(summary(datasetInput())[[3]][[1]][[2]]), ".", "The Winner of the game was...")
     #how to get this period to space without a space? Deal with non-league games. Insert game winner. 
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

