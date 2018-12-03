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
library(DT)

dataset <- read_rds("example_file.rds")
summary_dataset <- summary(dataset)

CU_PENN_dataset <- read_rds("CU_PENN.rds")

harvard_bc <- read_rds("harvard_bc.rds")

harvard_bc_plays <- read_rds("harvard_bc_plays.rds")

# Define UI for application that draws a histogram
ui <- fluidPage( theme = shinytheme("slate"),
   
   # Application title
   titlePanel("Harvard Women's Volleyball 2018"),
   
   # Sidebar with a select input for match and checbox group for team. 
   sidebarLayout(
      sidebarPanel( width = 3, 
         selectInput("data",
                     "Choose Match:",
                     choices = c("CU vs. Penn", "Nova KBM Branik vs. Braslovče", "Harvard vs. Boston College")), 
         checkboxGroupInput("team", 
                     "Choose a team",
                     ""),
         selectizeInput("serve_evaluation", 
                            "Serve Result", 
                            choices = c("Negative, opponent free attack", 
                                        "Positive, opponent some attack", 
                                        "OK, no first tempo possible", 
                                        "Error", 
                                        "Ace", 
                                        "Positive, no attack"), 
                            selected = "Error") #How to not get error when nothing is chosen for evaluation?
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel( type = "tabs", 
                     tabPanel(title = "Match Information", value = 1, 
                              htmlOutput("matchsummary"), 
                              tableOutput("attacksummary")), 
                     tabPanel(title = "Statistics"),
                     tabPanel(title = "Serving", 
                              fluidRow(
                                column(6, 
                                       wellPanel(
                                         plotOutput("serve_plot", width = "100%"))
                                       ),
                                column(6, 
                                       wellPanel(plotOutput("serve_map"))
                                       ))),
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
server <- function(input, output, session) {

  datasetInput <- reactive({
    switch(input$data, 
           "CU vs. Penn" = CU_PENN_dataset, 
           "Nova KBM Branik vs. Braslovče" = dataset,
           "Harvard vs. Boston College" = harvard_bc )
  })
  
  #Got code from "https://stackoverflow.com/questions/21465411/r-shiny-passing-reactive-to-selectinput-choices"
  observe({
    updateCheckboxGroupInput(session, "team", 
                      choices = teams(datasetInput()), 
                      selected = teams(datasetInput()))
  })
  
  
## Should I create serving data here? 
  serves <- reactive ({
    plays(datasetInput()) %>% 
      filter(skill == "Serve") 
  })
  
   output$serve_plot <- renderPlot({
      # generate bins based on input$bins from ui.R
     serves() %>% 
     ggplot(aes(x = evaluation, fill = team)) + geom_bar(position = "dodge") + theme(axis.text.x=element_text(angle=65,hjust=1)) +
       theme(legend.position="bottom", legend.direction = "vertical")
   })
   
   output$serve_map <- renderPlot({
     
     if (input$data == "CU vs. Penn") {
       serves() %>% 
         filter(serving_team == input$team) %>% 
         filter(evaluation == input$serve_evaluation) %>% 
         #got this code from raymond ben's github. Some of the coordinates are incorrect, how would I fix this? 
         ggplot(aes(start_coordinate_x, start_coordinate_y, xend = end_coordinate_x, yend = end_coordinate_y, color = evaluation)) + 
         geom_point() + ggcourt() +geom_segment() +  theme(legend.position="bottom", legend.direction = "vertical")
     }
     
     else {
     }

   })
   # Possibly include leaflet that shows where the game was played. 
   output$matchsummary <- renderText({
     paste(h1("Match Summary"), br(), 
     "The duration of this game was", strong(summary(datasetInput())[[5]]), "minutes.",
     "The game was played in the", strong(summary(datasetInput())[[2]]), "league", 
     "on", strong(summary(datasetInput())[[1]]), "between", strong(summary(datasetInput())[[3]][[1]][[1]]), "and",
     strong(summary(datasetInput())[[3]][[1]][[2]]), ".", "The Winner of the game was...")
     #how to get this period to space without a space? Deal with non-league games. Insert game winner. 
   })
   
   output$attacksummary <- renderTable(
     plays(datasetInput()) %>% 
       filter(skill == "Attack") %>% 
       filter(team %in% input$team) %>% 
       #Use a group_by and summary command to calculate the total number of attacks during transition and off serve recieve. 
       # Find the number of kills as well as the attack efficiency. Attack efficiency is the number of kills minus the number of 
       #errors or blocked balls divided by total attempts. 
       group_by(phase) %>% 
       summarize(total = n(), 
                 kills = sum(evaluation_code == "#" ), 
                 errors = sum(evaluation_code == "="), 
                 efficiency = (sum(evaluation_code == "#") - sum(evaluation_code %in% c("=", "/"))), 
                 rate = efficiency/total
            
                          
                               
       ))
   
     
}

# Run the application 
shinyApp(ui = ui, server = server)

