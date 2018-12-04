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
library(shinydashboard)
library(scales)
library(lubridate)
library(shinyjqui)

#I do not have second round data for all the Ivy matches. Reading in the rds files for the Harvard matches into shiny project.  

CSU_shiny <- read_rds("shiny_data/ CSU .rds")
CCSU_shiny <- read_rds("shiny_data/ CCSU .rds")
EMU_shiny <- read_rds("shiny_data/ EMU .rds")
FAIR_shiny <- read_rds("shiny_data/ FAIR .rds")
MICH_shiny <- read_rds("shiny_data/ MICH .rds")
MSU_shiny <- read_rds("shiny_data/ MSU .rds")
BC_shiny <- read_rds("shiny_data/ BC .rds")
BROWN1_shiny <- read_rds("shiny_data/ BROWN1 .rds")
BROWN2_shiny <- read_rds("shiny_data/ BROWN2 .rds")
COL1_shiny <- read_rds("shiny_data/ COL1 .rds")
COL2_shiny <- read_rds("shiny_data/ COL2 .rds")
CORN1_shiny <- read_rds("shiny_data/ CORN1 .rds")
CORN2_shiny <- read_rds("shiny_data/ CORN2 .rds")
DART1_shiny <- read_rds("shiny_data/ DART1 .rds")
DART2_shiny <- read_rds("shiny_data/ DART2 .rds")
NEU_shiny <- read_rds("shiny_data/ NEU .rds")
PENN1_shiny <- read_rds("shiny_data/ PENN1 .rds")
PRINCE1_shiny <- read_rds("shiny_data/ PRINCE1 .rds")
UMASSL_shiny <- read_rds("shiny_data/ UMASSL .rds")
YALE1_shiny <- read_rds("shiny_data/ YALE1 .rds")
YALE2_shiny <- read_rds("shiny_data/ YALE2 .rds")


# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "Harvard Volleyball"), 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Analytics Overview", tabName = "analytics", icon = icon("signal", lib = "glyphicon")),
      menuItem("Serving",tabName = "serving", icon = icon("th", lib = "glyphicon")),
      menuItem("Setting Location Charts", tabName = "setting_location", icon = icon("record", lib = "glyphicon")),
      menuItem("References", tabName = "references", icon = icon("book", lib = "glyphicon")),
      selectInput("data", "Choose Opponent:",
                  choices = c("Cleveland State University", "Central Connecticut State University", "Eastern Michigan University", "Fairfield",
                              "University of Michigan", "Michigan State University", "Boston College", "Brown 1", "Brown 2", "Columbia 1", "Columbia 2", "Cornell 1", "Cornell 2", 
                              "Dartmouth 1", "Dartmouth 2", "Northeastern University", "Penn 1", "Princeton 1", "University of Massachussets Lowell", "Yale 1", "Yale 2")
                  ),
      checkboxGroupInput("team", "Choose a team:", "")
    )),
  dashboardBody(
    tabItems(
    tabItem(tabName = "analytics", 
            h2("Match Summary Analytics"),
            hr(),
            fluidRow(
              column(6,
              box(title = "Attacking",
                  width = NULL,
                  height = 200,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  tableOutput("attacksummary")), 
              box(title = "Scoring by Rotation",
                  tableOutput("rotationpoints"), 
                  width = NULL), 
              box(title = "Receive", 
                  tableOutput("receivesummary"),
                  collapsible = TRUE, 
                  width = NULL)),
              column(6,
              box(height = 530,
                  width = NULL,
                       h2("Match Summary"), 
                  br(), 
                  h3(htmlOutput("matchsummary")), 
                  HTML('<center><img src="harvardathleticslogo.jpg" width="400"></center>')
                  
                     ), 
              box(height = 370, 
                  width = NULL, 
                  "about these metrics")
              )
              )
            ), 
    tabItem(tabName = "serving",
            h2("Serving Tab Content"), 
            fluidRow(
              box(
                selectizeInput("serve_evaluation", "Serve Result", 
                               choices = c("Negative, opponent free attack", 
                                           "Positive, opponent some attack", 
                                           "OK, no first tempo possible", 
                                           "Error", 
                                           "Ace", 
                                           "Positive, no attack"), 
                               selected = "Error"))),
            fluidRow(
              jqui_resizable(plotOutput("serve_map")))
            ),
    tabItem(tabName = "setting_location", 
            h2("Setting Content"),
            fluidRow(
              box(title = "Filters", 
                  solidHeader = TRUE, 
                  background = "red",
                  width = 12, 
                  selectInput("pass_result", "Pass Result", 
                              #have to have choices be codes because they have to apply to both reception and digging. Evaluation descriptions are not the same for the two skills 
                              choices = c("=","#", "!",
                                          "-", "+", "/"), 
                              selected = "#"), 
                  selectizeInput("phase", "Serve Receive/Transition",
                                 choices = c("Transition", "Reception")))), 
            fluidRow(
              box(title = "Setting Decisions", 
                  plotOutput("setting_location"), 
                  solidHeader = TRUE, 
                  width = 12))
            
            ), 
    tabItem(tabName = "references", 
            h2("References"), 
            br(),
           h3(p("This Shiny App was built by Maclaine Fields using DataVolley files from the Harvard Women's Volleyball 2018 Season. All data was obtained with permission of coaching staff.
Icons are from", 
a("Glyphicon.", href = "https://www.glyphicons.com"), "Original code can be found on", 
              a("Github.", href = "https://github.com/macfields/mfields_finalproject")),
            br(), 
            p("The inspiration for the layout and summary analytics of this app came from apps on", 
a("setting patterns", href = "https://apps.untan.gl/volleyset/ "),
"and", a("match analytics", href = "https://apps.untan.gl/dvrr/"), "created by Mark Lebedew, head coach of the Australian national men's team and Aluron Virtu,
            Michael Mattes, co-trainer and scout for GCDW Herrsching Volleyball Bundesliga, and Francesco Oleni of Chinese Senior Men’s National Team. "))

    )
)))
  

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  

  #creating reactive dataset such that whichever game the user chooses, the appropriate dataset is used throughout the match. 
  datasetInput <- reactive({
    switch(input$data, 
           "Cleveland State University" = CSU_shiny,
           "Central Connecticut State University" = CCSU_shiny,
           "Eastern Michigan University" = EMU_shiny,
           "Fairfield" = FAIR_shiny,
           "University of Michigan" = MICH_shiny,
           "Michigan State University" = MSU_shiny,
           "Boston College" = BC_shiny, 
           "Brown 1" = BROWN1_shiny, 
           "Brown 2" = BROWN2_shiny,
           "Columbia 1" = COL1_shiny,
           "Columbia 2" = COL2_shiny,
           "Cornell 1" = CORN1_shiny,
           "Cornell 2" = CORN2_shiny, 
           "Dartmouth 1" = DART1_shiny,
           "Dartmouth 2" = DART2_shiny, 
           "Northeastern University" = NEU_shiny, 
           "Penn 1" = PENN1_shiny,
           "Princeton 1" = PRINCE1_shiny,
           "University of Massachussets Lowell" = UMASSL_shiny,
           "Yale 1" = YALE1_shiny,
           "Yale 2" = YALE2_shiny
           )
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
     validate(
       need(input$team != "", "Please select a team.")
     )
     serves() %>% 
         filter(serving_team == input$team) %>% 
         filter(evaluation == input$serve_evaluation) %>% 
         #got this code from raymond ben's github. Some of the coordinates are incorrect, how would I fix this? 
         ggplot(aes(start_coordinate_x, start_coordinate_y, xend = end_coordinate_x, yend = end_coordinate_y, color = evaluation)) + 
         geom_point() + ggcourt(fixed_aspect_ratio = TRUE) +geom_segment() +  theme(legend.position="bottom", legend.direction = "vertical")
     
     
   })
   
   output$matchsummary <- renderText({
     
     #Ivy League games should show the Ivy League in the match summary, whereas the non-conference matches should show "non-conference". 
     #I make this text reactive by subsetting specific metadata from the summary of the datavolley file. The summary() function gives the duration, result, 
     #teams, league, and date of a match. By using paste, I am able to write a match summary depending on what opponent the user chooses. 
     
     #I want to include the score for the game. I do so by subsetting the summary of the dataset down to home team and visiting team scores. 
     # I can then use paste() to put together the scores in an (home-away, home-away, home-away) format that is typical for volleyball scoring. 
     # The issue is that not every match has the same number of sets. I need to past the scores only for the sets that were played. The length of the 
     # home team score variable is equal to the number of sets played, so I use an ifelse statement to paste the scores in the correct format depending on the 
     # length of score_home_team. 
     
     #I also want to include the winner in the summary. The winner with all more total points than the loser. So if the sum of score_home_team is greater than
     #sum of score_visiting_team, the home team won. If the sum of score_visiting_team is greater than the sum of score_home_team, the home team won.
     
     #Lastly, I also want to include the date on which the match was played. The date given in the summary is in different forms for some of the matches. I want to get the date
     #for all the matches in a uniform format, so I use the format() function. 
     
     result <- summary(datasetInput())[4]
     set_scores <- result$set_scores
     score_home_team <- set_scores$score_home_team
     score_visiting_team <- set_scores$score_visiting_team
     
     score <- 
       if (length(score_home_team) == 3) {
         paste(score_home_team[1], "-", score_visiting_team[1], ",", score_home_team[2], "-", score_visiting_team[2], ",", 
               score_home_team[3], "-", score_visiting_team[3])
       }
     
     else if (length(score_home_team) == 4) {
       paste(score_home_team[1], "-", score_visiting_team[1], ",", score_home_team[2], "-", score_visiting_team[2], ",", 
             score_home_team[3], "-", score_visiting_team[3], ",", score_home_team[4], "-", score_visiting_team[4])
     }
     
     else {
       paste(score_home_team[1], "-", score_visiting_team[1], ",", score_home_team[2], "-", score_visiting_team[2], ",", 
             score_home_team[3], "-", score_visiting_team[3], ",", score_home_team[4], "-", score_visiting_team[4], ",",
             score_home_team[5], "-", score_visiting_team[5])
     }
     
     winner <- 
       if (sum(score_home_team) > sum(score_visiting_team)) {
         summary(datasetInput())[[3]][[1]][[1]]
       }
     
     else {
       summary(datasetInput())[[3]][[1]][[2]]
     }
     
     date<- 
       if(input$data %in% c("Cleveland State University","Central Connecticut State University","Eastern Michigan University",
         "Fairfield","University of Michigan","Michigan State University","Brown 1","Brown 2","Penn 1",
         "Princeton 1","Yale 2"))
        {
         ydm(summary(datasetInput())[[1]])
       }
       else {
         ymd(summary(datasetInput())[[1]])
       }
     
       date <- format(date, '%m/%d/%Y')
     
     
     if(!input$data %in% c("Cleveland State University", "Central Connecticut State University", 
                          "Eastern Michigan University", "Fairfield", "University of Michigan", "Michigan State University", 
                          "Boston College", "Northeastern University", "University of Massachussets Lowell")) { 
     paste(
     "The duration of this game was", strong(summary(datasetInput())[[5]]), "minutes.",
     "The game was played in the Ivy League", 
     "on", date, "between", strong(summary(datasetInput())[[3]][[1]][[1]]), "and",
     strong(summary(datasetInput())[[3]][[1]][[2]]), ".", "The score was", score,",", "and", winner , "won.")
     }
     
     else {
       paste(
         "The duration of this game was", strong(summary(datasetInput())[[5]]), "minutes.", "This was a non-conference match played", 
         "on", date, "between", strong(summary(datasetInput())[[3]][[1]][[1]]), "and",
         strong(summary(datasetInput())[[3]][[1]][[2]]), ".", "The score was", score,",", "and",winner, "won.")
     }
   })
   
  
   output$attacksummary <- renderTable({
     ##If the user doesn't select a team, the plots won't show. But I don't want a red, bold error statement to show. I use validate
     # to customize the error message. 
     validate(
       need(input$team != "", "Please select a team.")
     )
     plays(datasetInput()) %>% 
       filter(skill == "Attack") %>% 
       filter(team %in% input$team) %>% 
       #Use a group_by and summary command to calculate the total number of attacks during transition and off serve recieve. 
       # Find the number of kills as well as the attack efficiency. Attack efficiency is the number of kills minus the number of 
       #errors or blocked balls divided by total attempts. 
       group_by(phase) %>% 
       summarize(Total = n(), 
                 Kills = sum(evaluation_code == "#" ), 
                 Errors = sum(evaluation_code == "="), 
                 efficiency = (sum(evaluation_code == "#") - sum(evaluation_code %in% c("=", "/"))), 
                 `Efficiency Rate` = efficiency/Total) %>% 
       select(phase,Total, Kills, Errors, `Efficiency Rate`) %>% 
       rename(
         "Phase" = phase, 
         "Total Attempts" = Total, 
         "Kills" = Kills
       )
   
   })
       
   output$receivesummary <- renderTable({
     validate(
       need(input$team != "", "Please select a team.")
     )
     plays(datasetInput()) %>% 
       filter(skill == "Reception") %>% 
       filter(team %in% input$team) %>% 
       group_by(skill_type, evaluation) %>% 
       count() %>% 
       spread(skill_type, n) %>% 
       replace(.,is.na(.),"0") %>% 
       rename(
         "Evaluation" = evaluation
       )
      
   })
   ## Code from raymondben datavolley github on how to create a heatmap.
   # But I also want to figure out where the setter sets if they are given a perfect pass. 
   output$setting_location <- renderPlot({
     validate(
       need(input$team != "", "Please select a team.")
     )
    attack_rate <- plays(datasetInput()) %>% 
      mutate( 
             lag_2skill = lag(skill, n = 2L), 
             lag_2skill_evaluation_code = lag(evaluation_code,n =2L), 
             lag_2skill_evaluation = lag(evaluation, n = 2L)) %>% 
       filter(skill == "Attack" & team %in% input$team) %>% 
      filter(lag_2skill_evaluation_code == input$pass_result) %>% 
      filter(phase == input$phase) %>% 
       group_by(start_zone) %>% 
       summarize(n_attacks = n()) %>% 
       mutate(rate = n_attacks/sum(n_attacks)) %>% 
       ungroup()
    
    #I choose to format the rate as a percent because that it is a bit easier to comprehend. It is easy to
    # understand what 47% of sets go to the outside hitter means. 
    attack_rate2 <- cbind(attack_rate, dv_xy(attack_rate$start_zone, end = "lower"))
  
    #Must set width argument so that rectangles do not automatically adjust. Need rectangles to be the same width 
    # regardless of whether or not there are observations in the neighboring zones. 
    if (!input$pass_result %in% c("/", "=")) {
      ggplot(attack_rate2, aes(x,y, fill = (100* attack_rate2$rate))) + geom_tile(aes(width = 1)) + ggcourt(court = "lower", labels = c(input$team)) + geom_text(aes(label = percent(attack_rate2$rate))) +
     scale_fill_continuous(name = "Percentage of Sets")
    }
    
    else {
      ggplot() + ggcourt(court = "lower") 
    }
    
    
   })
   ## Want to look at points scored in each rotation. Again, I want to take out the system codes and only look at true player touches. I 
   ## then filter by team. 
   output$rotationpoints <- renderTable ({
     
     validate(
       need(input$team != "", "Please select a team.")
     )
     
     hometeamrotations <- plays(datasetInput()) %>% 
       filter(!is.na(player_number)) %>% 
       filter(team == home_team) %>% 
       filter(skill == "Reception") %>% 
       group_by(home_setter_position) %>% 
       summarize(
         sideouts = sum(point_won_by == home_team),
         total = n(),
         sideoutpercentage = sideouts/total) %>% 
       rename(
         "Rotation" = home_setter_position, 
         "Sideouts" = sideouts,
         "Total Opponent Serves" = total, 
         "Sideout Percentage" = sideoutpercentage
       )
     
     visitingteamrotations <- plays(datasetInput()) %>% 
       filter(!is.na(player_number)) %>% 
       filter(team == visiting_team) %>% 
       filter(skill == "Reception") %>% 
       group_by(visiting_setter_position) %>% 
       summarize(
         sideouts = sum(point_won_by == home_team),
         total = n(),
         sideoutpercentage = sideouts/total) %>% 
       rename(
         "Rotation" = visiting_setter_position, 
         "Sideouts" = sideouts, 
         "Total Opponent Serves" = total, 
         "Sideout Percentage" = sideoutpercentage
       )
     
     if (input$team == plays(datasetInput())$home_team) {
       hometeamrotations
     }
   else if(input$team == plays(datasetInput())$visiting_team){
     visitingteamrotations
   }
    
     
  
       
   })
   
}
     


# Run the application 
shinyApp(ui = ui, server = server)

