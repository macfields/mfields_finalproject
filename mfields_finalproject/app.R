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
library(datavolley) #Allows for the datavolley files to be read and for the use of specific datavolley commands. 
library(shinythemes)
library(tools)
library(DT)
library(shinydashboard)
library(scales)
library(lubridate)
library(shinyjqui) #This package allows the user to resize elements in the Shiny Dashboard. 

#Reading in the rds files for the Harvard matches into shiny project.I don't have second round data for all of the Ivy League matches,
# but I read in those for which I do. Rounds are noted by a number after the Ivy League competitor's name. Just like in my process.R file, I know
# there is an easier way to read these all in by writing a function. However, there are only 21 files, so it is not too bad to do it individually. 

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


# Define UI for application

## I wanted to create a shinydashboard because it is more visually appealing than the typical RShiny App. I create tabs in the sidebar 
# for Analytics Overview, Serving, Setting Location Charts, and References. I include the selectors for team and match in the sidebarMenu because I did not 
#want to have to include the selectors in all the tabs. I use icons from the glyphicon library for the tabs. There were some cool icons in the font-awesome library, 
#but after some research, I found that this version of R does not yet support the latest version of font-awesome. 

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
                              "Dartmouth 1", "Dartmouth 2", "Northeastern University", "Penn 1", "Princeton 1", "University of Massachussets Lowell", "Yale 1", "Yale 2"), 
                  selected = "Northeastern University"
                  ),
      checkboxGroupInput("team", "Choose a team:", "")
    )),
  
  ## Here, I'm specifying the content for each tab. Even though I wasn't initially too familiar with shinydashboard, these tabs behave similarly 
  # to tabs in a typical shiny app. I use fluidRow, columns, and boxes to organize each page. 
  
  #For the "Match Summary Analytics" page, I include a
  #few summary analytics and plots, as well as a written match summary. I display a table that shows sideout results by rotation, a table of serve receive results, and a table of attack percentages.
  #There a plenty more analytics that could be added to this page, but I wanted to choose a few that I thought 
  #were the most important to understanding the overall outcome of a match. 
  
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
                  htmlOutput("hometeam_name"),
                  tableOutput("home_rotationpoints"),
                  br(), 
                  htmlOutput("visitingteam_name"), 
                  tableOutput("visiting_rotationpoints"),
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
                  h5(htmlOutput("matchsummary")), 
                  
                  # I included the logo for purely aesthetic purposes. There was a lot of whitespace that I didn't like. I should probably check on the legal
                  # restrictions around using the Harvard Athletics Logo. 
                  
                  HTML('<center><img src="harvardathleticslogo.jpg" width="400"></center>')
                  
                     ), 
              #Volleyball metrics can be a little confusing for those are not familiar, so I wanted to be sure that I explained the metrics
              #that are used on my summary analysis page. It may be helpful in the future to provide a link to a webpage that explains the metrics in depth. I adjust the size of the box
              # to ensure that all of the text is able to fit inside. 
              box(height = 430, 
                  width = NULL, 
                  title = "Explanations of Metrics", 
                  solidHeader = TRUE,
                  strong("Attacking"), 
                  p("This table looks at overall attacking metrics for the chosen match, filtered by whether the team is attacking
                    of a serve in serve receive or a dig in transition. Total Attempts are the total number of attacks in that phase. Kills
                    are the total number of attacks that directly resulted in a point, while Errors are the number of hitting errors in that phase.
                    The Efficiency Rate is the Attack Efficiency Rate, a common volleyball metric. The Attack Efficiency Rate is equal to 
                    (# of Kills - #Errors)/Total Attempts."),
                  br(), 
                  strong("Scoring by Rotation"), 
                  p("This table looks at the Sideout Percentage for each rotation for each team. Sideouts are when a team wins a point off the other teams serve. The sideout percentage is
                    the percentage of sideouts in each rotation."),
                  br(), 
                  strong("Receive"), 
                  p("This table looks at serve receive results for each type of serve. It shows the number of each quality of pass
                    dependent on the serve type.")
                  
                  )
              )
              )
            ), 
    
    #Here, I'm inputing the content for the Serving Tab. I allow the user to filter by serve result, and then see the trajectory of the serves. One thing that was really challenging about this 
    # tab's content was sizing the chart correctly. There are no direct size arguments to the ggcourt() function that I use to create the serving map, but the chart will scale based on the size of the box. 
    # I found it easier to use the jqui_resizable function, which allows the user to adjust the size of the box. As the size of the box changes, so does the size of the plot. Without this function, I 
    # was repeatedly getting a plot that was way too small. 
    
    tabItem(tabName = "serving",
            h2("Serving Trajectories"), 
            fluidRow(
              box(width = 12,
                selectizeInput("serve_evaluation", "Serve Result", 
                               choices = c("Negative, opponent free attack", 
                                           "Positive, opponent some attack", 
                                           "OK, no first tempo possible", 
                                           "Error", 
                                           "Ace", 
                                           "Positive, no attack"), 
                               selected = "Error"))),
            fluidRow(
              box(width = 12,
                  title = "About This Chart", 
                  solidHeader = TRUE, 
                  
                  # I include this because it may not be intuitive to non-volleyball users what exactly this plot means. I also include a short insight about serving location so that non-volleyball users
                  # see what exactly this plot is showing. 
                  
                p("This graph shows the starting and ending location of serves for each team in a chosen match. 
                The starting and ending location of serves are given by coordinates in the volleymetrics files for each game. 
                  Serving locations trajectories can be filtered by serve result. The dot signifies the starting end of the trajectory. From this chart, it is clear that serves that result in 
                  a poor pass by the other team are usually not to the center of the court. On the other hand, serves that result in a good pass by the other team are usually to the zones in the center of the court")
              )
            ),
            fluidRow(
              box(width = 12,
              jqui_resizable(plotOutput("serve_map"))))
            ),
    
    ##Here, I'm inputting the content for the setting tendencies tab. I allow the user to filter by the result of the pass as well as transition/reception. This way, the chart shows setter 
    #tendencies on specific passes in each phase. There is more information about the creation of this plot in the server. 
    
    tabItem(tabName = "setting_location", 
            h2("Setting Tendencies"),
            fluidRow(
              box(title = "Filters", 
                  solidHeader = TRUE, 
                  width = 12, 
                  selectInput("pass_result", "Pass Result", 
                              
                              #Choices have to be evaluation codes because they have to apply to both reception and digging. Evaluation descriptions are not the same for the two skills. However, 
                              # I can edit choices such that the choices the user sees are undertandable (not in the code form). 
                              
                              choices = c("Error" = "=",
                                          "Perfect Pass" = "#",
                                          "Ok, no first tempo possible"="!",
                                          "Negative, limited attack" ="-", 
                                          "Positive, more than one attack possible"="+",
                                          "Overpass"="/"), 
                              selected = "#"), 
                  selectizeInput("phase", "Serve Receive/Transition",
                                 choices = c("Transition", "Reception"), 
                                 selected = "Reception"))),
            
            ## Again, I wanted to describe what exactly this chart shows and provide some short insights for non-volleyball users. I do so in another box for organizational/aesthetic purposes.
            
            fluidRow(
              box(title = "About This Chart", 
                  width = 12, 
                  solidHeader = TRUE,
                  p("This chart shows the percentage of sets going to a specific zone for each team. This is a model of setting decisions, and can be filtered by the 
                    quality of the pass. For instance, if #(Perfect Pass) is chosen, the chart shows where the setter is likely to set off a perfect pass. The chart shows that most teams are
going to set their middles and right sides on a perfect pass. This is because these are more challenging sets."))
            ),
            fluidRow(
              box(title = "Setting Decisions", 
                  
                  #Again, I use jqui_resizable to that the user can adjust the size of the plot. This was one way to get around the large amounts of white space in the shinydashboard, and also 
                  #improves the user experience. 
                  
                  jqui_resizable(plotOutput("setting_location")), 
                  solidHeader = TRUE, 
                  width = 12))
            
            ), 
    
    # Here, I'm inputting the content forl the references tab. I included a references tab so that I could cite where my data, icons, and inspiration came from. 
    # I include links to the icon website, as well as 
    # the professional volleyball analytics webpages so that the user can visit these. I also provide a link to my github repisitory so that the user can look at my original code. 
    
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
  

  #Here I create a reactive dataset such that whichever game the user chooses, the appropriate dataset is used throughout the app. I wanted to create a reactive dataset insteadd of using several filters
  # for clarity purposes. 
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
  
  #Got this code idea from "https://stackoverflow.com/questions/21465411/r-shiny-passing-reactive-to-selectinput-choices". This allows the Checkbox for team to change depending
  # on the match that the user selects. This way the user can only select teams from their chosen match. 
  
  observe({
    updateCheckboxGroupInput(session, "team", 
                      choices = teams(datasetInput()), 
                      selected = teams(datasetInput()))
  })
  
  
## Here I create a reactive serves dataset. By using the function plays() on datasetInput(), I can get data on each touch throughout the chosen match. I also want to filter out the system observations from 
  # the serves dataset. When I run plays() on a datavolley object, I got a dataset where each observation is a touch. However, the dataset also includes system observations, which our observations in between 
  # groups of touches that include metadata like video time. I do not need this. These observations also have NA for player_name. No other observations have NA for player_name. I filter out observations where player_name is NA.
  
  serves <- reactive ({
    plays(datasetInput()) %>% 
      filter(!is.na(player_name)) %>% 
      filter(skill == "Serve") 
  })
  
  ## Here I create the serving map that is shown in the serving tab. 
  
  ##The user must select a team in order for the serving map to appear. If they do not, an error will appear. 
  # I want to customize this error, so I use validate to create a custom error message. 
  
    output$serve_map <- renderPlot({
     validate(
       need(input$team != "", "Please select a team.")
     )
     serves() %>% 
         filter(serving_team == input$team) %>% 
         filter(evaluation == input$serve_evaluation) %>% 
       
         #Got the beginnings of this code from raymond ben's github at https://github.com/raymondben/datavolley. This plots the start coordinates and ending 
       # coordinates of a serve on a full volleyball court. The coordinates are connected by a segment (geom_segment). I set the fixed_aspect_ratio to true in the 
       # ggcourt() function because I want to be sure that the ratoi of the court dimensions is consistent regardless of the size of the plot. 
       
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
     "The duration of this game was", summary(datasetInput())[[5]], "minutes.",
     "The game was played in the Ivy League", 
     "on", date, "between", summary(datasetInput())[[3]][[1]][[1]], "and",
     summary(datasetInput())[[3]][[1]][[2]], ".", "The score was", score,",", "and", winner , "won.")
     }
     
     else {
       paste(
         "The duration of this game was", summary(datasetInput())[[5]], "minutes.", "This was a non-conference match played", 
         "on", date, "between", summary(datasetInput())[[3]][[1]][[1]], "and",
         summary(datasetInput())[[3]][[1]][[2]], ".", "The score was", score,",", "and",winner, "won.")
     }
   })
   
  
   #Here I'm creating the attacksummary plot to be shown on the Summary Analytics page. 
   
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
       
       # I want the names to be visually appealing in the final plot. 
       
       rename(
         "Phase" = phase, 
         "Total Attempts" = Total, 
         "Kills" = Kills
       )
   
   })
       
   #Here, I'm creating the receivesummary plot that is shown of the Summary Analytics page. 
   
   # Again, I want to prompt the user to select a team using a customized error message.
   
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
       
       # For some games, there may not be any of a certain type of reception. I don't want this to show up as NA in my plot. I want it to show up as O.  
       
       replace(.,is.na(.),"0") %>% 
       rename(
         "Evaluation" = evaluation
       )
      
   })
   
   ## Creating the setter tendencies map that is shown in the Setter Tendencies tab. 
   
   ## Beginnings of this code creating the setter decisions map came from raymondben's github. He showed how to create a heatmap.
   
   #But I also want to figure out where the setter sets after allowing the user to filter for phase and pass quality. 
   
   output$setting_location <- renderPlot({
     
     # Again, I create my own custom error message. 
     
     validate(
       need(input$team != "", "Please select a team.")
     )
    attack_rate <- plays(datasetInput()) %>% 
      
      ## I use the start_zone for attacks as an indicator of the position to which the setter set. I do this because after examining several datasets, the
      ## start_zone of attacks is more accurate than the end_zones of the set. T attack start_zone is consistent with the attack combination code and attack description, while the set end zone is not. I'm not sure why. 
      # For instance, when the outside hitter hits in zone 4, the hit start_zone is 4, but the set end zone is not 4. therefore I think that the attack start_zone variable
      # gives a more accurate description of set locations. I also compared the play-by-play analysis in the datasets with video I have from each game.
      
      
      # I need to know the quality of the pass. Since I'm looking at the start_zone of attacks, I only looking at observations where the skill is attack. However, 
      # since I need to know the quality of the pass, I also need to know the evaluation_code and evaluation for the touch that came two touches before the attack. One 
      # touch before the attack is the set, two touches before the attack is the reception or dig. I do this by creating lagged variables. 
      
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
    
    # By running dv_xy on the attack_rate$start_zone, I create x and y coordinates for each start zone. I bind this with the attack_rate data, so that I have coordinates for each start zone. 
    # I need the coordinates for plotting. 
    attack_rate2 <- cbind(attack_rate, dv_xy(attack_rate$start_zone, end = "lower"))
  
    
    # Must set width argument so that rectangles do not automatically adjust. Need rectangles to be the same width 
    # regardless of whether or not there are observations in the neighboring zones. 
    
    #I choose to format the rate as a percent because that it is a bit easier to comprehend and more aesthetic. 
    
    ## I use an ifelse statement because if the pass is an error or resulted in no attack, then it does not make sense to show theset distribution because no attack followed. If the pass
    # resulted in no attack or was an error, I just show am blank court. 
    
    if (!input$pass_result %in% c("/", "=")) {
      ggplot(attack_rate2, aes(x,y, fill = (100* attack_rate2$rate))) + geom_tile(aes(width = 1)) + ggcourt(court = "lower", labels = c(input$team)) + geom_text(aes(label = percent(attack_rate2$rate)), color = "white") +
     scale_fill_continuous(name = "Percentage of Sets")
    }
    
    else {
      ggplot() + ggcourt(court = "lower") 
    }
    
    
   })
   
   
   ## Here, I create the plot that shows scoring by rotation. Want to look at points scored in each rotation.
 
   output$home_rotationpoints <- renderTable ({
     
  
     
     # Again, I want to take out the system codes and only look at true player touches. I 
     ## then filter by team. 
     
     hometeamrotations <- plays(datasetInput()) %>% 
       filter(!is.na(player_number)) %>% 
       filter(team == home_team(datasetInput())) %>% 
       
       #I'm only looking at observations that were are service receptions. 
       
       filter(skill == "Reception") %>% 
       
       #Here, I'm grouping by rotation. Since I already filtered out observations where team == visiting_team, I can use 
       # home_setter_position as an indicator of the home teams rotation. 
      
       group_by(home_setter_position) %>% 
       
       #Create sideout, total, and sideoutpercentage variables. Sideouts are the sum of all observations where the home_team won. If 
       # the home_team was receiving and won, it means they sided out. I calculate a sideout percentage, a common volleyball metric. 
       
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
     
     
  
       
   })
   
   #repeating same steps as above, but for visiting team. 
   
   
   output$visiting_rotationpoints <- renderTable({
     

     visitingteamrotations <- plays(datasetInput()) %>% 
       filter(!is.na(player_number)) %>% 
       filter(team == visiting_team) %>% 
       filter(skill == "Reception") %>% 
       group_by(visiting_setter_position) %>% 
       summarize(
         sideouts = sum(point_won_by == visiting_team),
         total = n(),
         sideoutpercentage = sideouts/total) %>% 
       rename(
         "Rotation" = visiting_setter_position, 
         "Sideouts" = sideouts, 
         "Total Opponent Serves" = total, 
         "Sideout Percentage" = sideoutpercentage
       )
     
   })
   
   #Creating HTML output of the home team name and visiting team name to use in the Scoring by Rotation 
   #box in the Analytics Overview tab. 
   
   output$hometeam_name <- renderText ({
     home_team(datasetInput())
   })
   
   output$visitingteam_name <- renderText ({
     visiting_team(datasetInput())
   })
   
}
     


# Run the application 
shinyApp(ui = ui, server = server)

