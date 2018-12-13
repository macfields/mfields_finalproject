# This R script file also details the organization of the repository and the
# libraries that are needed to run the Shiny App.

#Repo Contents
#1. README.md - README file for this project. 
#2. 2018dvwfiles - folder that contains the 21 datavolley dvw files. 
#3. mfields_finalproject - Shiny application folder. Contains the app script, and two more folders. 
     #3a. shiny_data - folder that contains the rds files used in the shiny app. 
     #3b www - folder that contains that harvard athletics logo image. 
#4. process.R - R script in which I prepared the raw data for use in the shiny app. 
#5. redatavolleyfiles.zip - zip file of the orginal data. 

#Libraries needed: 
#devtools
#datavolley
#tidyverse
#scales
#fs
#lubridate
#shiny 
#shinythemes
#tools
#DT
#shinydashboard
#shinyjqui



library(devtools)
library(datavolley)
library(tidyverse)
library(scales)
library(fs)
library(lubridate)

#unzipping file and putting datavolley files in folder. 
unzip("redatavolleyfiles.zip", exdir = "2018dvwfiles/")

#I know there must be an easier name to read in these datavolley files. 
#However, given that there are only 21, I can do it by hand individually. Since the Harvard Women's 
#Volleyball team plays all Ivy League opponents twice, I designate Ivy League matches with a 1 or 2, 
#where matches ending in 1 are first round matches, and those ending in 2 are second round. I don't 
#insert technical timeouts because it doesn't matter for my analysis. 

CSU <- read_dv("2018dvwfiles/&2018-09-01 63826 CSU-HARV(VM).dvw", insert_technical_timeouts = FALSE)
MICH <- read_dv("2018dvwfiles/&2018-09-01 66885 MICH-HARV(VM).dvw", insert_technical_timeouts = FALSE)
EMU <- read_dv("2018dvwfiles/&2018-09-02 59573 EMU-HARV(VM).dvw", insert_technical_timeouts = FALSE)
CCSU <- read_dv("2018dvwfiles/&2018-09-07 66875 HARV-CCSU(VM).dvw", insert_technical_timeouts = FALSE)
MSU <- read_dv("2018dvwfiles/&2018-09-08 62690 HARV-MSU(VM).dvw", insert_technical_timeouts = FALSE)
FAIR <- read_dv("2018dvwfiles/&2018-09-08 66876 FAIR-HARV(VM).dvw", insert_technical_timeouts = FALSE)
BC <- read_dv("2018dvwfiles/&2018-09-13 66877 HARV-BC(VM).dvw", insert_technical_timeouts = FALSE)
NEU <- read_dv("2018dvwfiles/&2018-09-14 66231 NEU-HARV(VM).dvw", insert_technical_timeouts = FALSE)
UMASSL <- read_dv("2018dvwfiles/&2018-09-15 66878 HARV-UMassL(VM).dvw", insert_technical_timeouts = FALSE)
DART1 <- read_dv("2018dvwfiles/&2018-09-21 60794 DART-HARV(VM).dvw", insert_technical_timeouts = FALSE)
CORN1 <- read_dv("2018dvwfiles/&2018-09-28 45079 CORN-HARV(VM).dvw", insert_technical_timeouts = FALSE)
COL1 <- read_dv("2018dvwfiles/&2018-09-29 63605 COL-HARV(VM).dvw", insert_technical_timeouts = FALSE)
PENN1 <- read_dv("2018dvwfiles/&2018-10-05 66588 HARV-PENN(VM).dvw", insert_technical_timeouts = FALSE)
PRINCE1 <- read_dv("2018dvwfiles/&2018-10-06 66880 HARV-PRIN(VM).dvw", insert_technical_timeouts = FALSE)
BROWN1<- read_dv("2018dvwfiles/&2018-10-12 62190 BROWN-HARV(VM).dvw", insert_technical_timeouts = FALSE)
YALE1 <- read_dv("2018dvwfiles/&2018-10-13 63541 YALE-HARV(VM).dvw", insert_technical_timeouts = FALSE)
DART2 <- read_dv("2018dvwfiles/&2018-10-19 66881 HARV-DART(VM).dvw", insert_technical_timeouts = FALSE)
COL2 <- read_dv("2018dvwfiles/&2018-10-26 79208 HARV-COL(VM).dvw", insert_technical_timeouts = FALSE)
CORN2 <- read_dv("2018dvwfiles/&2018-10-27 45088 HARV-CORN(VM).dvw", insert_technical_timeouts = FALSE)
YALE2 <- read_dv("2018dvwfiles/&2018-11-02 66883 HARV-YALE(VM).dvw", insert_technical_timeouts = FALSE)
BROWN2 <- read_dv("2018dvwfiles/&2018-11-03 66884 HARV-BROWN(VM).dvw", insert_technical_timeouts = FALSE)



# I know I should complete a lot of my analysis in my process.R file, but I want to use the datavolley objects in a pretty raw form in the shiny app 
# because the raw form contains important metadata. So, I write RDS files to my shiny app directory and 
# do most of my data analysis there. I create a function to do so because there are a lot of files. 

#This function takes the datavolley objecft as an argument, and write an RDS file in my shiny app directory. 

# The combination deparse(substitute(x)) pulls out an objects name. I got this code from 
# https://stackoverflow.com/questions/37646382/how-do-deparse-substitute-work-to-allow-access-to-an-objects-name. 

write_dv_rds <- function(dv_object) {
  x <- paste("mfields_finalproject/shiny_data/", deparse(substitute(dv_object)),".rds")
  write_rds(dv_object, path = x)
}

write_dv_rds(CSU)
write_dv_rds(MICH)
write_dv_rds(EMU)
write_dv_rds(CCSU)
write_dv_rds(MSU)
write_dv_rds(FAIR)
write_dv_rds(BC)
write_dv_rds(NEU)
write_dv_rds(UMASSL)
write_dv_rds(DART1)
write_dv_rds(CORN1)
write_dv_rds(COL1)
write_dv_rds(PENN1)
write_dv_rds(PRINCE1)
write_dv_rds(BROWN1)
write_dv_rds(YALE1)
write_dv_rds(DART2)
write_dv_rds(COL2)
write_dv_rds(CORN2)
write_dv_rds(YALE2)
write_dv_rds(BROWN2)


## The following code is not used in my analysis or shiny app. I just wanted to get a sense of what the datavolley objects, their summary, and their plays dataset look like. 
brown_plays <- plays(BROWN1) %>% filter(!is.na(player_name))

glimpse(brown_plays)
names(brown_plays)
summary(BROWN1)


brown_serves <- brown_plays %>% 
  filter(skill == "Serve") %>% 
  select(point_id, code, team, player_number, skill_type, evaluation_code, evaluation, start_zone, end_zone, special_code, home_team_score, visiting_team_score, home_setter_position, visiting_setter_position, point_won_by, serving_team) 

#  A little bit about the layout of this project: 
#The zip file with all the datavolleyfiles is in the main project directory, mfields_finalproject. 
# The datavolley files individually are in thie 2018dvwfiles folder. The shiny app is in the mfields_finalproject folder within mfields_finalproject. 
# Within the shiny app, the .rds files that are used in the app are all in the shiny_data folder. 
# the www folder contains the Harvard Athletics Image. 
