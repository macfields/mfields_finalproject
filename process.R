library(devtools)
library(datavolley)
library(tidyverse)
library(scales)
library(fs)
library(lubridate)


#Obtained DataVolley files from Jared Goldberg, Assistant Harvard Women's Volleyball Coach. 
unzip("redatavolleyfiles.zip", exdir = "2018dvwfiles")

#Delete zipped file now that I no longer need it. 
file.remove("redatavolleyfiles.zip")

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



#I want to use the datavolley objects in a pretty raw form in the shiny app because the raw form contains important metadata. 
# One change, however, that I need to make is that the dates are not formatted the same in all of the datavolley files. 
# So, I write RDS files to my shiny app directory and do most of my data analysis there. I create a function to do so because there
#are a lot of files and my fingers hurt.


# example_file <- dv_example_file(choice = 1) %>% 
#   read_dv(insert_technical_timeouts = FALSE)


write_dv_rds <- function(dv_object) {
  x <- paste("mfields_finalproject/", deparse(substitute(dv_object)),".rds")
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

example_plays <- plays(example_file) 

#
example_serves <- example_plays %>% 
  filter(skill == "Serve") %>% 
  select(point_id, code, team, player_number, skill_type, evaluation_code, evaluation, start_zone, end_zone, special_code, home_team_score, visiting_team_score, home_setter_position, visiting_setter_position, point_won_by, serving_team) 

example_serves %>% 
  filter(team == "Braslovče") %>% 
  ggplot(aes(x = evaluation, fill = skill_type)) + geom_bar()


CU_PENN <- read_dv("CU-PENN.dvw", insert_technical_timeouts = FALSE, skill_evaluation_decode = skill_evaluation_decoder())
write_rds(CU_PENN, path = "mfields_finalproject/CU_PENN.rds")

#trying to figure out what serves have coordinates that are negative because 
# they look strange on the serving map in the shiny app. 
plays(CU_PENN) %>%  filter(skill == "Serve") %>% filter(evaluation == "Error") %>% 
  select(point_id, serving_team, home_team_score, visiting_team_score, player_name, skill, start_zone, end_zone, evaluation, code, start_coordinate, end_coordinate, start_coordinate_x, start_coordinate_y, end_coordinate_x, end_coordinate_y) %>% 
  View()



ggplot(aes(start_coordinate_x, start_coordinate_y,
                    xend=end_coordinate_x, yend=end_coordinate_y))+
  geom_segment() + geom_point() + ggcourt()

download.file(url = "https://raw.githubusercontent.com/jackconnolly21/heatmap/master/main/app/data/ivy2017/%262017-09-14%2023501%20HARV-BC(VM).dvw", 
              destfile = "harvard_bc.dvw", 
              quiet = TRUE, 
              mode = "wb")

harvard_bc <- read_dv("harvard_bc.dvw", insert_technical_timeouts = FALSE)
harvard_bc_plays <- plays(harvard_bc) %>% 
  # Here, I'm removing the system codes. These are codes that do not represent plays, but contain condensed metadata like 
  # rotation, setter number, and substitutions for each play. I do not need these to calculate the attacking statistics. System
  # codes are not coded for player number, so I can filter out observations where player umber is NA. 
  filter(!is.na(player_number))

write_rds(harvard_bc, path = "mfields_finalproject/harvard_bc.rds")

plays(harvard_bc) %>% filter(team == "Harvard University") %>% 
  filter(skill == "Attack") %>% 
  group_by(phase) %>% 
  filter(evaluation_code == "=") %>% 
  count() %>% View()

harvard_bc_plays %>% 
filter(skill == "Reception") %>% 
  filter(team == "Harvard University") %>% 
  group_by(skill_type, evaluation) %>% 
  count() %>% 
  spread(skill_type, n) %>% 
  replace(.,is.na(.),"0") %>% 
  View()

attack_rate <- 
  harvard_bc_plays %>% 
  filter(skill == "Attack") %>% 
  group_by(start_zone, team) %>% 
  summarize(n_attacks = n()) %>% 
  mutate(rate = n_attacks/sum(n_attacks)) %>% 
  ungroup() 

attack_rate2 <- cbind(attack_rate, dv_xy(attack_rate$start_zone, end = "lower"))

percent(attack_rate2$rate) %>% View()

#creates a variable lag_skill that is equal to the previous skill. I want to know what skill came before it, and what was the evaluation of that skill. This will help
#when creating the setting chart which shows where the setter set depending on the evaluation of the pass. I use hit start zone to evaluate set location
# instead of set end_zone because the attack start_zone is consistent with the attack combination code and attack description, while the set end zone is not. I'm not sure why. 
# For instance, when the outside hitter hits in zone 4, the hit start_zone is 4, but the set end zone is not 4. therefore I think that the attack start_zone variable
# gives a more accurate description of set locations. 
harvard_bc_plays %>% 
  mutate(lag_skill = lag(skill), 
         lag_2skill = lag(skill, n = 2L), 
         lag_skill_evaluation_code = lag(evaluation_code), 
         lag_skill_evaluation = lag(evaluation), 
         lag_2skill_evaluation_code = lag(evaluation_code,n =2L), 
         lag_2skill_evaluation = lag(evaluation, n = 2L)) %>% 
  filter(skill == "Attack") %>% 
  select(skill, lag_skill, lag_2skill, lag_skill_evaluation_code, lag_skill_evaluation,
         lag_2skill_evaluation_code, lag_2skill_evaluation) %>% 
  View()

harvard_bc_plays %>% 
  filter(skill %in% c("Set", "Attack")) %>% 
  filter(team == "Harvard University") %>% 
  select(skill, start_zone, end_zone,attack_code, attack_description, evaluation, player_name) %>% View()

laggedplays <- plays(CU_PENN) %>% 
  filter(!is.na(player_number)) %>% 
  mutate( 
    lag_2skill = lag(skill, n = 2L), 
    lag_2skill_evaluation_code = lag(evaluation_code,n =2L), 
    lag_2skill_evaluation = lag(evaluation, n = 2L))
  
laggedplays %>% 
filter(skill == "Attack") %>% View()
filter(team == "University of Pennsylvania") %>%
  filter(lag_2skill_evaluation == "Perfect pass") %>% View()

harvard_bc_plays %>% filter(skill=="Reception") %>% select(evaluation, evaluation_code) %>%  View()

set_scores <- result$set_scores
score_home_team <- set_scores$score_home_team


ymd(summary(CSU)[[1]])

plays_brown <- plays(BROWN1)


plays_brown %>% 
  filter(!is.na(player_number)) %>% 
  filter(team == home_team | team == visiting_team) %>% 
  filter(skill == "Reception") %>% 
  group_by(home_setter_position) %>% 
  summarize(
    sideouts = sum(point_won_by == home_team),
    total = n(),
    sideoutpercentage = sideouts/total
  )

plays_brown %>% 
  filter(!is.na(player_number)) %>% 
  filter(skill == "Reception") %>% 
  group_by(home_setter_position, ) %>% 
  summarize(
    sideouts = sum(point_won_by == home_team),
    total = n(),
    sideoutpercentage = sideouts/total
  )
summary(BROWN1)

home_team(BROWN1)
