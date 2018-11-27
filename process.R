library(devtools)
library(datavolley)
library(tidyverse)

example_file <- dv_example_file(choice = 1) %>% 
  read_dv(insert_technical_timeouts = FALSE)

write_rds(example_file, path = "mfields_finalproject/example_file.rds")

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



ggplot(aes(start_coordinate_x, start_coordinate_y,
                    xend=end_coordinate_x, yend=end_coordinate_y))+
  geom_segment() + geom_point() + ggcourt()