library(tidyverse)
library(magrittr)
library(mysportsfeedsR)
authenticate_v1_x('njpsy','asdfasdf')


# get NHL schedule --------------------------------------------------------
game.schedule <- msf_get_results(league='nhl', 
                                 season='2015-2016-regular', 
                                 feed='full_game_schedule')[["api_json"]][["fullgameschedule"]][["gameentry"]] %>%
  data.frame() %>%
  subset(select=c(6, 12, 16)) %>%
  setNames(c('date', 'away', 'home')) %>%
  mutate(request = paste(str_replace_all(.$date, '-', ''), .$away, .$home, sep='-')) %T>%
  write.csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\nhlstats\\schedule.csv')
# get NHL schedule --------------------------------------------------------


# get individual game data ------------------------------------------------
#add home and away teams?????
game.schedule <- read.csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\nhlstats\\schedule.csv')

game.data <- game.schedule$request[1:2] %>%
  map(~msf_get_results(league='nhl',
                       season='2015-2016-regular',
                       feed='game_playbyplay', 
                       params=list(gameid=., playtype='faceoff,goal')
                      )[["api_json"]][["gameplaybyplay"]][["plays"]][["play"]]  %>% 
      subset(select=c('period', 'time', 'faceoff.wonBy')) %>%
      split(.$period)
     ) %>% 
  unlist(recursive=FALSE)
# get individual game data ------------------------------------------------


# process into time differences -------------------------------------------
Time.Calculations <- function(ele){
  period.calculations <- ele$time %>% 
    str_split(':') %>% 
    map(~.[1] %>% as.numeric() * 60 + .[2] %>% as.numeric()) %>% 
    unlist() %>% 
    diff() %>%
    tibble() %>%
    setNames(c('playtimes'))
  
  period.calculations$goal  <- ifelse(!is.na(ele$faceoff.wonBy), FALSE, TRUE) %>% tail(-1)
  
  period.calculations$period <- ele$period[1]
  
  period.calculations %<>% subset(playtimes != 0)
}

time.data <- map_df(game.data, Time.Calculations) %T>%
  write.csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\nhlstats\\time_data.csv')
# process into time differences -------------------------------------------


# visual displays ---------------------------------------------------------
boxplot(time.data$playtimes)

ggplot(time.data) + geom_boxplot(aes(x=period, y=playtimes))

ggplot(time.data, aes(x=playtimes)) + geom_histogram(aes(fill=goal), position=position_fill(reverse=FALSE))
# visual displays ---------------------------------------------------------