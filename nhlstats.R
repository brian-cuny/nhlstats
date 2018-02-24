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
TimeToSeconds <- function(ele){
  ele$time %>% 
    str_split(':') %>% 
    map(~.[1] %>% as.numeric() * 60 + .[2] %>% as.numeric()) %>% 
    unlist()
}

game.schedule <- read.csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\nhlstats\\schedule.csv')

game.data <- game.schedule$request[1:200] %>%
  map_dfr(~msf_get_results(league='nhl',
                       season='2015-2016-regular',
                       feed='game_playbyplay', 
                       params=list(gameid=., playtype='faceoff,goal')
                      )[["api_json"]][["gameplaybyplay"]][["plays"]][["play"]]  %>% 
          mutate(seconds=TimeToSeconds(.)) %>%
          mutate(faceoff.wonBy=ifelse(is.na(.$faceoff.wonBy), 'goal', 'stop')) %>%
          subset(select=c('period', 'seconds', 'faceoff.wonBy')) %>%
          rename(result=faceoff.wonBy), 
        .id='id'
        ) %>%
  subset(!duplicated(.[, c('id', 'period', 'seconds')]))
# get individual game data ------------------------------------------------

# process into time differences -------------------------------------------
play.lengths <- c(0, diff(game.data$seconds))
time.diffs <- game.data %>%
  cbind(play.lengths) %>%
  subset(play.lengths > 0, select=c('id', 'period', 'play.lengths', 'result')) %T>%
  write.csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\nhlstats\\time_data.csv')
# process into time differences -------------------------------------------


# visual displays ---------------------------------------------------------
ggplot(time.diffs) + geom_boxplot(aes(x=period, y=play.lengths))

IQR(time.diffs$play.lengths)
summary(time.diffs$play.lengths)

ggplot(time.diffs %>% subset(play.lengths <= 175), aes(x=play.lengths)) + geom_histogram(aes(fill=result), position=position_fill(reverse=TRUE), binwidth=5)
# visual displays ---------------------------------------------------------