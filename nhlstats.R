library(tidyverse)
library(magrittr)
library(mysportsfeedsR)
authenticate_v1_x('njpsy','asdfasdf')

game.schedule <- data.frame(msf_get_results(league='nhl', season='2015-2016-regular', feed='full_game_schedule')[["api_json"]][["fullgameschedule"]][["gameentry"]]) %>%
  subset(select=c(6, 12, 16)) %>%
  setNames(c('date', 'away', 'home'))

game.schedule$request <- paste(str_replace_all(game.schedule$date, '-',''), game.schedule$away, game.schedule$home, sep='-')

game.schedule <- game.schedule[1:100, ]

#data <- msf_get_results(version='1.2',league='nhl',season='2015-2016-regular',feed='game_playbyplay', params=list(gameid=game.schedule$request[1], playtype='faceoff,goal,penalty'))

to_ret <- tibble()

data <- game.schedule$request[1:2] %>%
  map(~msf_get_results(version='1.2',league='nhl',season='2015-2016-regular',feed='game_playbyplay', params=list(gameid=., playtype='faceoff,goal')))

test <- data %>% map(~.[["api_json"]][["gameplaybyplay"]][["plays"]][["play"]]  %>% 
                       subset(select=c('period', 'time', 'faceoff.wonBy'))
                     ) %>%
  map(~split(., .$period)) %>% 
  unlist(recursive=FALSE)


#remove loop here!!!!!
for(i in 1:length(test)){
  period.calculations <- test[[i]]$time %>% 
    str_split(':') %>% 
    map(~.[1] %>% as.numeric() * 60 + .[2] %>% as.numeric()) %>% 
    unlist() %>% 
    diff() %>%
    tibble() %>%
    setNames(c('playtimes'))
  
  period.calculations$goal  <- ifelse(!is.na(test[[i]]$faceoff.wonBy),FALSE, TRUE) %>% tail(-1)
 
  period.calculations$period <- test[[i]]$period[1]
  
  period.calculations %<>% subset(playtimes != 0)
  
  to_ret <- rbind(to_ret, period.calculations)
}

boxplot(to_ret$playtimes)

summary(to_ret$playtimes)

no.outs <- to_ret[to_ret$playtimes <= 140, ]

ggplot(no.outs, aes(x=playtimes)) + geom_histogram(aes(fill=goal), position=position_fill(reverse=FALSE))
