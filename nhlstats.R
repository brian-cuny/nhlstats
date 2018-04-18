library(tidyverse)
library(magrittr)
library(httr)
library(jsonlite)
library(httr)
library(RCurl)
library(XML)

schedule <- GET('https://api.mysportsfeeds.com/v1.2/pull/nhl/2015-2016-regular/full_game_schedule.json', 
            authenticate('njpsy', 'asdfasdf'), 
            add_headers('Content-Type'='application/json', 'Accept-Encoding'='gzip')
            ) %>%
          content(as='text') %>%
          fromJSON(flatten=TRUE) %>%
          pluck(1) %>%
          pluck(2) %>%
          as.tibble() %>%
          subset(select=c(6, 12, 16)) %>%
          setNames(c('date', 'away', 'home')) %>%
          mutate(request = paste(str_replace_all(date, '-', ''), away, home, sep='-')) %T>%
          write.csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\nhlstats\\schedule.csv')


# get individual game data ------------------------------------------------
#schedule <- read_csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\nhlstats\\schedule.csv')

TimeToSeconds <- function(time){
  time %>% 
    str_split(':') %>% 
    map(~.[1] %>% as.numeric() * 60 + .[2] %>% as.numeric()) %>% 
    unlist()
}

Game.Request <- function(game){
  Sys.sleep(2)
  print(game)
  GET('https://api.mysportsfeeds.com/v1.2/pull/nhl/2015-2016-regular/game_playbyplay.json', 
      authenticate('njpsy', 'asdfasdf'), 
      add_headers('Content-Type'='application/json', 'Accept-Encoding'='gzip'),
      query=list('gameid'=game, 'playtype'='faceoff,goal')
     ) %>%
  content(as='text') %>%
  fromJSON(flatten=TRUE) %>%
  pluck(1) %>%
  pluck(3) %>%
  pluck(1) %>%
  as.tibble() %>%
  mutate(goal = ifelse(is.na(faceoff.wonBy), TRUE, FALSE),
         seconds = TimeToSeconds(time)) %>%
  select(period, seconds, goal) %>%
  subset(!duplicated(.[, c('period', 'seconds')]))
}

game.data <- schedule$request %>%
  map_df(~Game.Request(.),
         .id='id'
         ) 
# get individual game data ------------------------------------------------

# process into time differences -------------------------------------------

play.lengths <- c(0, diff(game.data$seconds))

time.diffs <- game.data %>%
  cbind(play.lengths) %>%
  filter(play.lengths > 0) %>%
  select(-seconds) %T>%
  write.csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\nhlstats\\time_data.csv')
# process into time differences -------------------------------------------


# visual displays ---------------------------------------------------------
ggplot() + 
  geom_histogram(data=time.diffs %>% filter(goal == TRUE), aes(x=play.lengths, y=..count../sum(..count..), stat='identity', fill=goal), alpha=0.5) +
  geom_histogram(data=time.diffs %>% filter(goal == FALSE), aes(x=play.lengths, y=..count../sum(..count..), stat='identity', fill=goal), alpha=0.5)

time.prop <- time.diffs %>%
  group_by(goal, play.lengths) %>%
  summarise(n = n()) %>%
  group_by(goal) %>%
  mutate(prop = n/sum(n))

ggplot(time.prop) + 
  geom_histogram(aes(x=play.lengths, y=prop, fill=goal), stat='identity') + 
  facet_wrap(~goal, ncol=1)

summary(time.diffs %>% filter(goal == TRUE) %>% select(play.lengths))
summary(time.diffs %>% filter(goal == FALSE) %>% select(play.lengths))

IQR(time.diffs$play.lengths)
summary(time.diffs$play.lengths)

ggplot(time.diffs %>% subset(play.lengths <= 175), aes(x=play.lengths)) + geom_histogram(aes(fill=result), position=position_fill(reverse=TRUE), binwidth=5)
# visual displays ---------------------------------------------------------




