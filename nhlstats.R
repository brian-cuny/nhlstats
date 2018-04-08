library(tidyverse)
library(magrittr)
library(mysportsfeedsR)
library(httr)
library(jsonlite)
library(httr)
library(RCurl)
library(XML)

test <- GET('https://api.mysportsfeeds.com/v1.2/pull/nhl/2015-2016-regular/full_game_schedule.json', authenticate('njpsy', 'asdfasdf'), add_headers('Content-Type'='application/json', 'Accept-Encoding'='gzip')) %>%
  content(as='text') %>%
  fromJSON(flatten=TRUE) %>%
  .[[1]] %>%
  .$gameentry %>%
  as.tibble()

# get NHL schedule --------------------------------------------------------
authenticate_v1_x('njpsy','asdfasdf')

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

game.schedule <- read.csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\nhlstats\\schedule.csv', stringsAsFactors=FALSE)

game.data <- game.schedule$request %>%
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
  write.csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\nhlstats\\time_data.csv', append='a')
# process into time differences -------------------------------------------


# visual displays ---------------------------------------------------------
ggplot(time.diffs) + geom_boxplot(aes(x=period, y=play.lengths))

IQR(time.diffs$play.lengths)
summary(time.diffs$play.lengths)

ggplot(time.diffs %>% subset(play.lengths <= 175), aes(x=play.lengths)) + geom_histogram(aes(fill=result), position=position_fill(reverse=TRUE), binwidth=5)
# visual displays ---------------------------------------------------------



ppg <- 48/82

ppg <- 48/81

in.playoffs <- tibble(success=rbinom(n=10000, size=82, prob=ppg), size=82)

in.playoffs %>%
  filter(success >= 48) %>%
  count()

ggplot(data=data.frame(x=c(0,82)), aes(x)) +
  stat_function(fun=dnorm, args=list(mean=82*.5, sd=sqrt(82*0.5*0.5)), color='red') +
  stat_function(fun=dnorm, args=list(mean=10 + 72*.5, sd=sqrt(82*0.5*0.5)), color='blue')

pbinom(q=47, size=82, prob=0.58, lower.tail=FALSE)
pbinom(q=0.5, size=3, prob=0.5825, lower.tail=FALSE)


devils.data.html <- getURL('https://www.hockey-reference.com/teams/NJD/2018_games.html') %>% 
  htmlParse()

devils.data <- devils.data.html %>%
  xpathSApply('//*[@id="games"]/tbody/tr//td[7]', xmlValue) 

result.data <- devils.data.html %>%
  xpathSApply('//*[@id="games"]/tbody/tr//td[8]', xmlValue) 

devils.t <- tibble(game=0:82, result=c('', devils.data), mod=c('', result.data)) %>%
  mutate(points = ifelse(result == 'W', 2, ifelse(result == 'L' & (mod == 'OT' | mod == 'SO'), 1, 0))) %>%
  mutate(remaining = 95-cumsum(points)) %>%
  mutate(games.left = 82-game) %>%
  mutate(remaining = ifelse(remaining > 0, remaining, -1)) %>%
  mutate(prob=pbinom(q=remaining/2, size=games.left, prob=0.58, lower.tail=FALSE)) %>%
  #mutate(prob=pnorm(remaining/(82-game), mean=48/82, sd=48/82/3, lower.tail=FALSE)) %>%
  mutate(prob=ifelse(is.na(prob), 1, prob))


devils.t %>%
  ggplot(aes(game, prob)) +
  geom_point()

#http://www.sportsclubstats.com/NHL.html
