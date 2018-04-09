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


# team data collection ----------------------------------------------------
win.rate <- 95/82

team.ids <- read_csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\nhlstats\\teams.csv', 
                     col_names=c('team', 'name'))

Team.Data.Collect <- function(x){
  print(paste('https://www.hockey-reference.com/teams/', x, '/2018_games.html', sep = ''))
  Sys.sleep(1)
  getURL(paste('https://www.hockey-reference.com/teams/', x, '/2018_games.html', sep = '')) %>% 
    htmlParse() %>%
    xpathSApply('//*[@id="games"]/tbody/tr//td[position()=7 or position()=8]', xmlValue) %>%
    matrix(ncol=2, byrow=T) %>%
    as.tibble() %>%
    rowid_to_column('game') %>%
    add_row(game = 0, V1 = '', V2 = '', .before = 1) %>%
    mutate(points = ifelse(V1 == 'W', 2, ifelse(V1 == 'L' & (V2 %in% c('OT', 'SO')), 1, 0)),
           total.points = cumsum(points),
           resid = total.points - game*win.rate, 
           team = x
    )
}


points.data <- team.ids$team %>%
  map_df(~Team.Data.Collect(.)) %>%
  select(-V1, -V2) %>%
  select(team, everything())



points.data %>%
  ggplot(aes(game, resid)) +
  geom_hex() +
  geom_hline(yintercept=0, color='yellow')
  
points.data %>%
  ggplot(aes(game, resid)) +
  geom_point() + 
  geom_hline(yintercept=0, color='yellow') +
  facet_wrap(~team)

summary.points.data <- points.data %>%
  group_by(team) %>%
  summarise(min = min(resid),
            max = max(resid),
            total.points = max(total.points), 
            end.position = last(resid)
            ) %>%
  arrange(desc(total.points))

ggplot(summary.points.data, aes(color=total.points > 95)) +
  geom_point(aes(reorder(team, total.points), end.position), size=2, show.legend=FALSE) +
  geom_segment(aes(x=team, xend=team, y=min, yend=max), show.legend=FALSE) +
  geom_hline(yintercept=0, color='black') +
  scale_y_continuous(limits=c(-35, 25), breaks=seq(-35, 25, 5)) +
  coord_flip() +
  labs(y='Net Points',
       x='Team',
       title='NHL Team Position and Season Range') +
  theme_bw() + 
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank())










#http://www.sportsclubstats.com/NHL.html
