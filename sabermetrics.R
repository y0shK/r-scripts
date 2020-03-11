# sabermetrics stuff

getwd()
setwd('/home/yash/hist_baseball')

data_baseball_hitting <- read.csv('batting.csv')

library(ggplot2)
library(dplyr)

# base on balls vs. runs?
# base on balls vs. strikeouts?
# base on balls vs. rbi?
# base on balls vs. hr?
# hr vs. rbi?

data_baseball_hitting %>%
  filter(year >= 2010) %>%
  ggplot(aes(x=bb, y=r, col=league_id, size=ibb)) + geom_point() + facet_wrap(~year) + labs(x='bases on balls', y='runs scored', title='Base on balls vs. runs scored', subtitle='stratified by intentional walks') + theme(plot.title = element_text(hjust=0.5))
  r_coef = cor(data_baseball_hitting$bb, data_baseball_hitting$r, use='complete.obs')
  print(r_coef)

data_baseball_hitting %>%
  filter(year >= 2010) %>%
  ggplot(aes(x=bb, y=so, col=league_id)) + geom_point() + facet_wrap(~year) + labs(x='bases on balls', y='strikeouts', title='Base on balls vs. strikeouts') + theme(plot.title = element_text(hjust=0.5))
  r_coef = cor(data_baseball_hitting$bb, data_baseball_hitting$so, use='complete.obs')
  print(r_coef)

data_baseball_hitting %>%
  filter(year >= 2010) %>%
  ggplot(aes(x=bb, y=rbi, col=league_id)) + geom_point() + facet_wrap(~year) + labs(x='bases on balls', y='runs batted in', title='Base on balls vs. runs batted in') + theme(plot.title = element_text(hjust=0.5))
  r_coef = cor(data_baseball_hitting$bb, data_baseball_hitting$rbi, use='complete.obs')
  print(r_coef) 

data_baseball_hitting %>%
  filter(year >= 2010) %>%
  ggplot(aes(x=bb, y=hr, col=league_id)) + geom_point() + facet_wrap(~year) + labs(x='bases on balls', y='home runs', title='Base on balls vs. home runs') + theme(plot.title = element_text(hjust=0.5))
  r_coef = cor(data_baseball_hitting$bb, data_baseball_hitting$hr, use='complete.obs')
  print(r_coef)

data_baseball_hitting %>%
  filter(year >= 2010) %>%
  ggplot(aes(x=hr, y=rbi, col=league_id, size=ab)) + geom_point() + facet_wrap(~ibb) + labs(x='home runs', y='runs batted in', title='Home runs vs. runs batted in', subtitle='stratified by intentional walks') + theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5))
  r_coef = cor(data_baseball_hitting$hr, data_baseball_hitting$rbi, use='complete.obs')
  print(r_coef)

# on-base percentage vs. runs scored  
data_baseball_hitting %>%
  filter(year >= 2010) %>%
  ggplot(aes(x=(h + bb + hbp)/(ab+bb+hbp+sf), y=r, col=league_id, size=hr)) + geom_point() + facet_wrap(~year) + labs(x='OBP', y='runs scored', title='OBP vs. runs scored', subtitle='shaped by home run count') + theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5))

# analyze player data of Scott Hatteberg, a Moneyball-era catcher for the Oakland A's
# how did his OBP affect his runs scored? how about his OPS?
hitter_data <- data_baseball_hitting %>%
  select(player_id, year, bb, r, h, hbp, ab, sf, h, double, triple, hr) %>%
  filter(player_id == 'hattesc01') %>%
  group_by(year) %>%
  summarise(at_bats = ab, hits = h, walks = bb, OBP = (h+bb+hbp)/(ab+bb+hbp+sf), SLG = (h + 2*double + 3*triple+4*hr)/(ab), OPS = OBP+SLG, runs_scored = r)

View(player_data)
