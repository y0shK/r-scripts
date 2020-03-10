# sabermetrics stuff

getwd()
setwd('/home/yash/hist_baseball')

data_baseball <- read.csv('batting.csv')

library(ggplot2)
library(dplyr)

# base on balls vs. runs?
# base on balls vs. strikeouts?
# base on balls vs. rbi?
# base on balls vs. hr?
# hr vs. rbi?

data_baseball %>%
  filter(year >= 2010) %>%
  ggplot(aes(x=bb, y=r, col=league_id, size=ibb)) + geom_point() + facet_wrap(~year) + labs(x='bases on balls', y='runs scored', title='Base on balls vs. runs scored', subtitle='stratified by intentional walks') + theme(plot.title = element_text(hjust=0.5))
  r_coef = cor(data_baseball$bb, data_baseball$r, use='complete.obs')
  print(r_coef)

data_baseball %>%
  filter(year >= 2010) %>%
  ggplot(aes(x=bb, y=so, col=league_id)) + geom_point() + facet_wrap(~year) + labs(x='bases on balls', y='strikeouts', title='Base on balls vs. strikeouts') + theme(plot.title = element_text(hjust=0.5))
  r_coef = cor(data_baseball$bb, data_baseball$so, use='complete.obs')
  print(r_coef)

data_baseball %>%
  filter(year >= 2010) %>%
  ggplot(aes(x=bb, y=rbi, col=league_id)) + geom_point() + facet_wrap(~year) + labs(x='bases on balls', y='runs batted in', title='Base on balls vs. runs batted in') + theme(plot.title = element_text(hjust=0.5))
  r_coef = cor(data_baseball$bb, data_baseball$rbi, use='complete.obs')
  print(r_coef) 

data_baseball %>%
  filter(year >= 2010) %>%
  ggplot(aes(x=bb, y=hr, col=league_id)) + geom_point() + facet_wrap(~year) + labs(x='bases on balls', y='home runs', title='Base on balls vs. home runs') + theme(plot.title = element_text(hjust=0.5))
  r_coef = cor(data_baseball$bb, data_baseball$hr, use='complete.obs')
  print(r_coef)

data_baseball %>%
  filter(year >= 2010) %>%
  ggplot(aes(x=hr, y=rbi, col=league_id, size=ab)) + geom_point() + facet_wrap(~ibb) + labs(x='home runs', y='runs batted in', title='Home runs vs. runs batted in', subtitle='stratified by intentional walks') + theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5))
  r_coef = cor(data_baseball$hr, data_baseball$rbi, use='complete.obs')
  print(r_coef)
