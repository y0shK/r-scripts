# sabermetrics stuff

getwd()
setwd('/home/yash/hist_baseball')

hitting_raw_data <- read.csv('batting.csv')

library(ggplot2)
library(dplyr)

# base on balls vs. runs?
# base on balls vs. strikeouts?
# base on balls vs. rbi?
# base on balls vs. hr?
# hr vs. rbi?

#typeof(hitting_data) # list
#typeof(hitting_data$r) # double

hitting_data <- hitting_raw_data %>%
  filter(year >= 2010)

hitting_data %>%
  ggplot(aes(x=bb, y=r, col=ibb, size=h)) + geom_point() + facet_wrap(~year) + labs(x='bases on balls', y='runs scored', title='Base on balls vs. runs scored', subtitle='stratified by intentional walks and hits') + theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5))
  r_coef = cor(hitting_data$bb, hitting_data$r, use='complete.obs')
  print(r_coef)

hitting_data %>%
  ggplot(aes(x=bb, y=so, col=ibb, size=h)) + geom_point() + facet_wrap(~year) + labs(x='bases on balls', y='strikeouts', title='Base on balls vs. strikeouts', subtitle='stratified by intentional walks and hits') + theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5))
  r_coef = cor(hitting_data$bb, hitting_data$so, use='complete.obs')
  print(r_coef)

hitting_data %>%
  ggplot(aes(x=bb, y=rbi, col=ibb, size=hr)) + geom_point() + facet_wrap(~year) + labs(x='bases on balls', y='runs batted in', title='Base on balls vs. runs batted in', subtitle='stratified by intentional walks and home runs') + theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5))
  r_coef = cor(hitting_data$bb, hitting_data$rbi, use='complete.obs')
  print(r_coef) 

hitting_data %>%
  ggplot(aes(x=bb, y=hr, col=ibb, size=rbi)) + geom_point() + facet_wrap(~year) + labs(x='bases on balls', y='home runs', title='Base on balls vs. home runs', subtitle='stratified by intentional walks and RBI') + theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5))
  r_coef = cor(hitting_data$bb, hitting_data$hr, use='complete.obs')
  print(r_coef)

hitting_data %>%
  ggplot(aes(x=hr, y=rbi, col=ibb, size=bb)) + geom_point() + facet_wrap(~year) + labs(x='home runs', y='runs batted in', title='Home runs vs. runs batted in', subtitle='stratified by intentional walks and walks') + theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5))
  r_coef = cor(hitting_data$hr, hitting_data$rbi, use='complete.obs')
  print(r_coef)

# on-base percentage vs. runs scored  
hitting_data %>%
  ggplot(aes(x=(h + bb + hbp)/(ab+bb+hbp+sf), y=r, col=hr, size=bb)) + geom_point() + facet_wrap(~year) + labs(x='OBP', y='runs scored', title='OBP vs. runs scored', subtitle='shaped by walks and home run count') + theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5))

# slugging precentage vs. runs scored
ggplot(data=hitting_data, aes(x=(h + 2*double + 3*triple+4*hr)/ab, y=r, color=hr, size=bb)) + geom_point()+ facet_wrap(~year) + labs(x='SLG', y='runs scored', title='SLG vs. runs scored', subtitle='shaped by walk count and home runs') + theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5)) + scale_x_continuous(limits=c(0, 1))
