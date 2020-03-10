getwd()
setwd('/home/yash/research/breast_cancer')

data <- read.csv('data.csv')

library(ggplot2)
library(dplyr)

# size=area_mean to add another layer of information
# it is possible that the trend is disrupted because of size rather than genuine correlation

# texture vs. smoothness
data %>%
  ggplot(aes(x=texture_mean, y=smoothness_mean, col=diagnosis, size=area_mean)) + geom_point() + 
  ggtitle('Texture vs. Smoothness') + theme(plot.title = element_text(hjust=0.5))

# smoothness vs. compactness
data %>%
  ggplot(aes(x=smoothness_mean, y=compactness_mean, col=diagnosis, size=area_mean)) + geom_point() +
  ggtitle('Smoothness vs. Compactness') + theme(plot.title = element_text(hjust=0.5))
