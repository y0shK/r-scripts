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

# from https://www.statmethods.net/advgraphs/trellis.html
# and http://www.sthda.com/english/wiki/scatterplot3d-3d-graphics-r-software-and-data-visualization

library(scatterplot3d)
attach(data)

scatterplot3d(x=texture_mean, y=compactness_mean, z=diagnosis, color='darkred', pch=16, box=FALSE)
scatterplot3d(x=texture_mean, y=smoothness_mean, z=diagnosis, color='steelblue', pch=16, box=FALSE)
scatterplot3d(x=smoothness_mean, y=compactness_mean, z=diagnosis, color='darkgreen', pch=16, box=FALSE)

library(lattice)

# plots the frequency of mean texture
densityplot(~texture_mean, main='Mean texture density plot', xlab='Mean texture')

radius.f <- factor(radius_mean, levels=c(quantile(radius_mean, 0.25), quantile(radius_mean, 0.50), quantile(radius_mean, 0.75)))
densityplot(~smoothness_mean | radius.f, main = 'Mean smoothness by mean radius', xlab='Mean smoothness', layout=c(1,3))
