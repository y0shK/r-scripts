# public health
# additionally, some linReg and logReg stuff

getwd()
setwd('/home/yash/public_health')

world_data_2015 <- read.csv('2015.csv')
happiness_data <- read.csv('world-happiness-report-2019.csv')

library(ggplot2)
library(magrittr)

world_data_2015_df <- data.frame(world_data_2015)
happiness_data_df <- data.frame(happiness_data)

summary(world_data_2015_df)
summary(happiness_data_df)

# graph the data with ggplot
world_data_2015 %>%
  ggplot(aes(x=Economy..GDP.per.Capita., y=Health..Life.Expectancy., size=Family, color=Freedom)) + geom_point()

happiness_data %>%
  ggplot(aes(x=Healthy.life.expectancy, y=Log.of.GDP.per.capita, size=Social.support, color=Freedom)) + geom_point()

attach(world_data_2015)

# use lm and ggplot to create residual models
# broom + lm() with ggplot: https://stackoverflow.com/questions/36731027/how-can-i-plot-the-residuals-of-lm-with-ggplot

library(broom)
gdp_health <- lm(Health..Life.Expectancy. ~ Economy..GDP.per.Capita.)

ggplot(gdp_health, aes(x=.fitted, y=.resid)) + geom_point() + labs(x='GDP per capita', y='Residuals', title='GDP per capita vs. Health, residuals')

library(lattice)
library(scatterplot3d)

densityplot(Economy..GDP.per.Capita., main='Economy: GDP per capita')
scatterplot3d(x=Economy..GDP.per.Capita., y=Freedom, z=Health..Life.Expectancy., pch=16, color='steelblue', box=FALSE)
