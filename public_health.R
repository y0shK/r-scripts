# public health
# additionally, some linear regression and logistic regression stuff

getwd()
setwd('/home/yash/public_health')

world_data_2015 <- read.csv('2015.csv')
happiness_data <- read.csv('world-happiness-report-2019.csv')

library(ggplot2)
library(magrittr)

# typeof(world_data_2015) # list
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

gdp_health <- lm(Health..Life.Expectancy. ~ Economy..GDP.per.Capita.)

ggplot(gdp_health, aes(x=.fitted, y=.resid)) + geom_point() + labs(x='GDP per capita', y='Residuals', title='GDP per capita vs. Health, residuals')

library(lattice)
library(scatterplot3d)

densityplot(Economy..GDP.per.Capita., main='Economy: GDP per capita')
scatterplot3d(x=Economy..GDP.per.Capita., y=Freedom, z=Health..Life.Expectancy., pch=16, color='steelblue', box=FALSE)

# density plot with library e1071

library(broom)
library(e1071)
plot(density(Economy..GDP.per.Capita.), main='Density plot: GDP per capita', ylab='Frequency')
polygon(density(Economy..GDP.per.Capita.), col='blue')

plot(density(Health..Life.Expectancy.), main='Density plot: Life expectancy', ylab='Frequency')
polygon(density(Health..Life.Expectancy.), col='green')

# linear regression for world_data_2015
lin_reg <- lm(Health..Life.Expectancy. ~ Economy..GDP.per.Capita., data=world_data_2015)
tidy(lin_reg) # use function tidy because it presents the data like a data frame

model_summary <- summary(lin_reg)
print(model_summary)

model_coefficients <- model_summary$coefficients

beta.estimate <- model_coefficients['Economy..GDP.per.Capita.', 'Estimate'] # find the likelihood of type 2 error
std.error <- model_coefficients['Economy..GDP.per.Capita.', 'Std. Error'] # standard error

t_val <- beta.estimate / std.error # t = beta / sd
p_val <- 2 * pt(-abs(t_val), df=nrow(world_data_2015)-ncol(world_data_2015))

# print string and variable contents successively
# https://stackoverflow.com/questions/15589601/print-string-and-variable-contents-on-the-same-line-in-r
print(paste0('t-value: ', t_val))
print(paste0('p-value: ', p_val))

# test the linear regression model with the actual data
# 80% train, 20% test

set.seed(100) # random sampling

# : operator creates first:last as vector c(first ... last)
training_row_index <- sample(1:nrow(world_data_2015), 0.8*nrow(world_data_2015))
training_data <- world_data_2015[training_row_index, ] # first 80%
test_data <- world_data_2015[-training_row_index, ] # last 20%

# develop the model and use it for predictions
lin_reg_prediction_model <- lm(Health..Life.Expectancy. ~ Economy..GDP.per.Capita., data=world_data_2015)
data_prediction <- predict(lin_reg_prediction_model, test_data)

# review the accuracy of the model
summary(lin_reg_prediction_model)

# calculate the accuracy and error
actual_vs_predicted <- data.frame(cbind(actuals=Health..Life.Expectancy., predicteds=data_prediction)) # cbind combines the actual and predicted objects
cor_accuracy <- cor(actual_vs_predicted)
head(actual_vs_predicted)

# to-do: logistic regression

health_list_vector > 0.5

bool_vector <- health_list_vector > 0.5

success_vector <- c()
failure_vector <- c()

i <- 1

for (val in bool_vector) {
  if (val == TRUE) {
    success_vector <- c(success_vector, health_list_vector[[i]])
  }
  else {
    failure_vector <- c(failure_vector, health_list_vector[[i]])
  }
  i <- i + 1
}
