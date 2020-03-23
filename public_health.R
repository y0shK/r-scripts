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

# logistic regression for world_data_2015

# make the y-variable a factor (for classification) for glm to accept it as a parameter
# https://stackoverflow.com/questions/47546658/logistic-regression-on-factor-error-in-evalfamilyinitialize-y-values-must
health_factor = as.factor(world_data_2015$Health..Life.Expectancy.)

# logistic regression with health/life expectancy as the dependent variable
logistic_model <- glm(health_factor ~ world_data_2015$Economy..GDP.per.Capita., data = world_data_2015, family='binomial')

# use predict() with an extra parameter, type='response' indicates the use of a specific glm model (i.e. logistic regression)
# https://www.theanalysisfactor.com/r-tutorial-glm1/
# https://stackoverflow.com/questions/23085096/type-parameter-of-the-predict-function/45647358
data_prediction_log <- predict(logistic_model, test_data, type='response')

summary(logistic_model)

actual_vs_predicted_log <- data.frame(cbind(actuals=world_data_2015$Health..Life.Expectancy., predicteds=data_prediction_log))
cor_accuracy_log <- cor(actual_vs_predicted_log)
head(actual_vs_predicted_log)

# create a list of the subset of world_data_2015 (e.g. world_data_2015$Health...)
health_list <- c(Health..Life.Expectancy.)
typeof(health_list) # list

health_list_vector <- unlist(health_list) # list becomes vector
health_list_vector > 0.5 # test for a specific condition

bool_vector <- health_list_vector > 0.5 # assign the condition to a variable for a if/for loop

success_vector <- c()
failure_vector <- c()

i <- 1

# for every value in the boolean vector, if the condition is true (val > 0.5), assign its value to the list vector
for (val in bool_vector) {
  if (val == TRUE) {
    success_vector <- c(success_vector, health_list_vector[[i]])
  }
  else {
    failure_vector <- c(failure_vector, health_list_vector[[i]])
  }
  i <- i + 1
}

print(success_vector)
print(failure_vector)

# define success and failure integer lists with which()
success <- which(world_data_2015$Health..Life.Expectancy. >= 0.5) # greater than 0.5 is "higher life expectancy"
failure <- which(world_data_2015$Health..Life.Expectancy. < 0.5) # less than 0.5 is "lower life expectancy"

print(success)
print(failure)

# lengths act as nrow for later sample() function
success_length <- length(success)
failure_length <- length(failure)

print(success_length)
print(failure_length)

set.seed(100)

# equal amounts of success and failure data
# takes sample of earlier defined data (some subset from 1:len(success/failure) and converts it to integers)
success_training_rows <- sample(1:success_length, as.integer(0.7*success_length))
failure_training_rows <- sample(1:failure_length, as.integer(0.7*failure_length))

# creates lists by indexing the earlier samples
training_success_data <- success[success_training_rows]
training_failure_data <- failure[failure_training_rows]

# list to data.frame
# https://stackoverflow.com/questions/4227223/convert-a-list-to-a-data-frame
training_data_lists <- list(training_success_data, training_failure_data)
training_data_logis2_df <- data.frame(matrix(unlist(training_data_lists), nrow=length(training_data_lists), byrow=TRUE))

# create test data with the same procedure
test_success_data <- success[-success_training_rows] # last bits of data to test
test_failure_data <- success[-failure_training_rows]

test_data_lists <- list(test_success_data, test_failure_data)
test_data_logis2_df <- data.frame(matrix(unlist(test_data_lists), nrow=length(test_data_lists), byrow=TRUE))

# try out data
print(training_data_logis2_df)
print(test_data_logis2_df)

logistic_model_testdata <- glm(health_factor ~ world_data_2015$Economy..GDP.per.Capita., data = training_data_logis2_df, family='binomial')

data_prediction_log_testdata <- predict(logistic_model_testdata, test_data_logis2_df, type='response')

summary(logistic_model_testdata)

actual_vs_predicted_log_testdata <- data.frame(cbind(actuals=world_data_2015$Health..Life.Expectancy., predicteds=data_prediction_log_testdata))
cor_accuracy_log_testdata <- cor(actual_vs_predicted_log_testdata)
head(actual_vs_predicted_log_testdata)
