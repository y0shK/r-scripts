# lengths act as nrow 
success_length <- length(success)
failure_length <- length(failure)

print(success_length)
print(failure_length)

set.seed(100)

# equal amounts of success and failure data
success_training_rows <- sample(1:success_length, as.integer(0.7*success_length))
failure_training_rows <- sample(1:failure_length, as.integer(0.7*failure_length))

training_success_data <- success[success_training_rows]
training_failure_data <- failure[failure_training_rows]
training_data_logis2 <- rbind(training_success_data, training_failure_data) 

# create test data
test_success_data <- success[-success_training_rows] # last bits of data to test
test_failure_data <- success[-failure_training_rows]
test_data_logis2 <- rbind(test_success_data, test_failure_data)

# try out data
print(training_data_logis2)

logistic_model_testdata <- glm(health_factor ~ world_data_2015$Economy..GDP.per.Capita., data = training_data_logis2, family='binomial')

data_prediction_log_testdata <- predict(logistic_model_testdata, test_data_logis2, type='response')

summary(logistic_model_testdata)

actual_vs_predicted_log_testdata <- data.frame(cbind(actuals=world_data_2015$Health..Life.Expectancy., predicteds=data_prediction_log_testdata))
cor_accuracy_log_testdata <- cor(actual_vs_predicted_log_testdata)
head(actual_vs_predicted_log_testdata)
