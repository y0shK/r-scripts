# heart disease R analysis

getwd()
setwd('/home/yash/heartDiseaseR')

heart_raw_data <- read.csv('heart.csv')

library(ggplot2)
library(dplyr)
library(magrittr)

# use data frame for t-test
heart_df <- data.frame(heart_raw_data)

# create ggplot function with desired specs to easily add extra arguments
cholesterol_trestbps <- list(geom_point(), labs(x='cholesterol (mg/dL)', y='resting blood pressure (mmHg)', title='cholesterol vs. resting blood pressure'), theme(plot.title = element_text(hjust=0.5)))

ggplot_extra <- function(list_arg, num) {
  arg_string <- list_arg[num]
  return(arg_string)
}

heart_raw_data %>%
  ggplot(aes(chol, y=trestbps, col=sex, size=age)) + ggplot_extra(cholesterol_trestbps, 1) + ggplot_extra(cholesterol_trestbps, 2) + ggplot_extra(cholesterol_trestbps, 3)

# create exponential prediction for cholesterol
cholesterol_trestbps_df <- data.frame(heart_raw_data$chol, heart_raw_data$trestbps)

# directly refer to data rather than using $ notation
attach(heart_raw_data)

# linear model, y = exp(x)
# assigned as log(y) = x
exponential.model <- lm(log(trestbps) ~ chol)
summary(exponential.model)

#resid <- (trestbps - exponential.model(trestbps))
plot(resid(exponential.model))

# normal cumulative distribution function (ncdf), n > 1000
plot(density(resid(exponential.model)))

# normal probability plot
qqnorm(resid(exponential.model))
qqline(resid(exponential.model))
