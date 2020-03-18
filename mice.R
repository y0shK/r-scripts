# what proteins distinguish control mice from Down syndrome mice?

getwd()
setwd('/home/yash/mice')

mice_data <- read.csv('Data_Cortex_Nuclear.csv')
mice_df <- data.frame(mice_data)

head(mice_data)
summary(mice_data)

xtabs(~DYRK1A_N + class, data = mice_data)

# use the generalized linear model (glm) function for logistic regression
mice_data$DYRK1A_N <- factor(mice_data$DYRK1A_N) # treat the protein as a categorical variable
mice_data$ITSN1_N <- factor(mice_data$ITSN1_N)

# how do the following two proteins affect the classification of control vs. Down syndrome? 
logistic_model <- glm(formula = class ~ DYRK1A_N + ITSN1_N, data = mice_data, family = "binomial", maxit=100)

# residual deviance - 0 degrees of freedom because for each data point it creates a classification (no degrees of freedom, all pre-determined)
summary(logistic_model)
confint.default(logistic_model)
