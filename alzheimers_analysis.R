# MRI and Alzheimers analysis - dataset from Kaggle

setwd('/home/yash/alzheimers_data')

longitudinal <- read.csv('oasis_longitudinal.csv')

age <- longitudinal$Age
etiv <- longitudinal$eTIV
mse <- longitudinal$MMSE

diagnosis <- longitudinal$Group
diagnosis # blocked by strings (e.g. 'Demented'), not with numbers

numeric_diagnosis <- as.numeric(diagnosis)
numeric_diagnosis # blocked by numbers (e.g. 1, 2, 3 - 3 factors because 'Converted' is a factor)

palette_by_diagnosis <- colorRampPalette(c('red', 'blue'))
intervals_by_diagnosis <- palette_by_diagnosis(3)[as.numeric(cut(numeric_diagnosis, breaks=3))]
color_ramp_palette_col_diagnosis <- c(return_hexadecimals(3))

# 1-variable linear regression, simply x-y
age_etiv_lin_model <- lm(etiv ~ age, longitudinal)
age_etiv_resid <- resid(age_etiv_lin_model)

summary(age_etiv_lin_model)

plot(age, etiv, col=color_ramp_palette_col_diagnosis, xlab='Age', ylab='Cranial volume', main='Age vs. cranial volume', pch=16)
legend('topright', legend=c('Converted', 'Demented', 'Nondemented'), col=color_ramp_palette_col_diagnosis, pch=16)
abline(age_etiv_lin_model) # line of best fit
print(coef(age_etiv_lin_model))

plot(age_etiv_resid, col=color_ramp_palette_col_diagnosis, xlab='Age', ylab='Residuals', main='Age vs. cranial volume residuals', pch=16)
legend('topright', legend=c('Converted', 'Demented', 'Nondemented'), col=color_ramp_palette_col_diagnosis, pch=16)
abline(0, 0) # residual line, acts as x-axis

# sapply to find means
sapply(age, mean)
sapply(etiv, mean)
sapply(mse, mean)

# decision tree for visualization
library(party)

output.tree <- ctree(diagnosis ~ age + etiv + mse, data=longitudinal)
plot(output.tree)

# multivariate regression for group, with respect to age, estimated intracranial volume, and mental status

# function to check the reliability of the multivariate regression
statistical_checks <- function(x) {
  head(resid(x)) # residuals
  coef(x) # coefficients
  sigma(x) # residual standard error
  vcov(x) # variance-covariance matrix
}

# find estimated total intracranial volume from age and mental status, blocked by diagnosis
multivariate_alzheimers_model <- lm(etiv ~ age + mse, data = longitudinal) # multivariate model, no blocking
summary(multivariate_alzheimers_model)

statistical_checks(multivariate_alzheimers_model)

attach(longitudinal)
alzheimers_rownums <- split(1:nrow(longitudinal), Group) # 1-variate model, block by diagnosis

# use the attached data rather than predefined variables, because the actual data varies in the regression
zlm_alzheimers_model <- function(Group) lm(eTIV ~ Age, data=longitudinal[Group, ])$coefficients # create a function of how age affects ETIV, blocked by group
w_alzheimers_model <- lapply(alzheimers_rownums, zlm_alzheimers_model) # apply the function through the already-blocked rows
w_alzheimers_model # print the trends after lapply, which applies it to each blocked list, find specific coefficients
