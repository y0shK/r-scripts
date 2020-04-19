# notes from github fasteR textbook to shore up understanding after 'Learn Statistics with R'

# use data set mtcars as an example

mtcars
summary(mtcars)

mean(mtcars$mpg, na.rm = TRUE)
hist(mtcars$mpg) # default breaks = 5 by default

# why use vectors? simply concatenate values in an easily accessible form of data
# vector has 1 individual with multiple pieces of information
# e.g. stats for a Pokemon
pikachu_stats <- c(100, 100, 90, 150, 150, 120)
names(pikachu_stats) <- c('HP', 'Atk', 'Def', 'Sp. Atk', 'Sp. Def', 'Spd')

pikachu_stats # easy to access any particular piece of information
pikachu_stats[4] # pikachu's special attack?

head(pikachu_stats) # head function works to preview vectors

# get variables after a certain index
# use specific time-frame 10:25 for different modes of analysis
n10_25 <- mtcars$mpg[10:25]

mean(n10_25, na.rm = TRUE) # 10th through 25th index
sd(n10_25, na.rm = TRUE)
length(n10_25) # 16

# when did sum of gross horsepower become greater than some value?
sum(mtcars$hp > 100) # yields 23, i.e. 23 years
# how does R complete this function?
  # computes a numeric sum vector e.g. c(1, 3, 7, 53, 93, 111)
  # and compares it element-by-element c(100, 100, 100, ..., 100)
  # if numeric sum > vector_100, then return TRUE, which happens in 23 years

# why use data frames? compile different vectors together of different types (string, numeric, boolean) which correspond to individuals in a data set
# has multiple individuals with multiple pieces of information each
# e.g. age, hair color, likes sports? Y/N

age_vector_df <- c(23, 34, 10, 92, 54, 22, 12, 52, 56) # numeric vector
hair_color_vector_df <- as.factor(c('black', 'black', 'red', 'blue', 'blond', 'blond', 'blond', 'red', 'black')) # converted to factor - can be used to block
likes_sports_vector_df <- as.logical(c(0, 0, 0, 1, 0, 1, 1, 0, 1)) # converts 0/1 to false/true

levels(hair_color_vector_df) # all possible versions of factor

example_data_df <- data.frame(age_vector_df, hair_color_vector_df, likes_sports_vector_df)
names(example_data_df) <- c('Age', 'Hair color', 'Likes sports')
example_data_df

mean(example_data_df$Age)
example_data_df[6, 2] # 'blond'

mean(example_data_df[, 1]) # all rows in 1st column
example_data_df[2:5, c(1, 3)] # from rows 2-5, pull columns 1 and 3

tg <- ToothGrowth
which(tg$len < 16) # tooth length < 16

# factor class is used for categorical variables
  # similar to blocking in experimental design
class(hair_color_vector_df) # character, though used as a factor class - converted to factor with as.factor coercion

class(tg) # data.frame, appends multiple varaibles of multiple subjects
class(tg$supp) # factor, acts as a blocking variable to analyze dose

# how likely is it to like sports given hair color?
black_hair <- example_data_df[example_data_df$`Hair color` == 'black', ] # produce boolean vector, take rows of 'black' and all columns
  # type of black_hair -> data.frame

blond_hair <- example_data_df[example_data_df$`Hair color` == 'blond', ]

determine_sports_black_hair_subset <- black_hair[black_hair$`Likes sports` == TRUE, ]
determine_sports_blond_hair_subset <- blond_hair[blond_hair$`Likes sports` == TRUE, ]

nrow(determine_sports_black_hair_subset) # how many people like sports, i.e. == TRUE?
nrow(determine_sports_blond_hair_subset)

# test how mean ages compare to hair (no real correlation)
determine_age_black_hair_subset <- mean(black_hair$Age)
determine_age_blond_hair_subset <- mean(blond_hair$Age)

determine_age_black_hair_subset
determine_age_blond_hair_subset

# use tapply to easily compare means

# blocking by hair color, compute the mean of each group of ages
tapply(example_data_df$Age, example_data_df$`Hair color`, mean) # result is a vector, can be saved and manipulated

# further tapply example
diagnoses <- c('M', 'B', 'B', 'B', 'M', 'B', 'M', 'B', 'M', 'B')
tumor_lengths <- c(23, 43, 12, 52, 14, 65, 23, 72, 12, 72)
tapply(tumor_lengths, diagnoses, mean) # block by diagnosis, find mean of tumor lengths - useful medical data

tapply(tumor_lengths, diagnoses, summary) # good for in-depth, numerical analysis of each kind of tumor
tapply(tumor_lengths, diagnoses, length) # good to see the overall breakdown of tumors by kind

row.names(example_data_df) <- c('Alice', 'Bob', 'Charlie', 'David', 'Eagle', 'Frankenstein', 'Goat', 'Helicopter', 'Igloo')
row.names(example_data_df)[8] <- 'Hello World' # replace concatenated value in vector with this custom string
example_data_df

# data cleaning
getwd()
setwd('~/Downloads')

pima_csv <- read.csv('Pima.csv', header=TRUE)
pima_csv

head(pima_csv) # default - 10 values
table(pima_csv$glucose) # table() gives a count of individuals with the given amount of the variable
  # in this case, table() gives a count of women based on their glucose levels
  # this table has some unreasonable values - glucose = 0 should not be possible

# clean the data to remove glucose = 0, not physiologically possible
reasonable_glucose_pima_csv <- pima_csv[pima_csv$glucose > 0, ] # rows = all glucose values > 0, columns = all

head(reasonable_glucose_pima_csv)
table(reasonable_glucose_pima_csv$glucose)

# check to see the impact of cleaning up the data
mean(pima_csv$glucose)
mean(reasonable_glucose_pima_csv$glucose)
# not much difference, but still good practice

original_triceps <- mean(pima_csv$triceps)
original_insulin <- mean(pima_csv$insulin)

# use NA instead of simple removal
recode_to_NA_glucose <- pima_csv$glucose == 0 # vector of TRUE and FALSE
pima_csv$glucose[recode_to_NA_glucose] <- NA # previous boolean vector is here used to set the zero elements to NA

sum(is.na(pima_csv$glucose)) # counts number of NA, output = 5
mean(pima_csv$glucose, na.rm = TRUE) # na.rm is FALSE by default

# determine other unreasonable values to clean

clean_data <- function(df_column_supplied, original) {
  index_for_NA <- df_column_supplied == 0
  df_column_supplied[index_for_NA] <- NA
  
  # create a vector that differentiates between original calculation and calculation after 0 -> NA and removed
  mean_vector <- c(mean(original, na.rm = TRUE), mean(df_column_supplied, na.rm = TRUE))
  return(mean_vector)
}

clean_data(pima_csv$triceps, original_triceps)
clean_data(pima_csv$insulin, original_insulin)

# R lists
print(which(pima_csv$glucose == 88)) # find the specific indices which correspond to a glucose level of 88 -> 9 total instances
print(pima_csv$glucose == 157) # create a boolean vector with TRUE wherever the glucose index is 157 and FALSE elsewhere

# can create boolean vectors with df$category == some value, for all values
  # alternative is to use split(df$category_examined, df$category_blocked)
  # where category_examined is the numeric vector being printed and category_blocked is the value checked against the == operator

block_by_glucose_7 <- pima_csv$glucose
examine_insulin_7 <- pima_csv$insulin

split_function_7 <- split(examine_insulin_7, block_by_glucose_7)
split_function_7

# split_function_7 is an object 'list' which specific, accessible vectors that possess all the frequency indices
  # access the 1st vector with split_function_7$vector_value
  # access the 1st index with split_function_7[[1]]

head(split_function_7$`56`) # access specific vector corresponding to frequency 56
head(split_function_7[[1]]) # access first index, which will display the first vector

# access specific index from specific vector
split_function_7[[35]][1] # find the 35th category (vector) and the 1st element of that category vector
