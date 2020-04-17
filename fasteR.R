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
