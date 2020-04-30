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
setwd('~/r-datasets')

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

# Ch 9 - use different graphing techniques to effectively display data
plot(pima_csv$glucose, xlab='glucose amount', ylab='frequency', main = 'glucose amount vs. frequency', col = 'springgreen4', pch = 16)
hist(pima_csv$glucose, xlab='Glucose amount', main = 'Glucose histogram', col = c('red', 'blue', 'springgreen2', 'gray', 'orange', 'springgreen4', 'magenta', 'cyan')) # color vector needs to match number of breaks
  # use different colors for visualization: https://stackoverflow.com/questions/38810453/how-to-plot-a-histogram-with-different-colors-in-r

getwd()
setwd('/home/yash/r-datasets')

world_data_2015 <- read.csv('2015.csv')
#happiness_data <- read.csv('world-happiness-report-2019.csv')

# cut() function - divides range of x into intervals and sorts values depending on which interval they fall into
  # leftmost interval = 1, ...
  # https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/cut

# use colorRampPalette to create a transition depending on values' percentile (e.g. 25th is different from 75th)
  # use that color transition to visually segregate different groups of values
  # https://stackoverflow.com/questions/9946630/colour-points-in-a-plot-differently-depending-on-a-vector-of-values

palette_gdp <- colorRampPalette(c('magenta', 'cyan')) # two 'endpoints' of color
gdp <- world_data_2015$Economy..GDP.per.Capita.
happiness_score <- world_data_2015$Happiness.Score

# create 5 intervals and then use cut() to assign all ordered pairs in the data to an interval
  # then assign them their respective colorRampPalette color
  # each data point has its xLoc, yLoc, and color -> informative and easy-to-peruse graph
happiness_score_color <- palette_gdp(5)[as.numeric(cut(happiness_score, breaks=5))]

plot(gdp, happiness_score, pch=16, col=happiness_score_color, xlab='GDP', ylab='Happiness rating', main='GDP vs. happiness', font.main=1, cex.main=1) # unbolded and same-size-as-labels title

# chapter 9 - base graphing functions in R
getwd()
setwd('~/r-datasets')

pokemon_csv <- read.csv('pokemon.csv')

# different pokemon.csv, change values
pkmn_atk <- pokemon_csv$attack
pkmn_spd <- pokemon_csv$speed
pkmn_sp_atk <- pokemon_csv$sp_attack

#pkmn_spd

# 4 arbitrary intervals - slow, average, faster, 'highest tier' -> add factors for color
# create a range of hexadecimal colors and then use cut() to assign them to each data point (Pokemon)
palette_by_speed <- colorRampPalette(c('magenta', 'cyan')) 

# assign each Pokemon based on speed to an interval
intervals_by_speed <- palette_by_speed(4)[as.numeric(cut(pkmn_spd, breaks=4))] # cut must be numeric

# use colorRampPalette to return a vector of colors that is used in plot()
  #https://bookdown.org/rdpeng/exdata/plotting-and-color-in-r.html
return_hexadecimals_rb <- colorRampPalette(c('red', 'blue'))
color_ramp_palette_col_rb <- c(return_hexadecimals(4))

plot(pkmn_atk, pkmn_sp_atk, col=color_ramp_palette_col_rb, cex.main=1, xlab='Attack', ylab='Sp. Attack', main = 'Offensive potential in Pokemon', font.main=1, pch=16)
legend('topleft', legend=c(x='slow', 'average', 'fast', 'highest tier'), col=color_ramp_palette_col_rb, cex=0.75, pch=16, bg='lightblue') # have to specify pch argument, otherwise dots will not show up

# use new Pokemon.csv file for histogram analysis
exp_growth <- pokemon_csv$experience_growth
base_total_stats <- pokemon_csv$base_total

exp_growth
base_total_stats

return_hexadecimals_bg <- colorRampPalette(c('skyblue', 'limegreen'))
color_ramp_palette_col_bg <- c(return_hexadecimals_bg(5))

hist(exp_growth, base_total_stats, col=color_ramp_palette_col_bg, cex.main=1, xlab='EXP growth', ylab='Base stats', main='EXP growth vs. Base stats', pch=16, breaks=5, ylim=c(0,600))
legend('topleft', legend=c(x='fluctuating', 'slow', 'medium slow', 'medium fast', 'fast'), col=color_ramp_palette_col_bg, cex=0.7, pch=16, bg='salmon')

# use new Pokemon.csv file for histogram analysis
exp_growth <- pokemon_csv$experience_growth
base_total_stats <- pokemon_csv$base_total

exp_growth
base_total_stats

return_hexadecimals_bg <- colorRampPalette(c('skyblue', 'limegreen'))
color_ramp_palette_col_bg <- c(return_hexadecimals_bg(5))

<<<<<<< HEAD
plot(pkmn_atk, pkmn_sp_atk, col=color_ramp_palette_col, cex.main=1, xlab='Attack', ylab='Sp. Attack', main = 'Offensive potential in Pokemon', font.main=1, pch=16)
legend('topleft', legend=c(x='slow', 'average', 'fast', 'highest tier'), col=color_ramp_palette_col, cex=0.75, pch=16) # have to specify pch argument, otherwise dots will not show up

# Ch. 17 - linear regression analysis

# brief MLB linear regression with R
age_mlb <- mlb$Age
height_mlb <- mlb$Height
weight_mlb <- mlb$Weight

# use tapply to examine all variables per player
mlb_tapply_representation <- tapply(weight_mlb, age_mlb, mean)
mlb_tapply_representation

# create a linear model for the regression body
mlb_lin_model <- lm(weight_mlb ~ age_mlb, mlb)
mlb_lin_resid <- resid(mlb_lin_model)

summary(mlb_lin_model)

# create a color palette to further contextualize age ranges
return_hexadecimals_mlb <- colorRampPalette(c('red', 'blue'))
color_ramp_palette_col_mlb <- c(return_hexadecimals_mlb(4))
  
# https://stats.idre.ucla.edu/r/faq/how-can-i-do-a-scatterplot-with-regression-line-or-any-other-lines/
with(mlb, plot(x = age_mlb,
               y = weight_mlb,
               xlab = 'Age',
               ylab = 'Weight',
               main = 'Age vs. Weight',
               pch = 16,
               col = color_ramp_palette_col_mlb))

legend('topright', legend=c(x='18-22', '23-30', '31-40', '41-50'), col=color_ramp_palette_col_mlb, cex=0.75, pch=16)

abline(mlb_lin_model)

# plot residuals
plot(age_mlb, mlb_lin_resid, data = mlb,
     ylab = 'Residuals', xlab = 'Age',
     main = 'Age vs. Residuals (weight)', pch=16, col = color_ramp_palette_col_mlb)
legend('topright', legend=c(x='18-22', '23-30', '31-40', '41-50'), col=color_ramp_palette_col_mlb, cex=0.75, pch=16)
abline(0, 0) # horizontal line y=0 to plot and contextualize residuals

# additional R regression topics
mlb_rownums <- split(1:nrow(mlb), mlb$PosCategory) # split data by row numbers of each position
str(mlb_rownums) # str() displays the structure of a called R object

# split() produces a list blocked by position
  # then iterate through list to call lm, blocked by position
position_names <- c('Catcher', 'Infielder', 'Outfielder', 'Pitcher')
mlb_position_df <- data.frame() # instantiate an empty data frame to block by position

attach(mlb) # attaching mlb data directly lets values vary; values are static if called as df$subset
for (pos in position_names) {
  pos_rows <- mlb_rownums[[pos]]
  lm_age <- lm(Weight ~ Age, data = mlb[pos_rows, ]) # only rows with specific positions, all columns
  newrow <- lm_age$coefficients
  mlb_position_df <- rbind(mlb_position_df, newrow)
}

row.names(mlb_position_df) <- position_names # rows
names(mlb_position_df) <- c('coefficient', 'slope') # columns
mlb_position_df

# Pokemon analysis
# arbitrary question - does attack vary by generation?

# find a certain piece of data (generation number) within all rows and block by that data
pokemon_rownums <- split(1:nrow(pokemon_csv), pokemon_csv$Generation)
str(pokemon_rownums) # like head(), but whole - very compact

generations <- c(1:6)

pkmn_stats_df <- data.frame() # empty df instantiation, which can slowly be appended to (like Java or Python)

# rownames chooses a variable to make independent and 'block' as in a statistical experiment
# variables that are iterated through essentially act as dependent variables ('factors')

attach(pokemon_csv)

for (gen in generations) {
  # go through each generation and append the specific sets of row numbers (e.g. gen 1 = 1 - x, gen 2 = x+1 - y, etc.)
  stat_rows <- pokemon_rownums[[gen]] # use [[]] to access the vector data, rather than the list containing the vector with []
  
  # create lm by generation, input = df[[vector of each stat for all Pokemon]] -> output = linear regression for each stat
  # stat_rows needs the unlist() method because data needs to be in vector form, i.e. stat_rows is type list -> transformed to vector for linReg
  lm_by_generation <- lm(Total ~ Attack, data = pokemon_csv[unlist(stat_rows), ], na.action = na.exclude) # use just two variables; multivariate later
  
  # find coefficients and use rbind to apply them row-wise to the data frame
  newrow_pokemon <- lm_by_generation$coefficients
  pkmn_stats_df <- rbind(pkmn_stats_df, newrow_pokemon)
}
print(stat_rows)

row.names(pkmn_stats_df) = c('Kanto', 'Johto', 'Hoenn', 'Sinnoh', 'Unova', 'Kalos')
names(pkmn_stats_df) <- c('coefficients', 'slope')
pkmn_stats_df

# multivariate linear regression in R
# good resource: https://data.library.virginia.edu/getting-started-with-multivariate-multiple-regression/

# function to check the reliability of the multivariate regression
statistical_checks <- function(x) {
  head(resid(x)) # residuals
  coef(x) # coefficients
  sigma(x) # residual standard error
  vcov(x) # variance-covariance matrix
}

# one dependent variable - total stats depending on all other stats
# lm() is a flexible enough function that it can create a multivariate as well as a single-variable regression

multivariate_pokemon_regression <- lm(Total ~ Attack + Defense + Sp..Atk + Sp..Def + Speed, data = pokemon_csv)
summary(multivariate_pokemon_regression)
statistical_checks(multivariate_pokemon_regression)

# two dependent variables - can we predict a Pokemon's attacking stats given its defensive stats and speed?

multivariate_two_variables <- lm(Attack + Sp..Atk ~ HP + Defense + Sp..Def + Speed, data = pokemon_csv)
summary(multivariate_two_variables)
statistical_checks(multivariate_two_variables)
