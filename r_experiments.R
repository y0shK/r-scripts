# reshape methods?
# transpose t()
# melt()

new_charizard_data_7 <- c(6, 90, 70, 80)
new_blastoise_data_7 <- c(9, 80, 80, 80)
names(new_charizard_data_7) <- c('Dex_Number', 'Attack', 'Defense', 'Speed_Stat')

charizard_blastoise_df_7 <- data.frame(new_charizard_data_7, new_blastoise_data_7)

print(charizard_blastoise_df_7)

library(reshape)

# coerce data frame to matrix
charizard_blastoise_matrix_7 <- data.matrix(charizard_blastoise_df_7)
charizard_blastoise_matrix_7

# take transpose of matrix
charizard_blastoise_matrix_flipped_7 <- t(charizard_blastoise_matrix_7)
charizard_blastoise_matrix_flipped_7

# convert matrix back to data frame to use in melt()
charizard_blastoise_df_flipped_7 <- as.data.frame(charizard_blastoise_matrix_flipped_7)
charizard_blastoise_df_flipped_7

# library reshape uses melt() and cast() to reshape the data from a data.frame to a table-like format
  # much more general in how reshape occurs

# molten data contains the data frame with ID = c(classification), measured = c(variables)
melt_data_7 <- melt(charizard_blastoise_df_flipped_7, id = c('Dex_Number'), measured = c('Attack', 'Defense', 'Speed_Stat'))
print(melt_data_7)

# casted data reshapes the data, classification ~ variable
cast_data_7 <- cast(melt_data_7, Dex_Number ~ variable)
cast_data_7

# Stack Overflow: https://stackoverflow.com/questions/27125342/r-reshape-cast-error

# library reshape2
library(reshape2)

# melt function - wide to long
# cast function - long to wide

# load in a public health dataset - long data
getwd()
setwd('/home/yash/public_health')

world_data_2015 <- read.csv('2015.csv')
happiness_data <- read.csv('world-happiness-report-2019.csv')

world_data_2015 # long data

# fantastic link for library reshape2 and melt/dcast/acast below
# https://seananderson.ca/2013/10/19/reshape/

# dcast()

melt_world_data <- melt(world_data_2015)
head(melt_world_data) # long data

# the default is that all numeric variables should be melted
# set specific identifier variables to not be melted

# find attributes freedom/generosity with respect to health/life
melt_world_data_aes <- melt(world_data_2015, id.vars = c('Health..Life.Expectancy.'),
                           measure.vars = c('Freedom', 'Generosity'),
                           variable.name = 'health_variable',
                           value.name = 'health_value')

head(melt_world_data_aes) # long data, based on health as id rather than numeric

# cast (long to wide) is more tricky, there are multiple reshape functions for casting
  # e.g. dcast to return data.frame, acast to return vector, matrix, array

# id variables are all categorical, variables implicitly mentioned in cast() are numeric
melt_world_data_id <- melt(world_data_2015, id.vars = c('Health..Life.Expectancy.', 'Country', 'Region', 'Happiness.Rank'))

# cast() documentation
# https://www.rdocumentation.org/packages/reshape2/versions/1.4.3/topics/cast

cast_world_data_id <- dcast(melt_world_data_id, Health..Life.Expectancy. + Country + Region + Happiness.Rank ~ variable, fun.aggregate = mean, na.rm = TRUE)
head(cast_world_data_id)

# acast() experiment

# create statistics for each Pokemon
samurott_data <- c(100, 100, 100, 503)
serperior_data <- c(90, 100, 110, 497)
emboar_data <- c(110, 100, 90, 500)

# input names for each vector to classify each stat per Pokemon
names(samurott_data) <- c('Sp..Atk', 'Sp..Def', 'Spd', 'Dex.Number')
names(serperior_data) <- c('Sp..Atk', 'Sp..Def', 'Spd', 'Dex.Number')
names(emboar_data) <- c('Sp..Atk', 'Sp..Def', 'Spd', 'Dex.Number')

# to convert a usable data.frame of information to a meltable and castable form,
  # convert it to matrix, flip the dimensions with the tranpose, then convert back to data.frame
  # this ensures that each column is a piece of information (i.e. stats) rather than a group (i.e. Pokemon)

# construct function to convert data.frame to a usable form for melting/casting
prepare_df_for_melt_cast <- function(df_given) {
  # coerce data frame to matrix
  df_to_matrix <- data.matrix(df_given)
  
  # take transpose of matrix
  matrix_transpose <- t(df_to_matrix)
  
  # convert matrix back to data frame to use in melt()
  matrix_to_flipped_df_of_given <- as.data.frame(matrix_transpose)
  return(matrix_to_flipped_df_of_given)
}

# unusable data.frame
unova_df <- data.frame(samurott_data, serperior_data, emboar_data)
unova_df

# usable data.frame
prepared_unova_df <- prepare_df_for_melt_cast(unova_df) # df to use with melting and casting
prepared_unova_df # should be the transpose of the previous data.frame

# print out data frame
unova_count <- 1
for (i in 1:3) {
  print(unova_df[[unova_count]]) # force R to consider unova data frame a numeric vector and print out its nth array
  unova_count <- unova_count + 1
}

# melt a vector or list - melt default function, melt()
# https://rdrr.io/cran/reshape2/man/melt.default.html

melt_unova_data <- melt(prepared_unova_df, id.vars = c('Dex.Number'), measured.vars = c('Sp..Atk', 'Sp..Def', 'Spd'), factorsAsStrings = TRUE)
melt_unova_data

# use a different set of parameters in acast rather than the formula used by dcast
# https://stackoverflow.com/questions/3768417/how-to-use-acast-reshape2-within-a-function-in-r

cast_unova_data <- acast(prepared_unova_df, list(names(prepared_unova_df)[1], names(prepared_unova_df)[2], names(prepared_unova_df)[3]))
cast_unova_data
