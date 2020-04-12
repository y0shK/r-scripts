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
samurott_data <- c('Samurott', 100, 100, 100)
serperior_data <- c('Serperior', 90, 100, 110)
emboar_data <- c('Emboar', 110, 100, 90)
names(samurott_data) <- c('Name', 'Sp..Atk', 'Sp. Def', 'Speed')

unova_vector <- c(samurott_data, serperior_data, emboar_data)
unova_vector

# melt a vector - melt default function, melt()
# https://rdrr.io/cran/reshape2/man/melt.default.html

# melt_aes takes the wide unova_vector data and transforms it to long data
melt_unova_data_aes <- melt(unova_vector, id.vars = c('Name'), 
                            measure.vars = c('Sp..Atk', 'Sp. Def', 'Speed'),
                            variable.name = 'Stat type')

melt_unova_data_aes
