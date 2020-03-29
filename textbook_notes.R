# monkeying around with textbook examples
# can remove these from workspace with rm()

# Ch. 4 from the online textbook

# list.files() - print files in the workspace (.Rproj)
# file.choose() - R prompts user to pick a file in the workspace, then outputs the file's directory

getwd()
setwd('/home/yash/public_health')

num_vgc_deaths <- c(3, 10, 5)
print(num_vgc_deaths)

names(num_vgc_deaths) <- c("S1", "Venusaur", "banned")
print(num_vgc_deaths)

vgc_stats <- c('atk' = 320, 'def' = 400, 'spd' = 520)
vgc_stats

vgc_stat_names <- c('atk', 'def', 'spd')
vgc_stat_names

# class() is useful for high-level data, logistics (is the data numbers or letters? 'numeric' or 'character')
# typeof() is useful for low-level, programming (is the data type double, int, string, etc.)
# it's possible to coerce an element to be a different class - as.int, as.string, etc.

class(vgc_stats)
typeof(vgc_stats)

class(vgc_stat_names)
typeof(vgc_stat_names)

# use 'factors' for ordinal variables, where numerics (e.g. 1, 2, 3) are coerced to classifications (e.g. Group 1, Group 2, etc.)
group_numerical <- c(1, 2, 3) # simple numerical vector
group_ordinal <- as.factor(c(1, 2, 3)) # we tell R that 1, 2, 3 is ordinal - i.e. a classification, not a number

print(group_numerical)
print(group_ordinal)
class(group_ordinal) # factor

# assign labels to each level in the ordinal groups/'factors'
group_ordinal <- NULL
group_ordinal <- c(1, 1, 1, 2, 3)

levels(group_ordinal) <- c("Charizard", "Blastoise", "Venusaur")

print(group_ordinal)

data_2016 <- read.csv('2016.csv')
print(data_2016) # printed as a data frame

data_2016_happiness_score <- data_2016$Happiness.Score
happiness_vector <- list(data_2016_happiness_score)
# happiness_vector has 1 element, the whole list

typeof(happiness_vector)
print(happiness_vector)

unlist(happiness_vector) # we tell R we want each element to be each number, not 1 element for the whole list
typeof(happiness_vector) # happiness_vector has many elements, based on list element number

# data frames
# used to represent paired data -> 1 coherent data piece for multiple vectors

poke_names <- c('Charmander', 'Squirtle', 'Bulbasaur')
dex_num <- c(4, 7, 1)
poke_type <- c('fire', 'water', 'grass')

gen1_data <- data.frame(poke_names, dex_num, poke_type)
gen1_data

# rename data frame column name
# https://stackoverflow.com/questions/6081439/changing-column-names-of-a-data-frame
colnames(gen1_data)[1] <- "pokemon names"
gen1_data

print(gen1_data$dex_num)
print(gen1_data$poke_type)
names(gen1_data) # pokemon names, dex_num, poke_type

# lists simply collect types of data and append them
# lists can also have vectors in them as a subset of the list
charizard <- list(age = 25, popular = TRUE, dex_entries=c(4, 5, 6))
print(charizard)
print(charizard$dex_entries)

# formulas - variables that create relationships (numerical) to other variables
# used in logistic regression - e.g. y ~ x in logit()

formula1 <- formula_y ~ formula_x
