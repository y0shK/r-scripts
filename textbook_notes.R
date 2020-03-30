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
# Ch. 5 from the online textbook
charizard_stats <- c(78, 84, 78, 109, 85, 100, 534)
names(charizard_stats) <- c('HP', 'Attack', 'Defense', 'Sp. Atk', 'Sp. Def', 'Speed', 'Total')

charizard_stats
typeof(charizard_stats) # double
class(charizard_stats) # numeric

descriptive_charizard_stats <- charizard_stats[1:6]

sum(charizard_stats)
sum(descriptive_charizard_stats) / 6 # element 7 is the total
mean(descriptive_charizard_stats) # ignore element 7

median(descriptive_charizard_stats)

# 'trimmed' mean -> discard most extreme values (largest & smallest) and then take mean
# takes off a certain percentage of the values (to get rid of outliers)
# should probably report actual and trimmed mean

#sort(descriptive_charizard_stats, decreasing = FALSE)
#n_val <- length(descriptive_charizard_stats)
#trimmed_mean_vector <- descriptive_charizard_stats[2:(n_val-1)]
#trimmed_mean_vector

trimmed_mean <- mean(descriptive_charizard_stats, trim=.1)
trimmed_mean

library(lsr)
modeOf(charizard_stats)

mode_stats <- c(20, 22, 22, 13, 43, 65, 32, 34, 56, 742)
sort(mode_stats, decreasing = FALSE)

modeOf(mode_stats)
maxFreq(charizard_stats)
maxFreq(mode_stats)

run_differential <- c(2, 4, 2, 3, 10, 3, 4, 2, 1, 2, 4, 5, 6, 1, 2, 3)
sort(run_differential, decreasing = FALSE)

max(run_differential)
min(run_differential)
range(run_differential)

quantile(run_differential, probs=.5)
quantile(run_differential, probs = c(.25, .75))
IQR(run_differential)

# mean absolute deviation
# like a z-score, but without dividing by standard deviation or standard error (also no normality assumption)

run_differential_avg <- mean(run_differential)
absolute_deviation <- abs(run_differential - run_differential_avg)
avg_absolute_deviation <- mean(absolute_deviation)
print(avg_absolute_deviation)

# variance - how much spread is in the data? (also additive, so var_x+var_y=var_z)
# standard deviation - takes variance and converts it back to original units (root mean squared -> square root)
var(run_differential)
sd(run_differential)

# median absolute deviation - how far is the data point from the "typical" data point?
mad(run_differential, constant=1)
mad(charizard_stats, constant=1)

# why use median absolute deviation?
# it is "robust" like the mean absolute deviation - not affected by extreme values
# MAD * 1.4826 = standard deviation (iff bell curve, else don't use MAD)
# when to use MAD? only during bell curve, otherwise stick to SD

library(psych)
skew(run_differential) # how normal is the data? high skew = low normality, slanted towards high or low values
kurtosi(run_differential) # how 'pointy' is the data? 'just pointy enough' is a normal curve (kurtosis=0), -kurtosis = too flat, +kurtosis = too pointy

# summary variables
summary(descriptive_charizard_stats)

squirtle_over_charizard <- c(TRUE, TRUE, FALSE, TRUE)

count <- 1
for (i in 1:20) {
  if (count %% 3 == 0) {
    squirtle_over_charizard[i] <- TRUE
  }
  else {
    squirtle_over_charizard[i] <- FALSE
  }
  count <- count + 1
}

# random shuffle for vector
# https://stackoverflow.com/questions/13765972/how-to-randomize-a-vector/13765997
squirtle_sample <- sample(squirtle_over_charizard)

summary(descriptive_charizard_stats)
summary(squirtle_sample)

# 1 = squirtle, 2 = ivysaur, ...

# names() for c() vectors
# levels() for factors
pkmn_trainer_preferred_poke <- as.factor(c(1, 1, 1, 1, 2, 3, 3))

levels(pkmn_trainer_preferred_poke) <- c('Squirtle', 'Ivysaur', 'Charizard')
summary(pkmn_trainer_preferred_poke)

describe(descriptive_charizard_stats)
describe(squirtle_sample) # from psych package, only for numerics, not for factors
