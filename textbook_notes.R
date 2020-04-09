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

pokemon_vector <- c('Charmander', 'Squirtle', 'Bulbasaur')
atk_vector <- c(5, 4, 3)
def_vector <- c(3, 4, 5)
spd_vector <- c(4, 4, 4)
value_vector <- c(9, 8, 7)
#names(stats_vector) <- c(pokemon_vector[1], pokemon_vector[2], pokemon_vector[3])
#stats_vector

pokemon_data_frame <- data.frame(pokemon_vector, atk_vector, def_vector, spd_vector)
pokemon_data_frame

pokemon_data_frame$pokemon_vector

# basic summary call; q1, median, q3, ...
summary(pokemon_data_frame)

# psych package - good for case studies, clinical trials, etc, ...
# describeBy() from psych package - segregate data by specific groups
describeBy(pokemon_data_frame, atk_vector)
describeBy(pokemon_data_frame, def_vector)
describeBy(pokemon_data_frame, spd_vector)

# by() - same as describeBy(), but has a third parameter for a function call
by(data=pokemon_data_frame, INDICES=atk_vector, FUN = summary) # most general = most powerful
# by() gives built-in blocking (e.g. drug/placebo, treatment/homeopathy) w/ INDICES

# aggregate() lets you combine variables' impact in a data.frame format
aggregate(formula = value_vector ~ atk_vector + def_vector + spd_vector, data = pokemon_data_frame, FUN = mean)

getwd()
setwd('~/pokemon_analysis')

pokemon_csv <- read.csv('pokemon.csv')

pokemon_csv_df <- data.frame(pokemon_csv)
pokemon_csv_df

# tinker around with different summary functions()

pokemon_csv_df$Generation <- as.factor(pokemon_csv_df$Generation) # let the data.frame know that generation is nominal, not numerical
summary(pokemon_csv_df)
by(pokemon_csv_df, INDICES=pokemon_csv_df$Generation, FUN=summary)
describe(pokemon_csv_df)
describeBy(pokemon_csv_df, pokemon_csv_df$Generation) # 'block' by generation

z_score_atk_vector <- c()

for (i in 1:length(pokemon_csv_df$Name)) {
  z_score_atk_local <- (pokemon_csv_df$Attack[i] - mean(pokemon_csv_df$Attack)) / sd(pokemon_csv_df$Attack)
  z_score_atk_vector[i] <- z_score_atk_local
}

z_score_atk_vector
pokemon_csv_df_numeric <- c(pokemon_csv_df$Attack, pokemon_csv_df$Defense, pokemon_csv_df$Defense)

by(pokemon_csv_df, INDICES = pokemon_csv_df$Attack, FUN=summary)

cor(pokemon_csv_df$Attack, pokemon_csv_df$HP)
cor(pokemon_csv_df$Defense, pokemon_csv_df$HP)

# cor() is for linear regression; how well does the line fit?
# cor() with method=spearman converts the data so that it's ordinal
  # spearman covers nonlinear inc or dec relationships (like ln(x))
  # kendall (third method) looks at whether one data point does more of the behaviors than the other, factors that additional data in
cor(pokemon_csv_df$Attack, pokemon_csv_df$HP, method='spearman')
cor(pokemon_csv_df$Defense, pokemon_csv_df$HP, method = "spearman")

# correlate() from the lsr package is similar to cor(), but it will ignore string factors
# cor(pokemon_csv_df) # error
correlate(pokemon_csv_df) # ignores all string factors

pkmn_atk_vector <- c(300, 200, 100, 150, NA, 220)
# mean(pkmn_atk_vector) -> NA
mean(pkmn_atk_vector, na.rm=TRUE) # ignores NA and computes the rest
# Chapter 6
fibonacci <- c(1, 1, 2, 3, 5, 8, 13)

# plot() has typical arguments (x/y) and 'graphical parameters' (which are generic arguments for all functions, like col, which are abstracted so arguments are specific to their function)
# pch parameter changes what each point looks like (default is pch = 1, white circle)
# can also specify plot size (default cex=1), line type (lty='solid' by default)

# type = points only
plot.default(fibonacci, 
     main='fibonacci graph', 
     sub='below x-axis', 
     xlab='index number', 
     ylab='fibonacci element', # # index is x-axis, vector[i] is y-axis
     font.main=1, # plain text for title
     cex.main=1, # normal size
     font.axis = 2, # bold text
     col.lab='springgreen4', pch=16) 

# type = line only (connecting each point)
plot.default(fibonacci, 
     main='fibonacci graph', 
     sub='below x-axis', 
     xlab='index number', 
     ylab='fibonacci element', # # index is x-axis, vector[i] is y-axis
     font.main=1, # plain text for title
     cex.main=1, # normal size
     font.axis = 2, # bold text
     col.lab='springgreen4', type='l', pch=16)

# histogram-like vertical bars up until each point
plot.default(fibonacci, 
             main='fibonacci graph', 
             sub='below x-axis', 
             xlab='index number', 
             ylab='fibonacci element', # # index is x-axis, vector[i] is y-axis
             font.main=1, # plain text for title
             cex.main=1, # normal size
             font.axis = 2, # bold text
             col.lab='springgreen4', type='h', pch=16)

# both lines and points (without lines crossing over points)
plot.default(fibonacci, 
             main='fibonacci graph', 
             sub='below x-axis', 
             xlab='index number', 
             ylab='fibonacci element', # # index is x-axis, vector[i] is y-axis
             font.main=1, # plain text for title
             cex.main=1, # normal size
             font.axis = 2, # bold text
             col.lab='springgreen4', type='b', pch=17)

# alter different components of graph
plot.default(fibonacci, 
     type = 'b',
     col = 'springgreen3',
     pch=19, # change plotting character
     cex=3, # 3x usual size
     lty=2, # dashed lines
     lwd = 4) # line width 4x usual
# histograms
pkmn_atk_hist <- c(550, 200, 150, 150, 600, 500, 300, 100, 520, 300, 500, 400, 550, 340, 420, 590, 300, 400, 290, 290, 370, 380, 380, 250, 270) # pretend attack stats

hist(pkmn_atk_hist) # default settings, R calibrates
hist(pkmn_atk_hist, breaks = 5) # around 5, whatever looks pleasant/efficient
# hist(pkmn_atk_hist, breaks = 0:length(pkmn_atk_hist)) # vector hard-codes 5 (human override of R's algorithm)

# nice histogram
hist(pkmn_atk_hist, 
     xlab = 'attack stat',
     density = 10, # 10 shading lines/inch
     angle = 40, # angle of shading lines
     border = 'gray20',
     col = 'springgreen4',
     labels = TRUE,
     ylim = c(0, 10))

# boxplots
summary(pkmn_atk_hist)
boxplot(pkmn_atk_hist)

# nice boxplot
boxplot(pkmn_atk_hist,
        xlab = 'attack stat',
        ylab = 'stat value',
        border ='gray50',
        frame.plot = TRUE,
        staplewex = 0, # remove whiskers
        whisklty = 1) # solid line

# use which() to filter by outliers in boxplot
which(pkmn_atk_hist > 500) # which index corresponds to an attack greater than 500?

# create a boxplot with multiple individual boxes
attach(pokemon_csv)
boxplot(formula = HP ~ Generation, data = pokemon_csv)

# nice boxplot with multiple individual plots
boxplot(formula = HP ~ Generation,
        data = pokemon_csv,
        xlab = 'Generation',
        ylab = 'HP',
        frame.plot = FALSE, # don't draw a frame around the plots
        staplewex = 0, # don't draw whiskers,
        staplecol = 'white',
        boxwex = 0.75, # narrow the box length
        boxfill = 'springgreen4', # box color,
        whisklty = 1, # solid lines, not dashed,
        whiskcol = 'limegreen',
        boxcol = 'black',
        outcol = 'purple',
        outpch = 16, # element denoting outlier,
        outcex = 0.75, # shrink outlier size
        medlty = 'blank', # remove lines for medians
        medpch = 20, # draw solid dots for the medians
        medlwd = 1.25) # make the median solid dots bigger

# nice boxplot with multiple individual plots
# create a function to pass individual stat relationships to
plot_pokemon_boxplot <- function(stat1, stat2, stat1_name, stat2_name) {
  independent = stat2_name
  dependent = stat1_name
  
  boxplot(formula = stat1 ~ stat2,
        data = pokemon_csv,
        xlab = independent,
        ylab = dependent,
        frame.plot = FALSE, # don't draw a frame around the plots
        staplewex = 0, # don't draw whiskers,
        staplecol = 'white',
        boxwex = 0.75, # narrow the box length
        boxfill = 'springgreen4', # box color,
        whisklty = 1, # solid lines, not dashed,
        whiskcol = 'limegreen',
        boxcol = 'black',
        outcol = 'purple',
        outpch = 16, # element denoting outlier,
        outcex = 0.75, # shrink outlier size
        medlty = 'blank', # remove lines for medians
        medpch = 20, # draw solid dots for the medians
        medlwd = 1.25) # make the median solid dots bigger
}

plot_pokemon_boxplot(HP, Generation, 'HP', 'Generation')
plot_pokemon_boxplot(Attack, Generation, 'Attack', 'Generation')
plot_pokemon_boxplot(Defense, Generation, 'Defense', 'Generation')
plot_pokemon_boxplot(Sp..Atk, Generation, 'Sp. Atk', 'Generation')
plot_pokemon_boxplot(Sp..Def, Generation, 'Sp. Def', 'Generation')
plot_pokemon_boxplot(Speed, Generation, 'Speed', 'Generation')

# scatter plots
# plot points
plot(x = HP,
     y = Defense,
     xlab = 'HP',
     ylab = 'Defense',
     pch = 16, # put xlim or ylim in if necessary
     col = 'springgreen2',
     frame.plot = FALSE) # no box
     
# create a linear model for HP and defense
reg_model <- lm(Defense ~ HP, data = pokemon_csv)
reg_model_resid <- resid(reg_model)
summary(reg_model)

# https://stats.idre.ucla.edu/r/faq/how-can-i-do-a-scatterplot-with-regression-line-or-any-other-lines/
with(pokemon_csv, plot(x = HP,
                       y = Defense,
                       xlab = 'HP',
                       ylab = 'Defense',
                       main = 'HP vs. Defense',
                       pch = 20, # put xlim or ylim in if necessary
                       col = 'springgreen2',
                       frame.plot = FALSE))
abline(reg_model) # abline() creates a trendline with the given lm

# plot residuals
plot(HP, reg_model_resid, data = pokemon_csv,
     ylab = 'Residuals', xlab = 'HP',
     main = 'HP vs. Residuals (defense)')
abline(0, 0) # horizontal line across the graph

cor(x = pokemon_csv$HP, pokemon_csv$Defense)

# pairs() compares different attributes of a data frame in a matrix-like format, with graphs in each element
pairs(formula = ~ HP + Defense + Sp..Def, 
      data = pokemon_csv)
pairs(formula = ~ Attack + Sp..Atk + Speed,
      data = pokemon_csv)

# bar graphs
defense_values <- tabulate(pokemon_csv$Defense) # tabulate creates a simple numeric vector
defense_pokemon <- levels(pokemon_csv$Defense)

# par() - change the space on a bar graph
# default bar graph just plots values, need to feed in more data
barplot(height = defense_values, names.arg = defense_pokemon,
        las = 2, main='defense') # rotate the labels vertically

# Chapter 7
pkmn_hp_vector_7 <- c(pokemon_csv$HP)
#pkmn_def_vector_7 <- c(pokemon_csv$Defense)

use_count <- c()
counter <- 1

for (i in 1:length(pkmn_hp_vector_7)) {
  if (pkmn_hp_vector_7[i] > 50) {
    use_count[counter] <- "true"
  }
  else {
    use_count[counter] <- "false"
  }
  counter <- counter + 1
}

print(use_count)

print(pkmn_hp_vector_7)
table(pkmn_hp_vector_7) # frequency table

# frequency table; x = HP amount, y = arbitrary true/false metric (e.g. usable in competitive?)
# some random ordered pair, e.g. (70, true) -> 57 implies that there are 57 pokemon in the data who 1) have HP == 70 and 2) are usable

use_from_hp_7 <- table(use_count, pkmn_hp_vector_7)
use_from_hp_7

# proportions rather than counts, prop = count / total
prop_from_hp_7 <- prop.table(x = use_from_hp_7) # count / total
prop_from_hp_row_7 <- prop.table(x = use_from_hp_7, margin = 1) # margin = 1 to say that 'total' is row by row; all elements in row sum to 1

prop_from_hp_7

prop_from_hp_row_7

# create a table from a data frame
pkmn_table_7 <- data.frame(use_count, pkmn_hp_vector_7)
pkmn_table_7

# operations to transform a variable
df_pkmn_speed_7 <- data.frame(pokemon_csv$Speed)

which(df_pkmn_speed_7 > 50)

df_store_pkmn_speed_7 <- data.frame("fast_pokemon" = which(df_pkmn_speed_7 > 50))
#df_pkmn_speed_7$fast_pokemon <- which(df_pkmn_speed_7 > 50)
#df_pkmn_speed_7$better_than_average <- which(quantile(df_pkmn_speed_7) > 0.5)

df_pkmn_speed_7
df_store_pkmn_speed_7

# cutting numeric variable data into categories
pkmn_stats_7 <- c(200, 150, 200, 220, 450, 100, 210, 120, 430, 500)

pkmn_stats_breaks_7 <- seq(from = 0, to = 600, by = 200)
pkmn_stats_breaks_7

pkmn_stats_labels_7 <- c("not viable", "viable", "extraordinary")

# cut() takes numerical data and assigns it into categories
# e.g. breaks <- seq(from = 0, to = 1, by = 1)
  # labels <- c('benign', 'malignant')
pkmn_cut_7 <- cut(pkmn_stats_7,
                  breaks = pkmn_stats_breaks_7, # edges for the categories
                  labels = pkmn_stats_labels_7)

pkmn_cut_7

# let R decide the breaks
pkmn_autonomous_7 <- cut(pkmn_stats_7, breaks = 3)
print(pkmn_autonomous_7)

benign_data_7 <- c(0, 0, 0, 1, 1, 1, 0)
benign_breaks_7 <- seq(from = -1, to = 1, by = 1)
benign_labels_7 <- c('benign', 'malignant') # number of edges; -1 to 0 is 'benign' & 0 to 1 is 'malignant' - no need to start at -1

benign_cut_7 <- cut(benign_data_7, breaks = benign_breaks_7, labels = benign_labels_7)

benign_cut_7

# useful mathematical functions?
round(x = 4.3, digits = 1) # truncates to 4, can provide digit number
# for sig figs, use signif(x = some value, digits = n)

# integer division - %/%
# modulus - %%
# interestingly, negative modulus rounds up
  # e.g. -42 mod 10 = 5, not 4, because remainder 2 necessitates a +1
# logarithms - log10(), log2(), log() is ln(x), exp(x) = e^x

# extract subset of a vector
viable_speed_7 <- which(pokemon_csv$Speed > 50)
print(viable_speed_7)
# R has an %in% operator, like pythonic in

pkmn_speed_vector_7 <- c(pokemon_csv$Speed) # call data frame element as a vector
print(pkmn_speed_vector_7[2:3]) # call specific subset
print(pkmn_speed_vector_7[-(1:500)]) # call all indices except 1:500

# reddit documentation for Error in `[.data.frame`(x, r, vars, drop = drop) : undefined columns selected
# https://www.reddit.com/r/RStudio/comments/85h7xh/subsetting_data_error_in_dataframex_r_vars_drop/

# subset a vector using the subset() method
# x = data frame, subset = df$element conditional, select = element (not df$element)
df_subset_7 <- subset(pokemon_csv, subset = pokemon_csv$Speed > 50, select = Speed)
df_subset_7

# can also subset by pulling out ordered pairs
pkmn_csv_as_df_7 <- data.frame(pokemon_csv)

# in pokemon_csv, x1:y1 is column number (what Pokemon?) and x2:y2 is stats for that Pokemon (name, HP, type, etc.)
df_subset_index_7 <- pkmn_csv_as_df_7[4:5, 3:4]
print(df_subset_index_7)

df_subset_by_name_7 <- pkmn_csv_as_df_7[c('Type.1', 'Type.2')] # subset by name rather than element number
df_subset_by_name_7

is_pokemon_dewott <- pokemon_csv$Name == 'Dewott'
is_pokemon_dewott # prints out entire array of false, 1 true
pokemon_csv[is_pokemon_dewott, c('X.', 'HP', 'Attack', 'Defense')] # pass in boolean to call specific elements just for Dewott

# can only subset rows or columns using an empty comma
pokemon_csv[, 1:2] # keep all rows, only first two columns
pokemon_csv[1:2, ] # keep all columns, only first two rows
pokemon_csv[,] # technically a valid command but doesn't subset anything

# can use negative indices to delete rows
pokemon_csv[1:3, -5] # rows 1-3, delete column 5, show the rest

# R has curious dropping behavior for a 1xnumber data frame
pokemon_csv[5, ] # a data frame for one row (row 5) and all columns
pokemon_csv[, 5] # NOT a data frame, actually a vector of type/elements of column 5
pokemon_csv[, 5, drop = FALSE]# optional drop parameter, type data frame

# square brackets for columns
pokemon_csv[1:2] # all rows, first two columns, R automatically assumes you want all rows
pokemon_csv[5] # n rows with 1 column, in format data frame
pokemon_csv[[5]] # n rows with 1 column, in format numeric/string vector (i.e. forcing R to drop the data frame to a vector)

# access element n from a list - list[n]
# access element n from 1 row of a data frame - data_frame_name[[n]]
  # this tells R that the data frame should be interpreted AS a list (i.e. a numeric vector, etc.)

# handling data - sorting, flipping, merging data
# sort numeric & string vectors

attack_nums_7 <- c(200, 300, 330, 150, 320)
sort(attack_nums_7) # can also set decreasing = TRUE

text_sort_7 <- c('atk', 'def', 'spd', 'generation')
text_sort_factor_7 <- as.factor(text_sort_7)

sort(text_sort_7)

print(text_sort_factor_7) # not in alpha order
sort(text_sort_factor_7) # still not in alpha order, sorts by order of factor levels given

# library lsr has sortFrame() function which can sort data frames
# lsr also has rowCopy and colCopy functions, which take a vector and create a matrix n times (x = vector, times = n)

# bind vectors together with data.frame, cbind(), and rbind()
charizard_vector_7 <- c(6, 100, 70, 90) # pokedex number, atk, def, spd
blastoise_vector_7 <- c(9, 85, 85, 90)

kanto_df_7 <- data.frame(charizard_vector_7, blastoise_vector_7)
kanto_df_7

# use 'column bind' function to combine the two vectors into a column matrix
kanto_cbind_7 <- cbind(charizard_vector_7, blastoise_vector_7)
kanto_cbind_7

# similar 'row bind' function, but for a horizontal rather than vertical matrix
kanto_rbind_7 <- rbind(charizard_vector_7, blastoise_vector_7)
kanto_rbind_7
