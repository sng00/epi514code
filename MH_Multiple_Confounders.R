# Adjusting for Multiple Confounders & Effect Modifiers using Mantel-Haenszel
# Author: Sarah Garcia
  # Adapted from code provided by Augusto Ferraris (https://github.com/a-ferraris/EPI-3-STARS/blob/main/simplified%20mh%20analysis%20with%202by2byk%20tables.R)
# Class: EPI 514
# Date Created: 05-10-2023
# Last Modified: 05-10-2023
# Libraries required:
  # tidyverse
  # epiR

#############################################################################################################################################b

# INTRODUCTION ----

# epi.2by2() requires the following format:

#                   |  outcome = yes (1) |  outcome = no(2)
# exposure = yes (1)|        X           |        X
# exposure = no (2) |        X           |        X


# we need to create a table for each level of our adjustment variables:

# example: 
            # adj var 1 = 1, adj var 2 = 1

#                   |  outcome = yes (1) |  outcome = no(2)
# exposure = yes (1)|        X           |        X
# exposure = no (2) |        X           |        X

            # adj var 1 = 1, adj var 2 = 2

#                   |  outcome = yes (1) |  outcome = no(2)
# exposure = yes (1)|        X           |        X
# exposure = no (2) |        X           |        X

# this would continue for each level of each adjustment variable

#############################################################################################################################################b

# MULTIPLE CONFOUNDERS ----

## EXPOSURE = 2 LEVELS & OUTCOME = 2 LEVELS ----

# make sure your exposure and outcome are coded as 1 for yes and 2
# for no for exposures or outcomes with mutliple levels see next section

# first we create an array with 2+ k dimensions with the xtabs() function
strat <- xtabs(~exposure + outcome + adjvar1 + adjvar2 + ... + adjvarK, data = data)

# now we need to convert this to 3 dimensions to feed into epi.2by2() using the array() function
# this decomposes the tables into numbers and then we can rearrange the data into 2x2xk tables with 3 dimensions
# we need to tell R how many tables to create based on the number of levels our adjustment variables have 
    # by multiplying the number of levels of each adjustment variable

array <- array(strat,
                  dim = c(2,2,n), # this creates a 3 dimension array with n tables
                  list(exposure = c('exposure yes', 'exposure no'), # this includes our exposure variable with relevant labels
                       outcomes = c('outcome yes', 'outcome no'), # this includes our outcome variable with relevant labels
                       confounders = 1:n)) 

# example:
  # exposure = exp
  # outcome = out
  # adjustment variable 1 = adjvar1 (2 levels)
  # adjustment variable 2 = adjvar2 (4 levels)
  # adjustment variable 3 = adjvar3 (2 levels)
    # this means we would make 2x4x2 = 16 tables

strat <- xtabs(~exp + out + adjvar1 + adjvar2 + adjvar3, data = data)

mh_array <- array(strat,
                  dim = c(2, 2, 16),
                  list(exp = c('yes', 'no'), # make sure this matches your coding 
                        out = c('yes', 'no'),
                        confounders = 1:16))

# now we use this array with epi.2by2()
epi.2by2(mh_array, method = 'cross.sectional')
  

## EXPOSURE OR OUTCOME = 2+ LEVELS ----

# for an exposure or outcome with multiple levels, we need to create bins for each level comparing to the referent group
# we'll use the mutate() function to create new variables that code each level as 1 for yes and 2 for no/referent group

# example: exposure has 3 levels

data <- data %>% mutate(
  lvl1_bin = case_when(
    exp =="level 1" ~ 1,
    exp =="ref" ~ 2),
  lvl1_bin = factor(lvl1_bin, labels=c("level 1", "ref")), 
  lvl2_bin = case_when(
    exp =="level 2" ~ 1,
    exp =="ref" ~ 2),
  lvl2_bin = factor(lvl2_bin, labels=c("level 2", "ref"))
)

# now we use the same approach as before, just creating arrays for each bin

# example:
  # exposure = exp (4 levels)
  # outcome = out
  # adjustment variable 1 = adjvar1 (2 levels)
  # adjustment variable 2 = adjvar2 (4 levels)
    # this means we would make 2x4 = 8 tables

strat_1 <- xtabs(~lvl1_bin + out + adjvar1 + adjvar2, data = data)
strat_2 <- xtabs(~lvl2_bin + out + adjvar1 + adjvar2, data = data)

array_1 <- array(strat_1,
               dim = c(2,2,8),
               list(lvl1_bin = c('level 1', 'ref'),
                    out = c('yes', 'no'),
                    confounders = 1:8))
array_2 <- array(strat_2,
                 dim = c(2,2,8),
                 list(lvl2_bin = c('level 2', 'ref'),
                      out = c('yes', 'no'),
                      confounders = 1:8))

# now we use each array with epi.2by2()

epi.2by2(array_1, method = 'cross.sectional')
epi.2by2(array_2, method = 'cross.sectional')

#############################################################################################################################################b

# EFFECT MODIFIER ----

## EXPOSURE OR OUTCOME = 2+ LEVELS ----
# to stratify by another variable, everything stays the same except when we use xtabs() we will subset the data

strat_y <- xtabs(~exposure + outcome + adjvar1 + adjvar2 + ... + adjvarK, data = data, subset = stratvar == 'yes')
strat_n <- xtabs(~exposure + outcome + adjvar1 + adjvar2 + ... + adjvarK, data = data, subset = stratvar == 'no')

array_y <- array(strat_y,
               dim = c(2,2,n),
               list(exposure = c('exposure yes', 'exposure no'),
                    outcomes = c('outcome yes', 'outcome no'),
                    confounders = 1:n)) 

array_n <- array(strat_n,
                 dim = c(2,2,n),
                 list(exposure = c('exposure yes', 'exposure no'),
                      outcomes = c('outcome yes', 'outcome no'),
                      confounders = 1:n))

## EXPOSURE OR OUTCOME = 2+ LEVELS ----

# if your exposure or outcome has more than 2 levels, you can use the bins again

strat_y_1 <- xtabs(~lvl1_bin + outcome + adjvar1 + adjvar2 + ... + adjvarK, data = data, subset = stratvar == 'yes')
strat_n_1 <- xtabs(~lvl1_bin + outcome + adjvar1 + adjvar2 + ... + adjvarK, data = data, subset = stratvar == 'no')

strat_y_2 <- xtabs(~lvl2_bin + outcome + adjvar1 + adjvar2 + ... + adjvarK, data = data, subset = stratvar == 'yes')
strat_n_2 <- xtabs(~lvl2_bin + outcome + adjvar1 + adjvar2 + ... + adjvarK, data = data, subset = stratvar == 'no')

array_y_1 <- array(strat_y_1,
                 dim = c(2,2,n),
                 list(lvl1_bin = c('level 1', 'ref'),
                      outcomes = c('outcome yes', 'outcome no'),
                      confounders = 1:n)) 

array_n_1 <- array(strat_n_1,
                 dim = c(2,2,n),
                 list(lvl1_bin = c('level 1', 'ref'),
                      outcomes = c('outcome yes', 'outcome no'),
                      confounders = 1:n))
array_y_2 <- array(strat_y_2,
                   dim = c(2,2,n),
                   list(lvl2_bin = c('level 2', 'ref'),
                        outcomes = c('outcome yes', 'outcome no'),
                        confounders = 1:n)) 

array_n_2 <- array(strat_n_2,
                   dim = c(2,2,n),
                   list(lvl2_bin = c('level 2', 'ref'),
                        outcomes = c('outcome yes', 'outcome no'),
                        confounders = 1:n))

epi.2by2(array_y_1)
epi.2by2(array_n_1)
epi.2by2(array_y_2)
epi.2by2(array_n_2)


# I hope this helps! :)