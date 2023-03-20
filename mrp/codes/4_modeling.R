# Packages and functions ----------------------------------------------------------------
library(tidyverse)
library(ordinal)
library(fastDummies)
source("mrp/functions.R")

# Data --------------------------------------------------------------------
Data <- readRDS("mrp/data/real_survey_data.rds")


# Exploration -------------------------------------------------------------
hist(Data$vote_solidity)
## Since the dependent variable does not follow a normal distribution,
### we will factorize it and do an ordinal regression


# Factorize dependent variable --------------------------------------------

Data$vote_solidity <- factor(Data$vote_solidity,
                             levels = 0:10 / 10)

table(Data$vote_solidity)
unique(Data$vote_solidity)


# Create model ------------------------------------------------------------

model <- ordinal::clmm(formula = vote_solidity ~
                         ageC * educ * income +
                         (1 | region),
                       data = Data)

## summary of model
summary(model)

## random effects by region
ranef(model)$region

## Save model
saveRDS(model, "mrp/models/model.rds")


