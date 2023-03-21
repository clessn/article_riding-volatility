# Packages and functions ----------------------------------------------------------------
library(tidyverse)

# Data --------------------------------------------------------------------

## survey data for model
Data <- readRDS("mrp/data/real_survey_data_with_riding_projections.rds")

## post strat table
post_strat <- readRDS("mrp/data/post_strat_table.rds")

# Model -------------------------------------------------------------------

model <- lm(vote_solidity ~
              ageC * educ + income +
              proj_CAQ*proj_PCQ*proj_QS*proj_PQ*proj_PLQ,
            data=Data)

summary(model)

# Predict on post_strat ---------------------------------------------------

post_strat$pred <- predict(model, newdata = post_strat)
hist(post_strat$pred)

# Aggregate to the riding level ---------------------------------------------------------------

Aggregated <- post_strat %>% 
  group_by(riding_id, riding_name) %>% 
  summarise(mean_vote_solidity = weighted.mean(x = pred, w = riding_prop))


