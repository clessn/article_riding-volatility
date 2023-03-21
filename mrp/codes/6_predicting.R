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

post_strat$vote_solidity_pred <- predict(model, newdata = post_strat)
hist(post_strat$vote_solidity_pred)

# Aggregate to the riding level ---------------------------------------------------------------
Aggregated <- post_strat %>% 
  group_by(riding_id) %>%
  ## Compute the weighting mean of the vote solidity prediction. The weight is the weight of the strat in the riding
  summarise(fragility_index_mrp = weighted.mean(x = vote_solidity_pred, w = riding_prop)) %>% 
  ## Normalizing between 0 and 1 and inversing it (we are measuring vote fragility, not solidity)
  mutate(fragility_index_mrp = clessnverse::normalize_min_max(fragility_index_mrp)*-1+1)


## Save it
saveRDS(Aggregated, "mrp/data/table_post_strat_fragility.rds")
