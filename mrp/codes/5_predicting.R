# Packages and functions ----------------------------------------------------------------
library(tidyverse)
source("mrp/functions.R", encoding = "UTF-8")


# Model -------------------------------------------------------------------
model <- readRDS("mrp/models/model_glm.rds")

# Data --------------------------------------------------------------------
post_strat <- readRDS("mrp/data/post_strat_table.rds")
census <- readRDS("mrp/data/census_data.rds") ## for region of riding

regions <- census$region
names(regions) <- census$riding_id

post_strat$region <- regions[as.character(post_strat$riding_id)]

post_strat$pred <- predict(model, newdata = post_strat, type = "response")
hist(post_strat$pred)
table(post_strat$pred)

