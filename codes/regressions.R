# Packages ----------------------------------------------------------------
library(tidyverse)
library(modelsummary)

# Data --------------------------------------------------------------------
Volatility <- readRDS("data/table3_aggregatedData.rds") %>% 
  select(riding_id, volatility, fragility_index)

Data <- readRDS("mrp/data/census_data.rds") %>% 
  select(riding_id, starts_with("age"), french,
         starts_with("income"), starts_with("educ"),
         starts_with("vote2018")) %>% 
  left_join(Volatility, ., by = "riding_id") %>% 
  select(-riding_id) %>% 
  mutate(high_educ = educColl + educUniv,
         age60p = age60m74 + age75p)

# Models -------------------------------------------------------------------

modelCAQ <- lm(volatility ~ fragility_index + educColl + educUniv +
               income100p + age15m29 + age30m44 + age60p + vote2018_CAQ,
             data = Data)
summary(modelCAQ)

vmCAQ <- sandwich::vcovHC(modelCAQ, type = 'HC1')

modelPLQ <- lm(volatility ~ fragility_index + educColl + educUniv +
                 income100p + age15m29 + age30m44 + age60p + vote2018_PLQ,
               data = Data)
summary(modelPLQ)

vmPLQ <- sandwich::vcovHC(modelPLQ, type = 'HC1')

modelQS <- lm(volatility ~ fragility_index + educColl + educUniv +
                 income100p + age15m29 + age30m44 + age60p + vote2018_QS,
               data = Data)
summary(modelQS)

vmQS <- sandwich::vcovHC(modelQS, type = 'HC1')

modelPQ <- lm(volatility ~ fragility_index + educColl + educUniv +
                income100p + age15m29 + age30m44 + age60p + vote2018_PQ,
              data = Data)
summary(modelPQ)

vmPQ <- sandwich::vcovHC(modelPQ, type = 'HC1')

modelAll <- lm(
  volatility ~ fragility_index + educColl + educUniv +
    income100p + age15m29 + age30m44 + age60p +
    vote2018_CAQ +
    vote2018_PLQ +
    vote2018_QS +
    vote2018_PQ,
  data = Data
)
summary(modelAll)

vmAll <- sandwich::vcovHC(modelAll, type = 'HC1')

# Model summary -----------------------------------------------------------

modelsummary(list(modelCAQ, modelPLQ, modelQS, modelPQ,
                  modelAll),
             stars = TRUE,
             output = "latex",
             statistic = c("({std.error})",
                           "conf.int"),
             fmt = 2,
             coef_rename = c("fragility_index" = "Fragility index",
                             "educColl"  = "Collegial education",
                             "educUniv"  = "University education",
                             "income100p"  = "Income above 100,000$",
                             "age15m29"  = "Age 15-29",
                             "age30m44"  = "Age 30-44",
                             "age60p"  = "Age 60+",
                             "vote2018_CAQ"  = "2018 vote share: CAQ",
                             "vote2018_PLQ"  = "2018 vote share: PLQ",
                             "vote2018_QS"  = "2018 vote share: QS",
                             "vote2018_PQ"  = "2018 vote share: PQ"),
             notes = list("N = 125",
                          "Source: Quebec census data and 8 monthly surveys from January to August (n = 9135)",
                          "Robust standard errors are in parentheses. 95\% confidence interval are in brackets.",
                          "Regression table made using modelsummary"),
             title = "Regression models with campaign volatility as the dependent variable",
             gof_map = "r.squared")



