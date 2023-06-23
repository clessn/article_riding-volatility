# Packages ----------------------------------------------------------------
library(tidyverse)
library(stargazer)

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

hist(Data$age60p)

# Models -------------------------------------------------------------------

modelCAQ <- lm(volatility ~ fragility_index + educColl + educUniv +
               income100p + age15m29 + age30m44 + age60p + vote2018_CAQ,
             data = Data)
summary(modelCAQ)

modelPLQ <- lm(volatility ~ fragility_index + educColl + educUniv +
                 income100p + age15m29 + age30m44 + age60p + vote2018_PLQ,
               data = Data)
summary(modelPLQ)

modelQS <- lm(volatility ~ fragility_index + educColl + educUniv +
                 income100p + age15m29 + age30m44 + age60p + vote2018_QS,
               data = Data)
summary(modelQS)

modelPQ <- lm(volatility ~ fragility_index + educColl + educUniv +
                income100p + age15m29 + age30m44 + age60p + vote2018_PQ,
              data = Data)
summary(modelPQ)

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


# Stargazer ---------------------------------------------------------------

stargazer(modelCAQ, modelPLQ, modelQS, modelPQ,
          modelAll,
          header = F,
          label = "table_reg",
          single.row = F,
          column.sep.width = "1pt",
          no.space = F,
          type = "latex",
          digits = 2,
          keep.stat = c("rsq"),
          #float.env = "sidewaystable",
          covariate.labels = c("Fragility index",
                               "Collegial education",
                               "University education",
                               "\\parbox{3cm}{Income above\\\\100,000\\$}",
                               "Age 15-29",
                               "Age 30-44",
                               "Age 60+",
                               "\\parbox{3cm}{2018 vote share\\\\CAQ}",
                               "\\parbox{3cm}{2018 vote share\\\\PLQ}",
                               "\\parbox{3cm}{2018 vote share\\\\QS}",
                               "\\parbox{3cm}{2018 vote share\\\\PQ}"),
          dep.var.labels = c("Campaign volatility"),
          notes = c("N = 125", "Source: Quebec census data and 8 monthly surveys from January to August (n = 9135)"),
          notes.align = "l", 
          notes.append = T,
          notes.label = "")



