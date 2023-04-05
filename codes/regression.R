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

modelA <- lm(volatility ~ fragility_index + educColl + educUniv +
               french + income100p + age60p,
             data = Data)
summary(modelA)

model <- lm(volatility ~ fragility_index + educColl + educUniv +
              french + income100p + age60p,
            data = Data)
summary(model)

modelB <- lm(volatility ~ fragility_index + educColl + educUniv +
               income100p + age60p +
               vote2018_CAQ,
             data = Data)
summary(modelB)

modelC <- lm(volatility ~ fragility_index + educColl + educUniv +
               income100p + age60p +
               vote2018_PLQ,
             data = Data)
summary(modelC)

modelD <- lm(volatility ~ fragility_index + educColl + educUniv +
               income100p + age60p +
               vote2018_QS,
             data = Data)
summary(modelD)

modelE <- lm(volatility ~ fragility_index + educColl + educUniv +
               income100p + age60p +
               vote2018_PQ,
             data = Data)
summary(modelE)

modelF <- lm(volatility ~ fragility_index + educColl + educUniv +
               income100p + age60p +
               vote2018_CAQ + vote2018_PLQ +
               vote2018_QS + vote2018_PQ,
             data = Data)
summary(modelF)

modelG <- lm(volatility ~ fragility_index + educColl + educUniv +
               income100p + age60p +
               vote2018_PLQ + vote2018_QS + vote2018_PQ,
             data = Data)
summary(modelG)

modelH <- lm(volatility ~ fragility_index + educColl + educUniv +
               income100p + age60p +
               vote2018_PQ + vote2018_CAQ,
             data = Data)
summary(modelH)

modelI <- lm(volatility ~ fragility_index + educColl + educUniv +
               income100p + age60p + french +
               vote2018_QS + vote2018_PQ,
             data = Data)
summary(modelI)

modelJ <- lm(volatility ~ fragility_index + educColl + educUniv +
               income100p + age60p +
               vote2018_CAQ + vote2018_QS,
             data = Data)
summary(modelJ)

modelK <- lm(volatility ~ fragility_index + educColl + educUniv +
               income100p + age60p +
               vote2018_PQ + vote2018_PLQ,
             data = Data)
summary(modelK)

modelL <- lm(volatility ~ fragility_index + educColl + educUniv +
               income100p + age60p +
              vote2018_CAQ + vote2018_PQ + vote2018_QS,
             data = Data)
summary(modelL)

modelM <- lm(volatility ~ fragility_index + educColl + educUniv +
               income100p + age60p +
              vote2018_QS + vote2018_PLQ + vote2018_CAQ,
             data = Data)
summary(modelM)

# Stargazer ---------------------------------------------------------------

stargazer(modelA, modelB, modelC,
          modelD, modelE, modelF,
          modelG, modelH, modelJ,
          modelK, modelL, modelM,
          modelI,
          header = F,
          single.row = T,
          column.sep.width = "1pt",
          no.space = F,
          type = "latex",
          digits = 2,
          keep.stat = c("rsq"),
          float.env = "sidewaystable",
          covariate.labels = c("Fragility index",
                               "Collegial education",
                               "University education",
                               "French speaking",
                               "\\parbox{3cm}{Income above\\\\100,000\\$}",
                               "Age 60+",
                               "\\parbox{3cm}{2018 vote share\\\\CAQ}",
                               "\\parbox{3cm}{2018 vote share\\\\PLQ}",
                               "\\parbox{3cm}{2018 vote share\\\\QS}",
                               "\\parbox{3cm}{2018 vote share\\\\PQ}"),
          dep.var.labels = c("Campaign volatility"),
          notes = c("N = 125", "Source: Quebec census data and 8 monthly surveys from January to August (n = 9135)"),
          notes.align = "l", 
          notes.append = T,
          notes.label = "",
          report = "vc*")



