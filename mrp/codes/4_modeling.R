# Packages and functions ----------------------------------------------------------------
library(tidyverse)
library(ordinal)

# Data --------------------------------------------------------------------
Data <- readRDS("mrp/data/real_survey_data.rds")


# Exploration -------------------------------------------------------------
hist(Data$vote_solidity)


# Factorize dependent variable --------------------------------------------

### for ordinal regressions

Data$vote_solidity_factor <- factor(Data$vote_solidity,
                             levels = 0:10 / 10)

table(Data$vote_solidity_factor)
unique(Data$vote_solidity_factor)



# Binarise dependent variable ---------------------------------------------

### for a glm binomial model
Data$fragile <- ifelse(Data$vote_solidity<=0.3, 1, 0)
table(Data$fragile)

# Create models ------------------------------------------------------------

## Ordinal model 1 -----------------------------------------------------------
model <- ordinal::clmm(formula = vote_solidity_factor ~
                         ageC * educ * income +
                         (1 | region),
                       data = Data)

## summary of model
summary(model)

## random effects by region
ranef(model)$region

## Save model
saveRDS(model, "mrp/models/model_ordinal.rds")


## Linear model ------------------------------------------------------------
model <- lmerTest::lmer(formula = vote_solidity ~
                          ageC * educ + income * factor(region) +
                          (1 | region),
                        data = Data)
summary(model)

## random effects by region
ranef(model)$region

## Save model
saveRDS(model, "mrp/models/model_linear.rds")


## Ordinal model 2: without random effects -----------------------------------------------------------
model <- MASS::polr(formula = vote_solidity_factor ~
                      ageC * educ + income * factor(region),
                    data = Data)

## summary of model
summary(model)

## Save model
saveRDS(model, "mrp/models/model_ordinal2.rds")


## Decision tree ---------------------------------------------------------
model <- rpart::rpart(formula = vote_solidity_factor ~
                        ageC + educ + income + factor(region),
                      data = Data)

rpart.plot::rpart.plot(model)

## Save model
saveRDS(model, "mrp/models/decision_tree.rds")


## Ordinal model 3: without interactions -----------------------------------------------------------
model <- MASS::polr(formula = vote_solidity_factor ~
                      ageC + educ + income + factor(region),
                    data = Data, Hess = F)

## summary of model
summary(model)

## Save model
saveRDS(model, "mrp/models/model_ordinal3.rds")



## Binomial --------------------------------------------------------------

model <- glm(
  formula = vote_solidity_factor ~
    ageC * educ + income * region,
  data = Data,
  family = binomial()
)

## summary of model
summary(model)

## Save model
saveRDS(model, "mrp/models/model_glm.rds")


