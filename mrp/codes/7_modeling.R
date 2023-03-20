# Packages ----------------------------------------------------------------
library(tidyverse)
source("mrp/functions.R", encoding = "UTF-8")

# Data --------------------------------------------------------------------
Data <- readRDS("mrp/data/real_survey_data_with_riding_projections.rds") %>% 
  drop_na(region, ageC, educ, income, vote_solidity, starts_with("proj"))


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




# Test with a variable with 3 levels --------------------------------------

Data$solid_3levels <- NA
Data$solid_3levels[Data$vote_solidity <= 0.3] <- 0
Data$solid_3levels[Data$vote_solidity >= 0.4 & Data$vote_solidity <= 0.7] <- 0.5
Data$solid_3levels[Data$vote_solidity >= 0.8] <- 1
table(Data$solid_3levels)

### Factorize it
Data$solid_3levels_ord <- factor(Data$solid_3levels,
                                 levels = c(0,0.5,1))


## Ordinal model 1 -----------------------------------------------------------
model <- ordinal::clmm(formula = vote_solidity_factor ~
                         ageC * educ * income +
                         proj_CAQ*proj_PCQ*proj_QS*proj_PQ*proj_PLQ +
                         (1 | region),
                       data = Data)

## summary of model
summary(model)

## random effects by region
ranef(model)$region

## Save model
saveRDS(model, "mrp/models/with_projections/model_ordinal.rds")

## Ordinal model 2: without random effects -----------------------------------------------------------
model <- MASS::polr(formula = vote_solidity_factor ~
                      ageC * educ + income * factor(region) +
                      proj_CAQ*proj_PCQ*proj_QS*proj_PQ*proj_PLQ,
                    data = Data)

## summary of model
summary(model)

## Save model
saveRDS(model, "mrp/models/with_projections/model_ordinal2.rds")

Data$pred <- predict(model, Data)


## Ordinal model 3: without random effects, 3-level DV -----------------------------------------------------------
model <- MASS::polr(formula = solid_3levels_ord ~
                      ageC * educ + income * factor(region) +
                      proj_CAQ*proj_PCQ*proj_QS*proj_PQ*proj_PLQ,
                    data = Data)

## summary of model
summary(model)

model2 <- MASS::stepAIC(model)

summary(model2)

Data$pred <- predict(model2, Data)
table(Data$pred)

## Binomial ----------------------------------------------------------------
model <- glm(
  formula = fragile ~
    ageC * educ + income * region +
    proj_CAQ*proj_PCQ*proj_QS*proj_PQ*proj_PLQ,
  data = Data,
  family = binomial()
)

## summary of model
summary(model)

Data$pred <- predict(model, newdata = Data, type = "response")
hist(Data$pred)

model2 <- MASS::stepAIC(model)

summary(model2)

Data$pred <- predict(model2, newdata = Data, type = "response")
hist(Data$pred)
ggplot(Data, aes(x = pred)) +
  geom_histogram() +
  facet_wrap(~fragile)


## Binomial 2 ----------------------------------------------------------------
model <- glm(
  formula = fragile1 ~
    ageC * educ + income * region +
    proj_CAQ*proj_PCQ*proj_QS*proj_PQ*proj_PLQ,
  data = Data,
  family = binomial()
)

## summary of model
summary(model)

Data$pred <- predict(model, newdata = Data, type = "response")
hist(Data$pred)

ggplot(Data, aes(x = pred)) +
  geom_histogram() +
  facet_wrap(~fragile1)


model2 <- MASS::stepAIC(model)

summary(model2)

Data$pred <- predict(model2, newdata = Data, type = "response")
hist(Data$pred)
ggplot(Data, aes(x = pred)) +
  geom_histogram() +
  facet_wrap(~fragile1)


# Linear model ------------------------------------------------------------

model <- lm(vote_solidity ~
              ageC*educ + income*region +
              proj_CAQ+proj_PCQ+proj_QS+proj_PQ+proj_PLQ,
            data = Data)
summary(model)

model2 <- MASS::stepAIC(model, method = "backward")
summary(model2)

Data$pred <- predict(model2, Data)
hist(Data$pred)

