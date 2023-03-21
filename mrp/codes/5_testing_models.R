# Packages and functions ----------------------------------------------------------------
library(tidyverse)

# Data --------------------------------------------------------------------
Data <- readRDS("mrp/data/real_survey_data_with_riding_projections.rds") %>%
  ## removing observations with NA in the following columns
  drop_na(region, ageC, educ, income, vote_solidity, starts_with("proj"))

# Exploration -------------------------------------------------------------
hist(Data$vote_solidity)

# Create multiple dependent variables -------------------------------------

## Use the appropriate one depending on the model

### ordinal regressions
Data$vote_solidity_factor <- factor(Data$vote_solidity,
                             levels = 0:10 / 10)

### for a glm binomial model
Data$fragile <- ifelse(Data$vote_solidity<=0.3, 1, 0)
table(Data$fragile)

## With the threshold at 0.1
Data$fragile1 <- ifelse(Data$vote_solidity<=0.1, 1, 0)
table(Data$fragile1)

## With 3 arbitrary levels
### 0.3- = fragile
### 0.4 to 0.7 = middle
### 0.8+ = solid
Data$vote_solidity_3levels <- NA
Data$vote_solidity_3levels[Data$vote_solidity <= 0.3] <- 0
Data$vote_solidity_3levels[Data$vote_solidity >= 0.4 & Data$vote_solidity <= 0.7] <- 0.5
Data$vote_solidity_3levels[Data$vote_solidity >= 0.8] <- 1
table(Data$vote_solidity_3levels)

### Factorize it
Data$vote_solidity_3levels <- factor(Data$vote_solidity_3levels,
                                 levels = c(0,0.5,1))



# Create models ------------------------------------------------------------

## Linear models ------------------------------------------------------------

### lm 1 --------------------------------------------------------------------
model <- lm(vote_solidity ~
              ageC * educ * income*factor(region) +
              proj_CAQ*proj_PCQ*proj_QS*proj_PQ*proj_PLQ,
            data=Data)

summary(model) ## adj r2 of 0.07, not good

## predict model on Data
pred <- predict(model, Data)
hist(pred)

### lm 2 --------------------------------------------------------------------
model <- lm(vote_solidity ~
              ageC * educ + income + factor(region) +
              proj_CAQ*proj_PCQ*proj_QS*proj_PQ*proj_PLQ,
            data=Data)

summary(model) ## adj r2 of 0.05, not good

## predict model on Data
pred <- predict(model, Data)
hist(pred)


### lm 3 --------------------------------------------------------------------
model <- lm(vote_solidity ~
              ageC * educ + income + factor(region) +
              proj_CAQ+proj_PCQ+proj_QS+proj_PQ+proj_PLQ,
            data=Data)

summary(model) ## adj r2 of 0.05, not good

## predict model on Data
pred <- predict(model, Data)
hist(pred)


### lm 4 (step wo interactions) --------------------------------------------------------------------
model <- lm(vote_solidity ~
              ageC * educ + income + factor(region) +
              proj_CAQ+proj_PCQ+proj_QS+proj_PQ+proj_PLQ,
            data=Data)

summary(model) ## adj r2 of 0.05, not good

model2 <- step(model)
summary(model2) ## adj r2 of 0.05

### lm 5 (step with interactions) --------------------------------------------------------------------
model <- lm(vote_solidity ~
              ageC * educ * income * factor(region) +
              proj_CAQ*proj_PCQ*proj_QS*proj_PQ*proj_PLQ,
            data=Data)

summary(model) ## adj r2 of 0.05, not good

model2 <- step(model)
summary(model2) ## adj r2 of 0.05

## Ordinal, 11 levels DV -----------------------------------------------------------

### ordinal 1 ----------------------------------------------------------------------
model <- MASS::polr(formula = vote_solidity_factor ~
                      ageC * educ + income * factor(region) +
                      proj_CAQ*proj_PCQ*proj_QS*proj_PQ*proj_PLQ,
                    data = Data)

## summary of model
summary(model)

pred <- predict(model, Data)
table(pred) ## no prediction between 0.1 and 1?

### ordinal 2 ----------------------------------------------------------------------
model <- MASS::polr(formula = vote_solidity_factor ~
                      ageC * educ + income + factor(region) +
                      proj_CAQ+proj_PCQ+proj_QS+proj_PQ+proj_PLQ,
                    data = Data)

## summary of model
summary(model)

pred <- predict(model, Data)
table(pred) ## no prediction between 0.1 and 1?


### ordinal 2 ----------------------------------------------------------------------
model <- MASS::polr(formula = vote_solidity_factor ~
                      ageC * educ + income + factor(region) +
                      proj_CAQ+proj_PCQ+proj_QS+proj_PQ+proj_PLQ,
                    data = Data)

## summary of model
summary(model)

pred <- predict(model, Data)
table(pred) ## no prediction between 0.1 and 1?


### ordinal 3 (step) ----------------------------------------------------------------------
model <- MASS::polr(formula = vote_solidity_factor ~
                      ageC * educ + income + factor(region) +
                      proj_CAQ+proj_PCQ+proj_QS+proj_PQ+proj_PLQ,
                    data = Data)

## summary of model
summary(model)

model2 <- step(model)
summary(model2)

pred <- predict(model2, Data)
table(pred) ## no prediction between 0.1 and 1?


### ordinal 4 (step with interactions) ----------------------------------------------------------------------
model <- MASS::polr(formula = vote_solidity_factor ~
                      ageC * educ + income*factor(region) +
                      proj_CAQ*proj_PCQ*proj_QS*proj_PQ*proj_PLQ,
                    data = Data)

## summary of model
summary(model)

model2 <- step(model)
summary(model2)

pred <- predict(model2, Data)
table(pred) ## no prediction between 0.1 and 1?


## Ordinal, 3 levels DV -----------------------------------------------------

### ordinal 5 ----------------------------------------------------------------------
model <- MASS::polr(formula = vote_solidity_3levels ~
                      ageC * educ * income +
                      proj_CAQ*proj_PCQ*proj_QS*proj_PQ*proj_PLQ,
                    data = Data)

## summary of model
summary(model)

pred <- predict(model, Data)
table(pred) ## no prediction at 0.5? completely different than original DV


### ordinal 6 ----------------------------------------------------------------------
model <- MASS::polr(formula = vote_solidity_3levels ~
                      ageC + educ + income +
                      proj_CAQ+proj_PCQ+proj_QS+proj_PQ+proj_PLQ,
                    data = Data)

## summary of model
summary(model)

pred <- predict(model, Data)
table(pred) ## no prediction at 0.5? completely different than original DV

### ordinal 7 (step) ----------------------------------------------------------------------
model <- MASS::polr(formula = vote_solidity_3levels ~
                      ageC + educ + income +
                      proj_CAQ+proj_PCQ+proj_QS+proj_PQ+proj_PLQ,
                    data = Data)

## summary of model
summary(model)

model2 <- step(model)
summary(model2)

pred <- predict(model2, Data)
table(pred) ## no prediction at 0.5? completely different than original DV


### ordinal 8 (step with interactions) ----------------------------------------------------------------------
model <- MASS::polr(formula = vote_solidity_3levels ~
                      ageC * educ * income +
                      proj_CAQ*proj_PCQ*proj_QS*proj_PQ*proj_PLQ,
                    data = Data)

## summary of model
summary(model)

model2 <- step(model)
summary(model2)

pred <- predict(model2, Data)
table(pred) ## no prediction at 0.5? completely different than original DV


## Binomial, threshold at 0.3 --------------------------------------------------------------

### bin 1 ----------------------------------------------------------------------
model <- glm(
  formula = fragile ~
    ageC * educ +
    proj_CAQ*proj_PCQ*proj_QS*proj_PQ*proj_PLQ,
  data = Data,
  family = binomial()
)

## summary of model
summary(model)

pred <- predict(model, newdata = Data, type = "response")
hist(pred)
## Distribution of model response among solid (0) and fragile (1) voters
ggplot(Data, aes(x = pred)) +
  geom_histogram() +
  facet_wrap(~fragile)
#### Not really performant


### bin 2 ----------------------------------------------------------------------
model <- glm(
  formula = fragile ~
    ageC * educ +
    proj_CAQ+proj_PCQ+proj_QS+proj_PQ+proj_PLQ,
  data = Data,
  family = binomial()
)

## summary of model
summary(model)

pred <- predict(model, newdata = Data, type = "response")
hist(pred)
## Distribution of model response among solid (0) and fragile (1) voters
ggplot(Data, aes(x = pred)) +
  geom_histogram() +
  facet_wrap(~fragile)
#### Not really performant


## Binomial, threshold at 0.1 --------------------------------------------------------------

### bin 3 ----------------------------------------------------------------------
model <- glm(
  formula = fragile1 ~
    ageC * educ +
    proj_CAQ*proj_PCQ*proj_QS*proj_PQ*proj_PLQ,
  data = Data,
  family = binomial()
)

## summary of model
summary(model)

pred <- predict(model, newdata = Data, type = "response")
hist(pred)
## Distribution of model response among solid (0) and fragile (1) voters
ggplot(Data, aes(x = pred)) +
  geom_histogram() +
  facet_wrap(~fragile)
#### Not really performant


### bin 4 ----------------------------------------------------------------------
model <- glm(
  formula = fragile1 ~
    ageC * educ +
    proj_CAQ+proj_PCQ+proj_QS+proj_PQ+proj_PLQ,
  data = Data,
  family = binomial()
)

## summary of model
summary(model)

pred <- predict(model, newdata = Data, type = "response")
hist(pred)
## Distribution of model response among solid (0) and fragile (1) voters
ggplot(Data, aes(x = pred)) +
  geom_histogram() +
  facet_wrap(~fragile)
#### Not really performant


## Linear multilevel -------------------------------------------------------

model <- lme4::lmer(vote_solidity ~
                     ageC * educ +
                     proj_CAQ*proj_PCQ*proj_QS*proj_PQ*proj_PLQ +
                      (1|region),
            data=Data)
summary(model)

model <- lme4::lmer(vote_solidity ~
                      ageC * educ + income*region +
                      (1|region),
                    data=Data)
summary(model) ## Almost no variance between regions

model <- lme4::lmer(vote_solidity ~
                      ageC + educ + income +
                      (1|region),
                    data=Data)
summary(model) ## Almost no variance between regions



## Ordinal multilevel ------------------------------------------------------

### with 11 levels in DV
model <- ordinal::clmm(vote_solidity_factor ~
                         ageC * educ +
                         proj_CAQ*proj_PCQ*proj_QS*proj_PQ*proj_PLQ +
                         (1|region),
                    data=Data)

summary(model)

model <- ordinal::clmm(vote_solidity_factor ~
                         ageC * educ * income +
                         (1|region),
                       data=Data)

summary(model)


### with 3 levels in DV
model <- ordinal::clmm(vote_solidity_3levels ~
                         ageC * educ +
                         proj_CAQ*proj_PCQ*proj_QS*proj_PQ*proj_PLQ +
                         (1|region),
                       data=Data)

summary(model)



## Binomial multilevel -----------------------------------------------------

## threshold at 0.3
model <- lme4::glmer(
  formula = fragile ~
    ageC * educ +
    proj_CAQ+proj_PCQ+proj_QS+proj_PQ+proj_PLQ +
    (1|region),
  data = Data,
  family = binomial()
)
## summary of model
summary(model)


## threshold at 0.3
model <- lme4::glmer(
  formula = fragile ~
    ageC * educ * income +
    (1|region),
  data = Data,
  family = binomial()
)
## summary of model
summary(model)



## threshold at 0.1
model <- lme4::glmer(
  formula = fragile1 ~
    ageC * educ +
    proj_CAQ+proj_PCQ+proj_QS+proj_PQ+proj_PLQ +
    (1|region),
  data = Data,
  family = binomial()
)
## summary of model
summary(model)


## Decision trees ---------------------------------------------------------

model <- rpart::rpart(formula = vote_solidity_factor ~
                        ageC + educ + income + factor(region) +
                        proj_CAQ + proj_PLQ + proj_QS + proj_PQ + proj_PCQ,
                      data = Data)
summary(model)
rpart.plot::rpart.plot(model) ## 0.1 or 1?

model <- rpart::rpart(formula = vote_solidity_3levels ~
                        ageC + educ + income + factor(region) +
                        proj_CAQ + proj_PLQ + proj_QS + proj_PQ + proj_PCQ,
                      data = Data)
summary(model)
rpart.plot::rpart.plot(model, box.palette = "blue") ## 0.1 or 1?

model <- rpart::rpart(formula = vote_solidity_3levels ~
                        ageC + educ + income + factor(region),
                      data = Data)
summary(model)
rpart.plot::rpart.plot(model, box.palette = "blue")


model <- rpart::rpart(formula = vote_solidity ~
                        ageC + educ + income + factor(region) +
                        proj_CAQ + proj_PLQ + proj_QS + proj_PQ + proj_PCQ,
                      data = Data)
summary(model)
rpart.plot::rpart.plot(model) ## 0.35 or 0.47?
pred <- predict(model, Data)
table(pred) ## only two possible values?


model <- rpart::rpart(formula = fragile ~
                        ageC + educ + income + factor(region) +
                        proj_CAQ + proj_PLQ + proj_QS + proj_PQ + proj_PCQ,
                      data = Data)
summary(model)
rpart.plot::rpart.plot(model) ## 0.35 or 0.47?
pred <- predict(model, Data)
table(pred) ## only two possible values?
## Distribution of model response among solid (0) and fragile (1) voters
ggplot(Data, aes(x = pred)) +
  geom_histogram() +
  facet_wrap(~fragile)
#### Not really performant



model <- rpart::rpart(formula = fragile1 ~
                        ageC + educ + income + factor(region) +
                        proj_CAQ + proj_PLQ + proj_QS + proj_PQ + proj_PCQ,
                      data = Data)
summary(model)
rpart.plot::rpart.plot(model) ## 0.35 or 0.47?
pred <- predict(model, Data)
table(pred) ## only two possible values?
## Distribution of model response among solid (0) and fragile (1) voters
ggplot(Data, aes(x = pred)) +
  geom_histogram() +
  facet_wrap(~fragile)
#### Not really performant

