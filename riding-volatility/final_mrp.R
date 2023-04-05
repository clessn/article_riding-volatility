#### Le choix des interactions inclus dans les modèles est basé
#### sur les graphiques générés dans `1.z.mrp/test_mrp.R` qui se trouvent
#### dans le SharedFolder

# Packages ----------------------------------------------------------------
library(tidyverse)
library(ordinal)
library(fastDummies)
#library(rstanarm)
#library(MCMCglmm)
#library(bayesplot)
source("functions.R", encoding = "UTF-8")

options(mc.cores = parallel::detectCores())

# Load post strat ---------------------------------------------------------
postStrat <- readRDS("_SharedFolder_riding-volatility/Data/warehouse/stratTable.rds")

# Load riding population number -------------------------------------------
riding_pop <- readRDS("_SharedFolder_riding-volatility/Data/warehouse/riding_pop_number.rds")

# Load Pol data -----------------------------------------------------------
PolData <- readRDS("_SharedFolder_riding-volatility/Data/warehouse/PolData_for_mrp.rds") %>% 
  select(
    # column identifying each respondent
    id,
    # Columns for the ridings
    riding, riding_id,
    # SES columns
    male, ageC, lang, educ, income,
    # VD: irc columns
    ircCAQ, ircPLQ, ircQS, ircPQ, ircPCQ
  )
str(PolData)

# Use riding_id -----------------------------------------------------------

riding_infos <- readxl::read_excel("_SharedFolder_riding-volatility/Data/lake/prov_ridings_ids.xlsx")

riding_ids <- riding_infos$riding_id
names(riding_ids) <- riding_infos$riding_name

postStrat$riding_id <- riding_ids[postStrat$riding]
sum(is.na(postStrat$riding_id))


# Factor some variables --------------------------------------------------------
PolData$riding_id <- factor(PolData$riding_id, levels=names(sort(table(PolData$riding_id))))
str(PolData)

ggplot(PolData, aes(x = riding_id)) +
  geom_bar(stat = "count", color = NA,
           fill = "#931412") +
  clessnverse::theme_clean_light()


PolData$ageC <- factor(PolData$ageC, levels = c("age15m29",
                                                "age30m44",
                                                "age45m59",
                                                "age60m74",
                                                "age75p"))

PolData$income <- factor(PolData$income, levels = c("incomeLow",
                                                    "incomeMid",
                                                    "incomeHigh"))

PolData$educ <- factor(PolData$educ, levels = c("educHSB",
                                                "educColl",
                                                "educUniv"))

PolData$lang <- factor(PolData$lang)


# Create ridingGroup ------------------------------------------------------
PolData$ridingGroup <- cut(as.numeric(as.character(PolData$riding_id)),
                           breaks = c(0, 155, 238,
                                      300, 338, 400,
                                      500, 600,
                                      648, 700, 800,
                                      830, 900, 918,
                                      1000))
table(PolData$ridingGroup)

####*************************
####*** START SKIP ******####
####*************************

# Find interactions between SES and ridings --------------------------------

## If we find interactions, add ses*riding to the formulas
#### Use riding group/region

### Vis
vis <- c("male", "ageC", "lang", "educ", "income")

test <- interaction_ridingGroups(vis, "ircCAQ", PolData)
str(test)

### CAQ ####
vd <- "ircCAQ"
interaction_ridingGroups(vis, vd, PolData) %>%
  ggplot(aes(x = factor_x,
             y = y_axis,
             group = factor_y)) +
  geom_line(aes(linetype = factor_y,
                color = factor_y),
            linewidth = 0.75,
            show.legend = F) +
  facet_grid(cols = vars(facet_x),
             scales = "free") +
  clessnverse::theme_clean_light() +
  ylab(paste0("Mean of ", vd)) +
  xlab("") +
  ggtitle("Mean IRC of regions (each line is a region)") +
  geom_smooth(aes(group = NULL))
ggsave(paste0("_SharedFolder_riding-volatility/models/interactions_", vd, ".png"))

#### lang
#### male
#### income


### PLQ ####
vd <- "ircPLQ"
interaction_ridingGroups(vis, vd, PolData) %>%
  ggplot(aes(x = factor_x,
             y = y_axis,
             group = factor_y)) +
  geom_line(aes(linetype = factor_y,
                color = factor_y),
            linewidth = 0.75,
            show.legend = F) +
  facet_grid(cols = vars(facet_x),
             scales = "free") +
  clessnverse::theme_clean_light() +
  ylab(paste0("Mean of ", vd)) +
  xlab("") +
  ggtitle("Mean IRC of regions (each line is a region)") +
  geom_smooth(aes(group = NULL))
ggsave(paste0("_SharedFolder_riding-volatility/models/interactions_", vd, ".png"))

#### ageC


### QS ####
vd <- "ircQS"
interaction_ridingGroups(vis, vd, PolData) %>%
  ggplot(aes(x = factor_x,
             y = y_axis,
             group = factor_y)) +
  geom_line(aes(linetype = factor_y,
                color = factor_y),
            linewidth = 0.75,
            show.legend = F) +
  facet_grid(cols = vars(facet_x),
             scales = "free") +
  clessnverse::theme_clean_light() +
  ylab(paste0("Mean of ", vd)) +
  xlab("") +
  ggtitle("Mean IRC of regions (each line is a region)") +
  geom_smooth(aes(group = NULL))
ggsave(paste0("_SharedFolder_riding-volatility/models/interactions_", vd, ".png"))

#### income
#### lang


### PQ ####
vd <- "ircPQ"
interaction_ridingGroups(vis, vd, PolData) %>%
  ggplot(aes(x = factor_x,
             y = y_axis,
             group = factor_y)) +
  geom_line(aes(linetype = factor_y,
                color = factor_y),
            linewidth = 0.75,
            show.legend = F) +
  facet_grid(cols = vars(facet_x),
             scales = "free") +
  clessnverse::theme_clean_light() +
  ylab(paste0("Mean of ", vd)) +
  xlab("") +
  ggtitle("Mean IRC of regions (each line is a region)") +
  geom_smooth(aes(group = NULL))
ggsave(paste0("_SharedFolder_riding-volatility/models/interactions_", vd, ".png"))

#### ageC
#### educ
#### male


### PCQ ####
vd <- "ircPCQ"
interaction_ridingGroups(vis, vd, PolData) %>%
  ggplot(aes(x = factor_x,
             y = y_axis,
             group = factor_y)) +
  geom_line(aes(linetype = factor_y,
                color = factor_y),
            linewidth = 0.75,
            show.legend = F) +
  facet_grid(cols = vars(facet_x),
             scales = "free") +
  clessnverse::theme_clean_light() +
  ylab(paste0("Mean of ", vd)) +
  xlab("") +
  ggtitle("Mean IRC of regions (each line is a region)") +
  geom_smooth(aes(group = NULL))
ggsave(paste0("_SharedFolder_riding-volatility/models/interactions_", vd, ".png"))


#### educ
#### income
#### lang

# Transform irc variables -------------------------------------------------
#### IRC must go from 0 to 1 where 0.5 is the voting cutoff
PolData$ircCAQ <- (PolData$ircCAQ+1)/2
PolData$ircPLQ <- (PolData$ircPLQ+1)/2
PolData$ircQS <- (PolData$ircQS+1)/2
PolData$ircPQ <- (PolData$ircPQ+1)/2
PolData$ircPCQ <- (PolData$ircPCQ+1)/2


#### Factor dependent variables to make ordinal regressions
PolData$ircCAQ <- factor(PolData$ircCAQ,
                         levels = seq(0, 1, by = 0.05),
                         ordered = T)

PolData$ircPLQ <- factor(PolData$ircPLQ,
                         levels = seq(0, 1, by = 0.05),
                         ordered = T)

PolData$ircQS <- factor(PolData$ircQS,
                        levels = seq(0, 1, by = 0.05),
                        ordered = T)

PolData$ircPQ <- factor(PolData$ircPQ,
                        levels = seq(0, 1, by = 0.05),
                        ordered = T)

PolData$ircPCQ <- factor(PolData$ircPCQ,
                         levels = seq(0, 1, by = 0.05),
                         ordered = T)

####*************************
####*** STOP SKIP ******####
####*************************


# Create riding_test (random effect) --------------------------------------
PolData$riding_test <- as.character(PolData$riding_id)
## if we want to have a random effect by region:
######PolData$riding_test <- as.numeric(substr(PolData$riding_test, 1,2))
PolData$riding_test <- as.numeric(as.character(PolData$riding_id))


# Factor ridingGroup ------------------------------------------------------
PolData$ridingGroup <- factor(PolData$ridingGroup)
table(PolData$ridingGroup)


####********************************************************************###
# MODELS ------------------------------------------------------------------
####********************************************************************###

PolData <- PolData %>% 
  drop_na(male, ageC, educ, lang, income, riding_id)


## CAQ ####
#modelCAQ <- clmm(
#  formula = ircCAQ ~ lang*educ + ageC*lang + lang*ridingGroup +
#    ageC + educ + male*ridingGroup + income*ridingGroup +
#    (1 | riding_test),
#  data = PolData
#)
#saveRDS(modelCAQ, "_SharedFolder_riding-volatility/models/caq_ordinal_rci2.rds")
modelCAQ <- readRDS("_SharedFolder_riding-volatility/models/caq_ordinal_rci2.rds")
summary(modelCAQ)

## PLQ ####
#modelPLQ <- clmm(
#  formula = ircPLQ ~ ageC*lang +
#    lang + ageC*ridingGroup + educ + male + income +
#    (1 | riding_test),
#  data = PolData
#)
#saveRDS(modelPLQ, "_SharedFolder_riding-volatility/models/plq_ordinal_rci2.rds")
modelPLQ <- readRDS("_SharedFolder_riding-volatility/models/plq_ordinal_rci2.rds")
summary(modelPLQ)

## QS ####

#### income
#### lang

#modelQS <- clmm(
#  formula = ircQS ~ lang*ridingGroup + ageC + educ + male +
#    income*ridingGroup + educ*income + lang*income + income*male +
#    (1 | riding_test),
#  data = PolData
#)
#saveRDS(modelQS, "_SharedFolder_riding-volatility/models/qs_ordinal_rci2.rds")
modelQS <- readRDS("_SharedFolder_riding-volatility/models/qs_ordinal_rci2.rds")
summary(modelQS)

## PQ ####

#### ageC
#### educ
#### male

#modelPQ <- clmm(
#  formula = ircPQ ~ male*income + ageC*lang + lang +
#    ageC*ridingGroup + educ*ridingGroup + male*ridingGroup + income +
#    (1 | riding_test),
#  data = PolData
#)
#saveRDS(modelPQ, "_SharedFolder_riding-volatility/models/pq_ordinal_rci2.rds")
modelPQ <- readRDS("_SharedFolder_riding-volatility/models/pq_ordinal_rci2.rds")
summary(modelPQ)

## PCQ ####

#### educ
#### income
#### lang

#modelPCQ <- clmm(
#  formula = ircPCQ ~ ageC*lang + lang*educ + male*ageC + 
#    lang*ridingGroup + ageC + educ*ridingGroup + male + income*ridingGroup +
#    (1 | riding_test),
#  data = PolData
#)
#saveRDS(modelPCQ, "_SharedFolder_riding-volatility/models/pcq_ordinal_rci2.rds")
modelPCQ <- readRDS("_SharedFolder_riding-volatility/models/pcq_ordinal_rci2.rds")
summary(modelPCQ)


####********************************************************************###
# PREDICT ------------------------------------------------------------------
####********************************************************************###

postStrat$riding_test <- as.numeric(as.character(postStrat$riding_id))
postStrat$ridingGroup <- cut(as.numeric(as.character(postStrat$riding_id)),
                             breaks = c(0, 155, 238,
                                        300, 338, 400,
                                        500, 600,
                                        648, 700, 800,
                                        830, 900, 918,
                                        1000))
table(postStrat$ridingGroup)


### Simulate the potential simulation
newdata <- postStrat[sample(1:nrow(postStrat), 5000),]
sample_preds <- predict_continuous(modelCAQ, newdata, sign_treshold = 0.1)
hist(sample_preds)
sd_CAQ <- sd(sample_preds)

# plot two densitys
dist <- data.frame(xi = rnorm(100,
                              #mean = sample_preds[sample(1:length(sample_preds), 1)],
                              mean = quantile(sample_preds, 0.75),
                              sd = sd_CAQ/2))
ggplot(data.frame(sample_preds), aes(x = sample_preds)) +
  geom_density(fill = "pink", color = "pink",
               alpha = 0.5) +
  geom_density(data = dist, aes(x = xi),
               fill = "lightblue", color = "lightblue", alpha = 0.5) +
  geom_vline(xintercept = mean(dist$xi))
#### In pink: the random distribution of 5000 observations in postStrat.
#### In blue: the random simulation of 250 occurences of 1 observation of postStrat
####           using rnorm with the predicted value of 1 observation as the mean
#####          and the sd of the random sample of predictions divided by 2 (arbitrary choice).
#### This will allow more variation in the predicted data instead of predicting each similar
#### observation in postStrat as one individual


### 4 Add weight of riding in the population (pop_riding/pop_qc)
###   column in postStrat
riding_pop_n <- riding_pop$pop_n
names(riding_pop_n) <- riding_pop$riding_id
pop_qc <- sum(riding_pop_n)
postStrat2 <- postStrat %>% 
  mutate(riding_pop = riding_pop_n[as.character(riding_id)],
         riding_weight = riding_pop/pop_qc,
         ### 5 Create weight in population (prct*riding_weight)
         ###   and n in population
         n_pop = prct*riding_pop,
         weight = prct*riding_weight*1000000,
         weight = ifelse(weight > 0 & weight <= 1, 1, weight),
         weight = round(weight)) %>% 
  filter(weight != 0)

hist(postStrat2$n_pop)         
hist(postStrat2$weight) ## distribution of the weights
sum(as.integer(postStrat2$weight)) ## eventual number of observations in data


## Generate weighted dataframe
weighted <- postStrat2[rep(1:nrow(postStrat2), round(postStrat2$weight)),]

## Predict for each party

## CAQ ---------------------------------------------------------------------
weighted$ircCAQ_pred <- predict_weighted_irc(modelCAQ, postStrat2, sign_treshold = 0.1,
                                             weight_column = "weight",
                                             model_dv_vector = PolData$ircCAQ,
                                             sd_divider = 2)
hist(weighted$ircCAQ_pred)


## PLQ ---------------------------------------------------------------------
weighted$ircPLQ_pred <- predict_weighted_irc(modelPLQ, postStrat2, sign_treshold = 0.1,
                                             weight_column = "weight",
                                             model_dv_vector = PolData$ircPLQ,
                                             sd_divider = 2)

## QS ----------------------------------------------------------------------
weighted$ircQS_pred <- predict_weighted_irc(modelQS, postStrat2, sign_treshold = 0.1,
                                             weight_column = "weight",
                                             model_dv_vector = PolData$ircQS,
                                             sd_divider = 2)

## PQ ----------------------------------------------------------------------
weighted$ircPQ_pred <- predict_weighted_irc(modelPQ, postStrat2, sign_treshold = 0.1,
                                             weight_column = "weight",
                                             model_dv_vector = PolData$ircPQ,
                                             sd_divider = 2)

## PCQ ---------------------------------------------------------------------
weighted$ircPCQ_pred <- predict_weighted_irc(modelPCQ, postStrat2, sign_treshold = 0.1,
                                             weight_column = "weight",
                                             model_dv_vector = PolData$ircPCQ,
                                             sd_divider = 2)


saveRDS(weighted, "_SharedFolder_riding-volatility/Data/warehouse/final_mrp.rds")
