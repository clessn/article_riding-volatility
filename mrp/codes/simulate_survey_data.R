# Packages ----------------------------------------------------------------
library(tidyverse)


# Data --------------------------------------------------------------------

## Partial data without SES variables
Data <- readRDS("data/table1_respondentsRCI.rds") %>% 
  ## Generate vote_fragility for each respondent
  pivot_longer(.,
               cols = starts_with("rci"),
               names_to = "party",
               values_to = "vote_fragility") %>% 
  group_by(respondent_id) %>% 
  filter(vote_fragility == max(vote_fragility)) %>% 
  sample_n(size = 1) %>% 
  drop_na(riding_id) %>% 
  select(respondent_id,
         month,
         riding_name,
         riding_id,
         vote_fragility)


## Census data
Census <- readRDS("mrp/data/census_data.rds") %>% 
  ## remove province line
  filter(riding_name != "Province")

#### Add region to Data
regions <- Census$region
names(regions) <- Census$riding_id

Data$region <- factor(regions[as.character(Data$riding_id)])


# Custom functions --------------------------------------------------------

## For each SES variable, create a function that gives a random value fitting the census data of the riding_id

## set seed
set.seed(1234)

## male --------------------------------------------------------------------
get_male <- function(riding_id){
  male_weights <- Census$male
  names(male_weights) <- Census$riding_id
  male_prob <- male_weights[as.character(riding_id)]
  female_prob <- 1-male_prob
  gender <- sample(x = c(1, 0),
                   size = 1,
                   replace = T,
                   prob = c(male_prob, female_prob))
  return(gender)
}

get_male(110) ## saint-francois
get_male(754) ## chauveau

## sapply by passing all values of Data$riding_id in the function
Data$male <- sapply(Data$riding_id, get_male)


## ageC ------------------------------------------------------------------
get_ageC <- function(riding_id){
  weights <- as.vector(t(Census[Census$riding_id==riding_id,c("age15m29", "age30m44", "age45m59", "age60m74", "age75p")]))
  ageC <- sample(x = c("age15m29", "age30m44", "age45m59", "age60m74", "age75p"),
                 size = 1,
                 replace = T,
                 prob = weights)
  return(ageC)
}

get_ageC(332) ## westmount

Data$ageC <- factor(sapply(Data$riding_id, get_ageC),
                    levels = c("age15m29", "age30m44", "age45m59",
                               "age60m74", "age75p"))


## lang ------------------------------------------------------------------
get_lang <- function(riding_id){
  weights <- as.vector(t(Census[Census$riding_id==riding_id,c("french", "english", "otherLang")]))
  weights2 <- c(weights[1], weights[2]+weights[3])
  lang <- sample(x = c("french", "english_other"),
                 size = 1,
                 replace = T,
                 prob = weights2)
  return(lang)
}

get_lang(938) ## ungava

Data$lang <- factor(sapply(Data$riding_id, get_lang))


## educ ------------------------------------------------------------------
get_educ <- function(riding_id){
  weights <- as.vector(t(Census[Census$riding_id==riding_id,c("educHSB", "educColl", "educUniv")]))
  educ <- sample(x = c("educHSB", "educColl", "educUniv"),
                 size = 1,
                 replace = T,
                 prob = weights)
  return(educ)
}

get_educ(838) ## rimouski

Data$educ <- factor(sapply(Data$riding_id, get_educ),
                    levels = c("educHSB", "educColl", "educUniv"))


## income ----------------------------------------------------------------
get_income <- function(riding_id){
  weights <- as.vector(t(Census[Census$riding_id==riding_id,c("income40m", "income40m99", "income100p")]))
  income <- sample(x = c("incomeLow", "incomeMid", "incomeHigh"),
                 size = 1,
                 replace = T,
                 prob = weights)
  return(income)
}

get_income(660) ## trois-rivieres

Data$income <- factor(sapply(Data$riding_id, get_income),
                      levels = c("incomeLow", "incomeMid", "incomeHigh"))



# Organize and save it -------------------------------------------------------------

Final <- Data %>% 
  select(respondent_id,
         riding_id,
         riding_name,
         region,
         male,
         ageC,
         lang,
         educ,
         income,
         vote_fragility)


saveRDS(Final, "mrp/data/simulated_survey_data.rds")

