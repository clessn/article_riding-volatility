library(tidyverse)

## Census data ####
Census <- readxl::read_excel("_SharedFolder_riding-volatility/Data/lake/census_excel.xlsx") %>% 
  select(riding_id, riding_name = riding, region, total_pop, male, female, age0m14, age15m29, age30m44, age45m59, age60m74, age75p, age18p,
         english, french, otherLang, income40m, income40m99, income100p, educHSB, educColl, educUniv)

saveRDS(Census, "_SharedFolder_riding-volatility/census_for_clean_repo.rds")

## Pol data ####
PolData <- readRDS("_SharedFolder_riding-volatility/Data/warehouse/PolData_for_mrp.rds") %>% 
  select(
    # column identifying each respondent
    respondent_id = id,
    # Columns for the ridings
    riding_id,
    riding_name = riding,
    # SES columns
    male, ageC, lang, educ, income,
    # VD: irc columns
    ircCAQ, ircPLQ, ircQS, ircPQ, ircPCQ
  )

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

regions <- Census$region
names(regions) <- Census$riding_id
PolData$region <- regions[as.character(PolData$riding_id)]

table(PolData$region)

Final <- PolData %>% 
  pivot_longer(starts_with("irc")) %>% 
  group_by(respondent_id) %>% 
  filter(value == max(value)) %>% 
  select(
    respondent_id,
    riding_id, riding_name, region,
    male, ageC, lang, educ, income,
    vote_fragility = value
  )

saveRDS(Final, "_SharedFolder_riding-volatility/survey_for_clean_repo.rds")
