# Packages ----------------------------------------------------------------
library(tidyverse)

# Data --------------------------------------------------------------------

## Survey data
Data <- readRDS("mrp/data/real_survey_data.rds") %>%
  ungroup() %>%
  #### Only select independent variables
  select(ageC, educ, income)

## Census data
Census <- readRDS("mrp/data/census_data.rds") %>% 
  ## remove province line
  filter(riding_name != "Province")

## Projections data
Projections <- readRDS("data/table2_duringCampaign.rds") %>% 
  filter(date == min(date) & ## Only keep the first date
           party != "PVQ") ## remove PVQ (green party)


# Generate synthetic strat table ------------------------------------------

for (i in 1:nrow(Census)) {
  options(dplyr.summarise.inform = FALSE)
  options(dplyr.left_join.inform = FALSE)
  riding_idi <- Census$riding_id[i]
  prop_age15p <- 1 - Census$age0m14[i]
  censusPropsAge <- c(Census$age15m29[i], Census$age30m44[i],
                      Census$age45m59[i], Census$age60m74[i],
                      Census$age75p[i])/prop_age15p
  censusPropsEduc <- c(Census$educHSB[i],
                       Census$educColl[i],
                       Census$educUniv[i])
  censusPropsIncome <- c("incomeLow" = Census$income40m[i],
                         "incomeMid" = Census$income40m99[i],
                         "incomeHigh" = Census$income100p[i])
  
  FirstStrat <- Data %>%
    select(ageC, educ) %>%
    na.omit() %>%
    group_by(ageC, educ) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    mutate(prct = n / sum(n)) %>% 
    group_by(educ) %>% 
    mutate(prct = sum(prct))
  FirstStrat$adjustCoef <- censusPropsEduc/FirstStrat$prct
  FirstStrat$newFreq <- round(FirstStrat$n*FirstStrat$adjustCoef)
  FirstStrat <- FirstStrat %>% 
    ungroup() %>% 
    select(ageC,educ,newFreq) %>%
    rename(n=newFreq) %>%
    mutate(prct=n / sum(n))
  
  LastStage <- FirstStrat
  
  Strat <- Data %>%
    select(ageC, educ, income) %>%
    na.omit() %>%
    group_by(ageC, educ, income) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    mutate(prct = n / sum(n))
  vars <- c("ageC", "educ", "income")
  args <- paste0("unique(Strat$", vars, ")", collapse = ", ")
  
  AllCombs <-
    eval(parse(text = paste0("expand.grid(", args, ")")))
  names(AllCombs) <- vars
  
  Strat <- left_join(AllCombs, Strat) %>%
    replace(is.na(.), 0) %>%
    group_by(educ, income) %>%
    mutate(prct2 = sum(prct))

    
  Strat$adjustCoef <- censusPropsIncome[Strat$income]/Strat$prct2
  Strat$adjustCoef <-
    ifelse(Strat$adjustCoef %in% c(-Inf, Inf), 0, Strat$adjustCoef)
  Strat$newFreq <- Strat$n * Strat$adjustCoef
  
  Strat <- Strat %>%
    select(all_of(vars), newFreq) %>%
    rename(n = newFreq) %>%
    group_by(ageC, educ) %>%
    mutate(prct = (n / sum(n)))
  
  LastStagej <- LastStage %>%
    select(-n) %>%
    rename(prct_ls = prct)
  
  Strat2 <- left_join(Strat, LastStagej) %>%
    mutate(prct = prct * prct_ls)
  
  LastStage <- Strat2 %>%
    select(-prct_ls)
  
  if (i == 1) {
    StratTable <- LastStage %>%
      mutate(riding_id = riding_idi)
  }
  else {
    TempStrat <- LastStage %>%
      mutate(riding_id = riding_idi)
    StratTable <- rbind(StratTable, TempStrat)
  }
  print(paste0(round(i / nrow(Census) * 100), "% - ", riding_idi))
}

### Check 
StratTable$n[is.nan(StratTable$n)] <- 0
StratTable$prct[is.nan(StratTable$prct)] <- 0

# Test
StratTable %>% 
  group_by(riding_id) %>% 
  summarise(sum = sum(prct)) %>% 
  arrange(-sum)
#### Every riding has a sum of 1? Good!!


# Add riding_name and region ----------------------------------------------

regions <- Census$region
names(regions) <- Census$riding_id
StratTable$region <- regions[as.character(StratTable$riding_id)]

riding_names <- Census$riding_name
names(riding_names) <- Census$riding_id
StratTable$riding_name <- riding_names[as.character(StratTable$riding_id)]


# Merge riding projections at start of campaign ---------------------------
Final <- Projections %>% 
  ## Selecting relevant columns
  select(riding_id, party, pred) %>% 
  ## Pivoting the df on the wide format
  pivot_wider(., names_from = party,
              values_from = pred,
              names_prefix = "proj_") %>% 
  ## Joining with Data
  left_join(StratTable, ., by = "riding_id") %>% 
  ## order the columns
  select(riding_id, riding_name, region, ageC, educ, income, n, riding_prop = prct,
         starts_with("proj"))


## Save it
saveRDS(Final, "mrp/data/post_strat_table.rds")

