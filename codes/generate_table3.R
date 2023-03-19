# Packages ----------------------------------------------------------------
library(tidyverse) ## for data wrangling via dplyr
devtools::install_github("clessn/clessnverse") ## if necessary
library(clessnverse) ## for the normalize_min_max() function

# Data --------------------------------------------------------------------

## Loading data from the pre-campaign surveys
Fragility <- readRDS("data/table1_respondentsRCI.rds")

## Loading data from during the campaign (via Qc125.com)
Volatility <- readRDS("data/table2_duringCampaign.rds") %>% 
  mutate(date = as.Date(date)) %>% 
  # Filter for campaign dates
  filter(date >= "2022-08-28" &
           date <= "2022-10-03" &
           # removing PVQ and independant candidates
           !(party %in% c("PVQ", "AUT")))


# Fragility index -------------------------------------------------------------------------

## 1. Keeping leading party of respondents ---------------------------------
ByRespondent <- Fragility %>% 
  # select relevant variables
  select(respondent_id, month, riding_id, starts_with("rci")) %>%
  # put them in the long format
  pivot_longer(
    .,
    # columns to put into long format
    cols = starts_with("rci"),
    # name of the new column containing the former variables names
    names_to = "party",
    # name of the new column containing the values
    values_to = "rci",
    # to remove the prefix rci_ from the variables names
    names_prefix = "rci_"
  ) %>% 
  # grouping by respondent to only keep the maximum RCI by respondent
  group_by(respondent_id) %>% 
  # filtering to only keep the maximum of the RCI column 
  filter(rci == max(rci)) %>% 
  # randomly keep one party by respondent when two or more parties are tied.
  ## The relevant information we need here is the RCI value, the party is not
  ## that important
  sample_n(size = 1) %>% 
  # Drop respondents with missing riding information
  drop_na(riding_id)


## 2. Generate fragility index by riding ------------------------------

## Two indicators
##### indicator1: mean RCI of leading parties for respondents of a riding
##### indicator2: proportion of respondents of a riding with a leading-party RCI below the arbitrary threshold of 3

FragByRiding <- ByRespondent %>%
  # Generate variable that is `1` if a respondent has a leading-party RCI below 3
  mutate(below3 = ifelse(rci <= 0.3, 1, 0)) %>%
  # Group by riding
  group_by(riding_id) %>%
  # Add a variable that contains the number of respondents by riding
  mutate(n = n()) %>%
  # Summarise data by the grouping variable (riding_id)
  summarise(
    # Get the mean of the leading-party RCIs
    mean_rci = mean(rci),
    # Get the number of respondents by riding with a leading-party RCI below 3
    # by computing the sum of the below3 column
    n_below3 = sum(below3),
    # Get the number of respondents by riding
    n_riding = unique(n)
  ) %>%
  mutate(
    # Reverse the RCI (since a lower RCI = more fragility)
    mean_rci_rev = (mean_rci * -1),
    # Compute the proportion of respondents in the riding with a RCI below 3
    prop_below3 = n_below3 / n_riding,
    # Scale the two indicators
    prop_below3_scaled = scale(prop_below3,
                               center = T)[, 1],
    mean_rci_scaled = scale(mean_rci_rev,
                            center = T)[, 1],
    # Add both indicators together and use the clessnverse::normalize_min_max
    # to put it between 0 and 1
    fragility_index = clessnverse::normalize_min_max(prop_below3_scaled + mean_rci_scaled)
  )

# Justification for scaling the indicators with the `scale()` function
hist(FragByRiding$prop_below3)
hist(FragByRiding$prop_below3_scaled)

hist(FragByRiding$mean_rci_rev)
hist(FragByRiding$mean_rci_scaled)

hist(FragByRiding$fragility_index)


# Campaign volatility -----------------------------------------

## We decided to measure riding volatility as the variance of the daily vote share
## predictions of each party
VolByRiding <- Volatility %>%
  mutate(pred = pred * 100) %>%
  ## group by riding and party
  group_by(riding_id, party) %>%
  ## generate variance of the pred column for each group
  summarise(var_pred = var(pred)) %>%
  ## add all parties' results together for each riding
  group_by(riding_id) %>%
  summarise(volatility = sum(var_pred)) %>%
  ## data transformations
  mutate(
    volatility_scale = scale(volatility,
                             center = T)[, 1],
    volatility_log = log(volatility),
    volatility_log_scale = scale(volatility_log,
                                 center = T)[, 1],
    volatility = clessnverse::normalize_min_max(volatility_log_scale)
  )

hist(VolByRiding$volatility)


# Join fragility index and campaign volatility by riding  --------------

Data <- Volatility %>%
  ## get unique riding_name for each riding_id
  group_by(riding_id) %>%
  summarise(riding_name = unique(riding_name)) %>%
  ## join fragility index
  left_join(x = ., y = FragByRiding, by = "riding_id") %>%
  ## join campaign volatility
  left_join(x = ., y = VolByRiding, by = "riding_id") %>%
  ## select 5 crucial columns
  select(riding_id, riding_name, fragility_index, volatility, n_riding)

# Save it -----------------------------------------------------------------

saveRDS(Data, "data/table3_agregatedData.rds")
