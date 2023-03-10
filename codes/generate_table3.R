# Packages ----------------------------------------------------------------
library(tidyverse) ## for data wrangling via dplyr
source("functions.R")

# Data --------------------------------------------------------------------

## Loading data from the pre-campaign surveys
Fragility <- readRDS("data/table1_respondentsRCI.rds")

## Loading data from during the campaign (via Qc125.com)
Volatility <- readRDS("data/table2_duringCampaign.rds")


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


## 2. Generate fragility indicators by riding ------------------------------

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
    # Add both indicators together and using the custom function `minmaxNormalization()`
    # to put it between 0 and 1
    fragility_index = minmaxNormalization(prop_below3_scaled + mean_rci_scaled)
  )

# Justification for scaling the indicators with the `scale()` function
hist(FragByRiding$prop_below3)
hist(FragByRiding$prop_below3_scaled)

hist(FragByRiding$mean_rci_rev)
hist(FragByRiding$mean_rci_scaled)

hist(FragByRiding$fragility_index)

