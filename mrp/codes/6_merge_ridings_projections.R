# Packages ----------------------------------------------------------------
library(tidyverse)

# Data --------------------------------------------------------------------
Data <- readRDS("mrp/data/real_survey_data.rds")

Projections <- readRDS("data/table2_duringCampaign.rds") %>% 
  filter(date == min(date) & ## Only keep the first date
           party != "PVQ") ## remove PVQ (green party)

## The first date we have is 2022-08-22, 6 days before the campaign.


# Wrangling ---------------------------------------------------------------

Final <- Projections %>% 
  ## Selecting relevant columns
  select(riding_id, party, pred) %>% 
  ## Pivoting the df on the wide format
  pivot_wider(., names_from = party,
              values_from = pred,
              names_prefix = "proj_") %>% 
  ## Joining with Data
  left_join(Data, ., by = "riding_id")

## Save it
saveRDS(Final, "mrp/data/real_survey_data_with_riding_projections.rds")



