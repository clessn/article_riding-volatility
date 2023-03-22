# Packages ----------------------------------------------------------------
library(tidyverse)
library(kableExtra)

# Data --------------------------------------------------------------------
Data <- readRDS("data/table3_aggregatedData.rds")



# Table1: most fragile ridings -------------------------------------------

table1 <- Data %>% 
  top_n(x = ., n = 15, wt = fragility_index) %>% 
  arrange(-fragility_index) %>% 
  mutate(fragility = round(fragility_index, 2)) %>% 
  select(riding_name, fragility)

table1 %>% 
  kableExtra::kbl(format = "latex",
                  col.names = c("Riding", "Vote fragility"),
                  align = "r",
                  caption = "Most fragile ridings before the campaign")



# Table2: most volatile ridings -------------------------------------------

table2 <- Data %>% 
  top_n(x = ., n = 15, wt = volatility) %>% 
  arrange(-volatility) %>% 
  mutate(volatility = round(volatility, 2)) %>% 
  select(riding_name, volatility)

table2 %>% 
  kableExtra::kbl(format = "latex",
                  col.names = c("Riding", "Campaign volatility"),
                  align = "r",
                  caption = "Most volatile ridings during the campaign")
