library(tidyverse)
d <- readRDS("data/table1_respondentsRCI.rds")

d2 <- d %>% 
  select(respondent_id,
         month,
         riding_name,
         riding_id,
         rci_CAQ = rci_caq,
         rci_PLQ = rci_plq,
         rci_QS = rci_qs,
         rci_PQ = rci_pq,
         rci_PCQ = rci_pcq)
saveRDS(d2, "data/table1_respondentsRCI.rds")

d <- readRDS("data/duringCampaign.rds")

d2 <- d %>% 
  select(date, riding_name, riding_id, party, pred, moes)

saveRDS(d2, "data/table2_duringCampaign.rds")

