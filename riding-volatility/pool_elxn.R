library(tidyverse)
source("functions.R", encoding = "UTF-8")

Omnibus <- readRDS("_SharedFolder_riding-volatility/Data/lake/to_merge/omnibusMain.rds") %>% 
  select(id = clessnId, potGrowthCAQ, potGrowthPLQ, potGrowthQS, potGrowthPCQ, potGrowthPQ, riding)

Dg1 <- read.csv("_SharedFolder_riding-volatility/Data/lake/to_merge/Pilote1.csv") %>% 
  select(id, potGrowthCAQ = op_potentialG_CAQ,
             potGrowthPLQ = op_potentialG_PLQ,
             potGrowthQS = op_potentialG_QS,
             potGrowthPCQ = op_potentialG_PCQ,
             potGrowthPQ = op_potentialG_PQ, riding)

Data <- rbind(Omnibus, Dg1) %>% 
  mutate(id = 1:nrow(.))

prepped <- Data %>% 
  select(id, starts_with("potGrowth"))

Graph <- irc_df(prepped, id_col = "id") %>% 
  left_join(., Data, by = "id") %>% 
  select(-starts_with("potGrowth")) %>% 
  pivot_longer(., cols = starts_with("irc"),
               names_to = "party",
               values_to = "irc",
               names_prefix = "irc")

library(ggridges)

ridings <- c("Maurice Richard")


Graph %>% 
  filter(party %in% c("CAQ", "PLQ"),
         riding %in% ridings) %>% 
  ggplot(aes(x = irc, y = factor(party))) +
  #stat_binline(aes(fill = party, color = party),
  #               alpha = 0.6, scale=2) +
  scale_color_manual(values=fills) +
  scale_fill_manual(values=fills) +
  geom_vline(xintercept = 0, color = "grey80", size = 1.5, alpha=0.5)+
  geom_vline(xintercept = 0.5, color = "grey80", size = 1, alpha=0.5, linetype="dashed")+
  geom_vline(xintercept = -0.5, color = "grey80", size = 1, alpha=0.5, linetype="dashed")+
  geom_density_ridges(aes(fill = party, color = party),
                      size = 1.25,
                      alpha = 0.4, scale=1.5,
                      rel_min_height=0.01,
                      quantile_lines=T,
                      quantiles=4,
                      bandwidth = 0.1,
                      show.legend = F) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(-1,1))+
  scale_y_discrete(labels = NULL)+
  theme_clean_dark() +
  facet_wrap(~riding) +
  theme(strip.background = element_rect(fill = "grey80"))




