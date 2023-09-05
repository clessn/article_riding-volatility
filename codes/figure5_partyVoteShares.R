# Packages ----------------------------------------------------------------
library(tidyverse) ## for data wrangling and ggplot
library(envalysis) ## for the `theme_publish()` function (in the graph)
library(ggtext) ## for the element_markdown() function (in the graph)

# Data -------------------------------------------------------------------
Volatility <- readRDS("data/table3_aggregatedData.rds") %>% 
  select(riding_id, volatility, fragility_index)

Data <- readRDS("mrp/data/census_data.rds") %>% 
  select(riding_id, starts_with("vote2018")) %>% 
  left_join(Volatility, ., by = "riding_id") %>% 
  ## remove PCQ since vote shares in 2018 are too small
  select(-vote2018_PCQ)


# Preparation -------------------------------------------------------------

## Pivot dataframe on the long format
Graph <- Data %>% 
  pivot_longer(.,
               cols = starts_with("vote2018"),
               names_to = "party",
               values_to = "voteshare2018",
               names_prefix = "vote2018_")

# Graph -------------------------------------------------------------------


## X = party vote share ----------------------------------------------------

ggplot(Graph, aes(x = voteshare2018, y = volatility)) +
  facet_wrap(~party) +
  geom_jitter(aes(alpha = fragility_index),
              size = 3.75) +
  geom_smooth()

ggplot(Graph, aes(x = voteshare2018, y = volatility)) +
  facet_wrap(~party) +
  geom_jitter(aes(alpha = fragility_index),
              size = 3.75) +
  geom_smooth() +
  scale_x_continuous(limits = c(0.2, 0.75))


## X = fragility ----------------------------------------------------

ggplot(Graph, aes(x = voteshare2018, y = fragility_index)) +
  facet_wrap(~party) +
  geom_jitter(size = 3.75) +
  geom_smooth()
