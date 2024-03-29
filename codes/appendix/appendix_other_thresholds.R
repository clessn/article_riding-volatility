# Packages ----------------------------------------------------------------
library(tidyverse) ## for data wrangling and ggplot
library(envalysis) ## for the `theme_publish()` function (in the graph)
library(ggtext) ## for the element_markdown() function (in the graph)

# Data -------------------------------------------------------------------
## The volatility variable is gonna be the same across the graphs
Data <- readRDS("data/table3_aggregatedData.rds") %>% 
  select(-fragility_index, -fragility_index_mrp, -n_riding)

## Loading data from the pre-campaign surveys
Fragility <- readRDS("data/table1_respondentsRCI.rds")


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


# Loop -----------------------------------------------------------------

## For each threshold (1, 2, 3, 4, 5), run the code to generate the fragility index 
## of the ridings

## Two indicators
##### indicator1: mean RCI of leading parties for respondents of a riding
##### indicator2: proportion of respondents of a riding with a leading-party RCI below the threshold

for (i in c(0.1, 0.2, 0.3, 0.4, 0.5)){
  FragByRiding <- ByRespondent %>%
    # Generate variable that is `1` if a respondent has a leading-party RCI below threshold
    mutate(below_threshold = ifelse(rci <= i, 1, 0)) %>%
    # Group by riding
    group_by(riding_id) %>%
    # Add a variable that contains the number of respondents by riding
    mutate(n = n()) %>%
    # Summarise data by the grouping variable (riding_id)
    summarise(
      # Get the mean of the leading-party RCIs
      mean_rci = mean(rci),
      # Get the number of respondents by riding with a leading-party RCI below threshold
      # by computing the sum of the below_threshold column
      n_below_threshold = sum(below_threshold),
      # Get the number of respondents by riding
      n_riding = unique(n)
    ) %>%
    mutate(
      # Reverse the RCI (since a lower RCI = more fragility)
      mean_rci_rev = (mean_rci * -1),
      # Compute the proportion of respondents in the riding with a RCI below 3
      prop_below_threshold = n_below_threshold / n_riding,
      # Scale the two indicators
      prop_below_threshold_scaled = scale(prop_below_threshold,
                                 center = T)[, 1],
      mean_rci_scaled = scale(mean_rci_rev,
                              center = T)[, 1],
      # Add both indicators together and use the clessnverse::normalize_min_max
      # to put it between 0 and 1
      fragility_index = clessnverse::normalize_min_max(prop_below_threshold_scaled + mean_rci_scaled),
      # add threshold column for the graph
      threshold = i*10
    )
  if (i == 0.1){
    GraphData <- FragByRiding
  } else {
    GraphData <- rbind(GraphData, FragByRiding)
  }
}

## Join volatility data (from Data) on GraphData

GraphData2 <- GraphData %>% 
  left_join(., Data, by = "riding_id") %>% 
  select(riding_id, volatility, fragility_index,
         threshold, n_riding)


# Preparation -------------------------------------------------------------

## For visualisation purposes, transform the `n_riding` variable, which will
## be our `alpha` parameter
GraphData2$alpha <- log(scale(GraphData2$n_riding, center = F)[,1]+0.000000001)

## Create a loess model for the alpha scale breaks
loess_alpha <- loess(alpha ~ n_riding,
                     data = GraphData2)


# Graphs -------------------------------------------------------------------

graph1 <- GraphData2 %>%
  # remove observations with threshold of 3
  filter(threshold != 3) %>%
  # rename thresholds
  mutate(threshold = paste0("RCI threshold at ", threshold)) %>% 
  ggplot(aes(x = fragility_index, y = volatility)) +
  xlab("<br>Fragility index<br>(before campaign)") +
  ylab("<br>Campaign volatility<br>") +
  facet_wrap(~threshold) +
  geom_vline(xintercept = 0.5, linewidth = 0.3) +
  geom_hline(yintercept = 0.5, linewidth = 0.3) +
  geom_text(x = 0, y = 1, label = "Q1", size = 8, color = "lightgrey") +
  geom_text(x = 1, y = 1, label = "Q2", size = 8, color = "lightgrey") +
  geom_text(x = 0, y = 0, label = "Q3", size = 8, color = "lightgrey") +
  geom_text(x = 1, y = 0, label = "Q4", size = 8, color = "lightgrey") +
  geom_smooth(method = "lm", se = F,
              linewidth = 0.5, linetype = "dashed",
              color = "black") +
  geom_jitter(size = 4, shape = 21, color = "black",
              fill = "#565656",
              ## control for alpha for number of respondents in riding 
              aes(alpha = alpha)) +
  scale_alpha_continuous(
    name = "Number of respondents",
    range = c(0, 0.75),
    labels = c(0, 50, 100, 150, 200),
    breaks = round(predict(
      object = loess_alpha,
      newdata = data.frame(n_riding = c(5, 50, 100, 150, 200))
    ))
  ) +
  scale_y_continuous(limits = c(-0.12, 1.12),
                     breaks = c(0, 1), 
                     labels = c("Not volatile", "Volatile")) +
  scale_x_continuous(limits = c(-0.12, 1.12),
                     breaks = c(0, 1), 
                     labels = c("Not fragile", "Fragile")) +
  #geom_text(x = 1.0875, y = 0.43, label = paste0("r = ", round(cor, 2)),
  #          color = "#454545", size = 4.5) +
  theme_publish() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.title.x = element_markdown(size = 20, hjust = 0.5, lineheight = 1.6),
        axis.title.y = element_markdown(size = 20, hjust = 0.5),
        axis.text = element_text(size = 12),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))



graph2 <- GraphData2 %>% 
  group_by(threshold) %>% 
  summarise(cor = cor.test(volatility, fragility_index)$estimate) %>% 
  ggplot(aes(x = threshold, y = cor)) +
  geom_line() +
  geom_point(size = 1) +
  theme_publish() +
  ylab("<br>Pearson correlation test between<br>fragility index and campaign volatility") +
  xlab("<br>RCI threshold<br>") +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.title.x = element_markdown(size = 20, hjust = 0.5, lineheight = 1.6),
        axis.title.y = element_markdown(size = 20, hjust = 0.5),
        axis.text = element_text(size = 12),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))

graph <- gridExtra::grid.arrange(graph1, graph2, ncol = 2)

ggsave("graphs/appendix_other_thresholds.png",
       plot = graph,
       width = 12, height = 7)

