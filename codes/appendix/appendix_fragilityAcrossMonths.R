# Packages ----------------------------------------------------------------
library(tidyverse)
library(envalysis) ## for theme

# Data -------------------------------------------------------------------
Data <- readRDS("data/table1_respondentsRCI.rds")

## Data wrangling ---------------------------------

GraphData <- Data %>%
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
  ## Grouping by month and by party
  group_by(month, party) %>%
  summarise(mean_rci = mean(rci)) %>%
  # Ordering the parties for the order they appear as
  # facets in the graph
  mutate(party = factor(party,
                        levels = c("CAQ",
                                   "PLQ",
                                   "QS",
                                   "PQ",
                                   "PCQ"))) 

# Order the months for the `x` parameter in the graph
survey_recency <- c("january" = 1, "february" = 2,
                    "march" = 3, "april" = 4, "may" = 5,
                    "june" = 6, "july" = 7, "august" = 8) 

# Add it as a variable in GraphData
GraphData$survey_recency <- survey_recency[GraphData$month]

# Associate the months as they are written in the data to
# their clean names for the graph
clean_months <- c("january" = "January", "february" = "February",
                  "march" = "March", "april" = "April",
                  "may" = "May", "june" = "June",
                  "july" = "July", "august" = "August")

# Add it as a variable in GraphData
GraphData$clean_month <- clean_months[GraphData$month]
# Order it for the graph
GraphData$clean_month <- reorder(GraphData$clean_month, GraphData$survey_recency)

# Multiply the rci by 10 for visualisation purposes
GraphData$mean_rci <- GraphData$mean_rci*10


# party linetypes
party_linetypes <- c("CAQ" = "dotdash","PLQ" = "dotted","PQ" = "dashed",
                     "QS" = "solid","PCQ"="longdash")

# Add loess prediction as a variable for the party labels
GraphData$pred_loess <- NA

lCAQ <- loess(mean_rci ~ survey_recency,
              data = GraphData %>% filter(party == "CAQ"))
GraphData$pred_loess[GraphData$party=="CAQ"] <- predict(lCAQ, GraphData[GraphData$party=="CAQ",])

lPLQ <- loess(mean_rci ~ survey_recency,
              data = GraphData %>% filter(party == "PLQ"))
GraphData$pred_loess[GraphData$party=="PLQ"] <- predict(lPLQ, GraphData[GraphData$party=="PLQ",])

lQS <- loess(mean_rci ~ survey_recency,
              data = GraphData %>% filter(party == "QS"))
GraphData$pred_loess[GraphData$party=="QS"] <- predict(lQS, GraphData[GraphData$party=="QS",])

lPQ <- loess(mean_rci ~ survey_recency,
              data = GraphData %>% filter(party == "PQ"))
GraphData$pred_loess[GraphData$party=="PQ"] <- predict(lPQ, GraphData[GraphData$party=="PQ",])

lPCQ <- loess(mean_rci ~ survey_recency,
              data = GraphData %>% filter(party == "PCQ"))
GraphData$pred_loess[GraphData$party=="PCQ"] <- predict(lPCQ, GraphData[GraphData$party=="PCQ",])

# add party labels at the end of the graph
labels <- GraphData %>% 
  filter(month == "august")

ggplot(GraphData, aes(x = clean_month, y = mean_rci)) +
  geom_line(aes(group = party),
            linewidth = 2.1,
            alpha = 0.15) +
  geom_smooth(aes(group = party, linetype = party),
              color = "black",
              se = F, show.legend = F,
              linewidth = 1.25) +
  geom_text(data = labels,
            aes(label = party,
                y = pred_loess),
            hjust = -0.25) +
  scale_linetype_manual(values = party_linetypes,
                        "") +
  scale_y_continuous(breaks = c(3, 5),
                     labels = c("Less solid", "Very solid")) +
  ylab("<br>Solidity of vote<br>(mean RCI of voters)<br>") +
  xlab("") +
  theme_publish() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.title.y = ggtext::element_markdown(lineheight = 1.25),
        axis.text.x = element_text(vjust = 0.65, size = 12),
        axis.ticks = element_blank(),
        #legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(1.25, 'cm'),
        legend.key.width = unit(2.25, 'cm'),
        legend.position = "right")

ggsave("graphs/appendix_fragilityAcrossMonths.png",
       width = 8, height = 5)  
  


