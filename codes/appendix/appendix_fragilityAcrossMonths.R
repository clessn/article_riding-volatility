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


# party colors
party_colors <- c("CAQ" = "#00FFFF","PLQ" = "#FF0024","PQ" = "#099FFF",
                  "QS" = "#FF6600","PCQ"="purple")

ggplot(GraphData, aes(x = clean_month, y = mean_rci)) +
  geom_line(aes(group = party, color = party),
            linewidth = 2.1, show.legend = F,
            alpha = 0.15) +
  geom_smooth(aes(group = party, color = party),
              se = F, show.legend = F,
              linewidth = 1.8) +
  scale_color_manual(values = party_colors) +
  ylab("<br>Fragility of vote<br>(mean RCI of voters)<br>") +
  xlab("") +
  theme_publish() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.title.y = ggtext::element_markdown(lineheight = 1.25),
        axis.text.x = element_text(vjust = 0.65, size = 12))

ggsave("graphs/appendix_fragilityAcrossMonths.png",
       width = 8, height = 5)  
  


