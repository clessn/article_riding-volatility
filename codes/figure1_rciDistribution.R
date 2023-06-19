# Packages ----------------------------------------------------------------
library(tidyverse) ## for data wrangling and ggplot
library(ggridges) ## for embedding density ridges
library(envalysis) ## for the `theme_publish()` function
library(ggtext) ## for the element_markdown() function (in the graph)

# Data -------------------------------------------------------------------
Data <- readRDS("data/table1_respondentsRCI.rds")
View(Data)
names(Data)
str(Data)

# Wrangling ---------------------------------------------------------------

GraphData <- Data %>%
  # select relevant variables
  select(respondent_id, month, starts_with("rci")) %>%
  rename(rci_QLP = rci_PLQ) %>% 
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
  # Ordering the parties for the order they appear as
  # facets in the graph
  mutate(party = factor(party,
                        levels = c("CAQ",
                                   "QLP",
                                   "QS",
                                   "PQ",
                                   "PCQ"))) 

# Order the months for the `alpha` parameter in the graph
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
GraphData$clean_month <- reorder(GraphData$clean_month, -GraphData$survey_recency)

# Multiply the rci by 10 for visualisation purposes
GraphData$rci <- GraphData$rci*10



# Graph -------------------------------------------------------------------
ggplot(GraphData, aes(x = rci, y = factor(clean_month))) +
  geom_density_ridges(
    # A larger bandwidth gives out a smoother density
    bandwidth = 0.75,
    # A larger scale means that the ridges will be more
    # embedded together. A value of 1 means that the highest
    # ridge will lightly touch the next ridge.
    scale = 4.5,
    color = "#494949",
    # Alpha parameter varies according to the recency of the survey
    aes(alpha = survey_recency),
    show.legend = F,
    # Show the median line in the densities
    quantile_lines = T,
    quantiles = c(0.5)
  ) +
  # Create one facet by party
  facet_wrap(~party) +
  scale_alpha_continuous(range = c(0, 0.4)) +
  scale_x_continuous(breaks = c(-8, 8), labels = c("Not Fragile", "Fragile")) +
  ylab("") +
  xlab("RCI") +
  geom_vline(xintercept = 0) +
  theme_publish(base_size = 15) +
  theme(
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    strip.text.x = element_text(face = "plain"),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_markdown(lineheight = 0, size = 15),
    axis.line.y = element_blank(),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )

ggsave("graphs/figure1_rciDistribution.png",
       width = 12, height = 9)

