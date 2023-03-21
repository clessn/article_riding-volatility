# Packages ----------------------------------------------------------------
library(tidyverse) ## for data wrangling and ggplot
library(ggridges) ## for embedding density ridges
library(envalysis) ## for the `theme_publish()` function
library(ggtext) ## for the element_markdown() function (in the graph)

# Data -------------------------------------------------------------------
Data <- readRDS("data/table3_aggregatedData.rds")
View(Data)
names(Data)
str(Data)


# Graph -------------------------------------------------------------------

Data %>%
  ## Put on the long format
  pivot_longer(
    .,
    ## pivot column with names starting with `fragility_index`
    cols = starts_with("fragility_index"),
    names_to = "index",
    values_to = "fragility_index"
  ) %>%
  ## Change the labels in `index` for the facets' title
  mutate(
    index = case_when(
      index == "fragility_index" ~ "Without MRP",
      index == "fragility_index_mrp" ~ "With MRP",
    ),
    ## Factor it so `without mrp` is the first panel
    index = factor(index, levels = c("Without MRP", "With MRP"))
  ) %>%
  ggplot(aes(x = fragility_index)) +
  geom_histogram(alpha = 1,
                 binwidth = 0.025,
                 fill = "#B4B4B4",
                 color = "#B4B4B4") +
  facet_wrap(~ index) +
  theme_publish(base_size = 20) +
  theme(
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(linewidth = 0.25),
    axis.title.x = element_text(lineheight = 0),
    axis.title.y = element_text(lineheight = 0),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  ) +
  scale_x_continuous(breaks = c(0.15, 0.9),
                     labels = c("Not fragile", "Fragile")) +
  scale_y_continuous(breaks = seq(from = 0, to = 20, by = 5),
                     limits = c(0, 15)) +
  xlab("Vote fragility index (before campaign)") +
  ylab("Number of ridings")

ggsave("graphs/figure2_fragilityIndexDistribution.png",
       width = 7, height = 4.5)
