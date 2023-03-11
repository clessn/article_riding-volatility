# Packages ----------------------------------------------------------------
library(tidyverse) ## for data wrangling and ggplot
library(ggridges) ## for embedding density ridges
library(envalysis) ## for the `theme_publish()` function
library(ggtext) ## for the element_markdown() function (in the graph)

# Data -------------------------------------------------------------------
Data <- readRDS("data/table3_agregatedData.rds")
View(Data)
names(Data)
str(Data)


# Graph -------------------------------------------------------------------
ggplot(Data, aes(x = volatility)) +
  geom_histogram(fill = "#B4B4B4",
                 alpha = 1,
                 binwidth = 0.025) +
  theme_publish(base_size = 20) +
  theme(
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(linewidth = 0.25),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  ) +
  scale_x_continuous(
    breaks = c(0.15, 0.85),
    labels = c("No volatility observed", "Volatility observed")
  ) +
  scale_y_continuous(breaks = seq(from = 0, to = 20, by = 5),
                     limits = c(0,20)) +
  xlab("Campaign volatility") +
  ylab("Number of ridings")

ggsave(
  "graphs/figure3_campaignVolatilityDistribution.png",
  width = 7,
  height = 4.5
)
