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
ggplot(Data, aes(x = fragility_index)) +
  geom_histogram(alpha = 1,
                 binwidth = 0.025,
                 fill = "#B4B4B4") +
  theme_publish(base_size = 20) +
  theme(
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(linewidth = 0.25),
    axis.title.x = element_text(lineheight = 0),
    axis.title.y = element_text(lineheight = 0),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  ) +
  scale_x_continuous(
    breaks = c(0.15, 0.85),
    labels = c("No volatility predicted", "Volatility predicted")
  ) +
  scale_y_continuous(breaks = seq(from = 0, to = 15, by = 5)) +
  xlab("Volatility index (before campaign)") +
  ylab("Number of ridings")

ggsave("graphs/figure2_fragilityIndexDistribution.png",
       width = 7, height = 4.5)
