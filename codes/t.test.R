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


# t.test ------------------------------------------------------------------
t.test(Data$fragility_index, Data$volatility)

