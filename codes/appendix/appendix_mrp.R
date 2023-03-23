# Packages ----------------------------------------------------------------
library(tidyverse)

# Data --------------------------------------------------------------------
Data <- readRDS("data/table3_aggregatedData.rds")



# Graph 1: scatter plot between fragility index without and with mrp --------

graph1 <- ggplot(Data, aes(x = fragility_index_mrp, y = fragility_index)) +
  geom_jitter(color = "#36454F", alpha = 0.8) +
  geom_smooth(color = "black") +
  envalysis::theme_publish() +
  xlab("Fragility index with MRP") +
  ylab("Fragility index\nwithout MRP") +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.ticks = element_blank())


graph2 <- ggplot(Data, aes(x = fragility_index_mrp, y = volatility)) +
  geom_jitter(color = "#36454F", alpha = 0.8) +
  geom_smooth(color = "black") +
  envalysis::theme_publish() +
  xlab("Fragility index with MRP") +
  ylab("Campaign volatility") +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.ticks = element_blank())

graph <- gridExtra::grid.arrange(graph1, graph2, ncol = 2)

ggsave(filename = "graphs/appendix_mrp.png",
       plot = graph,
       width = 7, height = 4.5)


