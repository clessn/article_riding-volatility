# Packages ----------------------------------------------------------------
library(tidyverse)

# Data --------------------------------------------------------------------
Data <- readRDS("data/table3_aggregatedData.rds")



# Graph 1: scatter plot between fragility index without and with mrp --------

graph1 <- ggplot(Data, aes(x = fragility_index_mrp, y = fragility_index)) +
  geom_jitter(color = "#36454F", alpha = 0.8) +
  geom_smooth(color = "black") +
  scale_x_continuous(breaks = c(0.05, 0.95), 
                     limits = c(-0.01, 1.01), 
                     labels = c("Not fragile", "Fragile")) +
  scale_y_continuous(breaks = c(0.05, 0.95), 
                     limits = c(-0.01, 1.01), 
                     labels = c("Not fragile", "Fragile")) +
  envalysis::theme_publish() +
  xlab("Fragility with MRP") +
  ylab("Fragility\nwithout MRP") +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.ticks = element_blank())


graph2 <- ggplot(Data, aes(x = fragility_index_mrp, y = volatility)) +
  geom_jitter(color = "#36454F", alpha = 0.8) +
  geom_smooth(color = "black") +
  scale_x_continuous(breaks = c(0.05, 0.95), 
                     limits = c(-0.01, 1.01), 
                     labels = c("Not fragile", "Fragile")) +
  scale_y_continuous(breaks = c(0.05, 0.95), 
                     limits = c(-0.01, 1.01), 
                     labels = c("Not volatile", "Volatile")) +
  envalysis::theme_publish() +
  xlab("Fragility with MRP") +
  ylab("Campaign volatility") +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.ticks = element_blank())

graph <- gridExtra::grid.arrange(graph1, graph2, ncol = 2)

ggsave(filename = "graphs/appendix_mrp.png",
       plot = graph,
       width = 7, height = 4.5)



