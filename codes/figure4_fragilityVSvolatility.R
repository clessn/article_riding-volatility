# Packages ----------------------------------------------------------------
library(tidyverse) ## for data wrangling and ggplot
library(envalysis) ## for the `theme_publish()` function
library(ggtext) ## for the element_markdown() function (in the graph)

# Data -------------------------------------------------------------------
Data <- readRDS("data/table3_agregatedData.rds")
View(Data)
names(Data)
str(Data)


# Graph -------------------------------------------------------------------

### Generate pearson correlation between two variables 
cor <- cor.test(Data$fragility_index, Data$volatility)$estimate

ggplot(Data, aes(x = fragility_index, y = volatility)) +
  xlab("<br>Fragility index<br>(before campaign)") +
  ylab("<br>Campaign volatility<br>") +
  geom_vline(xintercept = 0.5, linewidth = 0.3) +
  geom_hline(yintercept = 0.5, linewidth = 0.3) +
  geom_smooth(method = "lm", se = F,
              linewidth = 0.5, linetype = "dashed",
              color = "black") +
  geom_jitter(size = 4, shape = 21, color = "black",
              fill = "grey", alpha = 0.7) +
  scale_y_continuous(limits = c(-0.12, 1.12),
                     breaks = c(0, 0.5, 1)) +
  scale_x_continuous(limits = c(-0.12, 1.12),
                     breaks = c(0, 0.5, 1)) +
  geom_text(x = 1.0875, y = 0.43, label = paste0("r = ", round(cor, 2)),
            color = "#454545", size = 4.5) +
  geom_text(x = 0, y = 1, label = "Q1", size = 25, color = "grey") +
  geom_text(x = 1, y = 1, label = "Q2", size = 25, color = "grey") +
  geom_text(x = 0, y = 0, label = "Q3", size = 25, color = "grey") +
  geom_text(x = 1, y = 0, label = "Q4", size = 25, color = "grey") +
  theme_publish() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.title.x = element_markdown(size = 20, hjust = 0.5),
        axis.title.y = element_markdown(size = 20, hjust = 0.5),
        axis.text = element_text(size = 12),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))

ggsave("graphs/figure4_fragilityVSvolatility.png",
       width = 7, height = 7)

