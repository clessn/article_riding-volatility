# Packages ----------------------------------------------------------------
library(tidyverse) ## for data wrangling and ggplot
library(envalysis) ## for the `theme_publish()` function (in the graph)
library(ggtext) ## for the element_markdown() function (in the graph)

# Data -------------------------------------------------------------------
Data <- readRDS("data/table3_aggregatedData.rds")
View(Data)
names(Data)
str(Data)



# Preparation -------------------------------------------------------------

## For visualisation purposes, transform the `n_riding` variable, which will
## be our `alpha` parameter
Data$alpha <- log(scale(Data$n_riding, center = F)[,1]+0.000000001)

## Create a loess model for the alpha scale breaks
loess_alpha <- loess(alpha ~ n_riding,
                     data = Data)

# Graph -------------------------------------------------------------------

### Generate pearson correlation between two variables 
cor <- cor.test(Data$fragility_index, Data$volatility)$estimate

ggplot(Data, aes(x = fragility_index, y = volatility)) +
  xlab("<br>Fragility<br>(before campaign)") +
  ylab("<br>Campaign volatility<br>") +
  geom_vline(xintercept = 0.5, linewidth = 0.3) +
  geom_hline(yintercept = 0.5, linewidth = 0.3) +
  geom_text(x = 0, y = 1, label = "Q1", size = 15, color = "grey") +
  geom_text(x = 1, y = 1, label = "Q2", size = 15, color = "grey") +
  geom_text(x = 0, y = 0, label = "Q3", size = 15, color = "grey") +
  geom_text(x = 1, y = 0, label = "Q4", size = 15, color = "grey") +
  geom_smooth(method = "lm", se = F,
              linewidth = 0.5, linetype = "dashed",
              color = "black") +
  geom_jitter(size = 4, shape = 21, color = "black",
              fill = "#565656",
              ## control for alpha for number of respondents in riding 
              aes(alpha = alpha)) +
  scale_alpha_continuous(
    name = "Number of respondents",
    range = c(0, 1),
    labels = c(0, 50, 100, 150, 200),
    breaks = round(predict(
      object = loess_alpha,
      newdata = data.frame(n_riding = c(5, 50, 100, 150, 200))
    ))
  ) +
  scale_y_continuous(limits = c(-0.12, 1.12),
                     breaks = c(0, 1), 
                     labels = c("Not volatile", "Volatile")) +
  scale_x_continuous(limits = c(-0.12, 1.12),
                     breaks = c(0, 1), 
                     labels = c("Not fragile", "Fragile")) +
  #geom_text(x = 1.0875, y = 0.43, label = paste0("r = ", round(cor, 2)),
  #          color = "#454545", size = 4.5) +
  theme_publish() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.title.x = element_markdown(size = 15, hjust = 0.5, lineheight = 1.6),
        axis.title.y = element_markdown(size = 15, hjust = 0.5),
        axis.text = element_text(size = 12),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))

ggsave("graphs/figure4_fragilityVSvolatility.png",
       width = 7, height = 7)

