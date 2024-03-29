# Packages ----------------------------------------------------------------
library(tidyverse) ## for data wrangling and ggplot
library(envalysis) ## for the `theme_publish()` function (in the graph)
library(ggtext) ## for the element_markdown() function (in the graph)

# Data -------------------------------------------------------------------
Data <- readRDS("data/table3_aggregatedData.rds")

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

## Slide1: without colors --------------------------------------------------
ggplot(Data, aes(x = fragility_index, y = volatility)) +
  #geom_rect(xmin = 0.5, xmax = 1.5, ymin = 0.5, ymax = 1.5,
  #          fill = "#36827F", alpha = 0.1) +
  #geom_rect(xmin = 0.5, xmax = 1.5, ymin = -0.5, ymax = 0.5,
  #          fill = "#464D77", alpha = 0.1) +
  #geom_rect(xmin = -0.5, xmax = 0.5, ymin = -0.5, ymax = 0.5,
  #          fill = "#F9DB6D", alpha = 0.1) +
  #geom_rect(xmin = -0.5, xmax = 0.5, ymin = 0.5, ymax = 1.5,
  #          fill = "#FC7753", alpha = 0.1) +
  xlab("<br>Fragility index<br>(before campaign)") +
  ylab("Campaign volatility<br>(during campaign)<br>") +
  geom_vline(xintercept = 0.5, linewidth = 0.3) +
  geom_hline(yintercept = 0.5, linewidth = 0.3) +
  geom_smooth(method = "lm", se = F,
              linewidth = 0.5, linetype = "dashed",
              color = "black") +
  geom_jitter(size = 4, shape = 21, color = "black",
              fill = "grey", aes(alpha = alpha)) +
  geom_text(x = 1.07, y = 0.43, label = paste0("r = ", round(cor, 2)),
            color = "#454545", size = 5) +
  scale_alpha_continuous(
    name = "Number of respondents",
    range = c(0, 1),
    labels = c(0, 50, 100, 150, 200),
    breaks = round(predict(
      object = loess_alpha,
      newdata = data.frame(n_riding = c(5, 50, 100, 150, 200))
    ))
  ) +
  scale_x_continuous(limits = c(-0.1, 1.1),
                     breaks = c(0.25, 0.75),
                     labels = c("Not fragile", "Fragile")) +
  scale_y_continuous(limits = c(-0.1, 1.1),
                     breaks = c(0.15, 0.85),
                     labels = c("Not volatile", "Volatile")) +
  envalysis::theme_publish() +
  theme(text = element_text(lineheight = 0.35),
        axis.ticks = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.title.x = ggtext::element_markdown(size = 20, hjust = 0.5, lineheight = 1.1),
        axis.title.y = ggtext::element_markdown(size = 20, hjust = 0.5, lineheight = 1.1),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15, angle = 90, hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))


ggsave("graphs/beamer/results_slide1.png",
       width = 8, height = 5)


## Slide2: first square --------------------------------------------------
ggplot(Data, aes(x = fragility_index, y = volatility)) +
  geom_rect(xmin = 0.5, xmax = 1.5, ymin = 0.5, ymax = 1.5,
            fill = "#36827F", alpha = 0.1) +
  #geom_rect(xmin = 0.5, xmax = 1.5, ymin = -0.5, ymax = 0.5,
  #          fill = "#464D77", alpha = 0.1) +
  #geom_rect(xmin = -0.5, xmax = 0.5, ymin = -0.5, ymax = 0.5,
  #          fill = "#F9DB6D", alpha = 0.1) +
  #geom_rect(xmin = -0.5, xmax = 0.5, ymin = 0.5, ymax = 1.5,
  #          fill = "#FC7753", alpha = 0.1) +
  xlab("<br>Fragility index<br>(before campaign)") +
  ylab("Campaign volatility<br>(during campaign)<br>") +
  geom_vline(xintercept = 0.5, linewidth = 0.3) +
  geom_hline(yintercept = 0.5, linewidth = 0.3) +
  geom_smooth(method = "lm", se = F,
              linewidth = 0.5, linetype = "dashed",
              color = "black") +
  geom_jitter(size = 4, shape = 21, color = "black",
              fill = "grey", aes(alpha = alpha)) +
  geom_text(x = 1.07, y = 0.43, label = paste0("r = ", round(cor, 2)),
            color = "#454545", size = 5) +
  scale_alpha_continuous(
    name = "Number of respondents",
    range = c(0, 1),
    labels = c(0, 50, 100, 150, 200),
    breaks = round(predict(
      object = loess_alpha,
      newdata = data.frame(n_riding = c(5, 50, 100, 150, 200))
    ))
  ) +
  scale_x_continuous(limits = c(-0.1, 1.1),
                     breaks = c(0.25, 0.75),
                     labels = c("Not fragile", "Fragile")) +
  scale_y_continuous(limits = c(-0.1, 1.1),
                     breaks = c(0.15, 0.85),
                     labels = c("Not volatile", "Volatile")) +
  envalysis::theme_publish() +
  theme(text = element_text(lineheight = 0.35),
        axis.ticks = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.title.x = ggtext::element_markdown(size = 20, hjust = 0.5, lineheight = 1.1),
        axis.title.y = ggtext::element_markdown(size = 20, hjust = 0.5, lineheight = 1.1),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15, angle = 90, hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

ggsave("graphs/beamer/results_slide2.png",
       width = 8, height = 5)

## Slide3: second square --------------------------------------------------
ggplot(Data, aes(x = fragility_index, y = volatility)) +
  geom_rect(xmin = 0.5, xmax = 1.5, ymin = 0.5, ymax = 1.5,
            fill = "#36827F", alpha = 0.1) +
  #geom_rect(xmin = 0.5, xmax = 1.5, ymin = -0.5, ymax = 0.5,
  #          fill = "#F9DB6D", alpha = 0.1) +
  geom_rect(xmin = -0.5, xmax = 0.5, ymin = -0.5, ymax = 0.5,
            fill = "#464D77", alpha = 0.1) +
  #geom_rect(xmin = -0.5, xmax = 0.5, ymin = 0.5, ymax = 1.5,
  #          fill = "#FC7753", alpha = 0.1) +
  xlab("<br>Fragility index<br>(before campaign)") +
  ylab("Campaign volatility<br>(during campaign)<br>") +
  geom_vline(xintercept = 0.5, linewidth = 0.3) +
  geom_hline(yintercept = 0.5, linewidth = 0.3) +
  geom_smooth(method = "lm", se = F,
              linewidth = 0.5, linetype = "dashed",
              color = "black") +
  geom_jitter(size = 4, shape = 21, color = "black",
              fill = "grey", aes(alpha = alpha)) +
  geom_text(x = 1.07, y = 0.43, label = paste0("r = ", round(cor, 2)),
            color = "#454545", size = 5) +
  scale_alpha_continuous(
    name = "Number of respondents",
    range = c(0, 1),
    labels = c(0, 50, 100, 150, 200),
    breaks = round(predict(
      object = loess_alpha,
      newdata = data.frame(n_riding = c(5, 50, 100, 150, 200))
    ))
  ) +
  scale_x_continuous(limits = c(-0.1, 1.1),
                     breaks = c(0.25, 0.75),
                     labels = c("Not fragile", "Fragile")) +
  scale_y_continuous(limits = c(-0.1, 1.1),
                     breaks = c(0.15, 0.85),
                     labels = c("Not volatile", "Volatile")) +
  envalysis::theme_publish() +
  theme(text = element_text(lineheight = 0.35),
        axis.ticks = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.title.x = ggtext::element_markdown(size = 20, hjust = 0.5, lineheight = 1.1),
        axis.title.y = ggtext::element_markdown(size = 20, hjust = 0.5, lineheight = 1.1),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15, angle = 90, hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))


ggsave("graphs/beamer/results_slide3.png",
       width = 8, height = 5)


## Slide4: third square --------------------------------------------------
ggplot(Data, aes(x = fragility_index, y = volatility)) +
  geom_rect(xmin = 0.5, xmax = 1.5, ymin = 0.5, ymax = 1.5,
            fill = "#36827F", alpha = 0.1) +
  geom_rect(xmin = 0.5, xmax = 1.5, ymin = -0.5, ymax = 0.5,
            fill = "#F9DB6D", alpha = 0.1) +
  geom_rect(xmin = -0.5, xmax = 0.5, ymin = -0.5, ymax = 0.5,
            fill = "#464D77", alpha = 0.1) +
  #geom_rect(xmin = -0.5, xmax = 0.5, ymin = 0.5, ymax = 1.5,
  #          fill = "#FC7753", alpha = 0.1) +
  xlab("<br>Fragility index<br>(before campaign)") +
  ylab("Campaign volatility<br>(during campaign)<br>") +
  geom_vline(xintercept = 0.5, linewidth = 0.3) +
  geom_hline(yintercept = 0.5, linewidth = 0.3) +
  geom_smooth(method = "lm", se = F,
              linewidth = 0.5, linetype = "dashed",
              color = "black") +
  geom_jitter(size = 4, shape = 21, color = "black",
              fill = "grey", aes(alpha = alpha)) +
  geom_text(x = 1.07, y = 0.43, label = paste0("r = ", round(cor, 2)),
            color = "#454545", size = 5) +
  scale_alpha_continuous(
    name = "Number of respondents",
    range = c(0, 1),
    labels = c(0, 50, 100, 150, 200),
    breaks = round(predict(
      object = loess_alpha,
      newdata = data.frame(n_riding = c(5, 50, 100, 150, 200))
    ))
  ) +
  scale_x_continuous(limits = c(-0.1, 1.1),
                     breaks = c(0.25, 0.75),
                     labels = c("Not fragile", "Fragile")) +
  scale_y_continuous(limits = c(-0.1, 1.1),
                     breaks = c(0.15, 0.85),
                     labels = c("Not volatile", "Volatile")) +
  envalysis::theme_publish() +
  theme(text = element_text(lineheight = 0.35),
        axis.ticks = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.title.x = ggtext::element_markdown(size = 20, hjust = 0.5, lineheight = 1.1),
        axis.title.y = ggtext::element_markdown(size = 20, hjust = 0.5, lineheight = 1.1),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15, angle = 90, hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))


ggsave("graphs/beamer/results_slide4.png",
       width = 8, height = 5)


## Slide5: fourth square --------------------------------------------------
ggplot(Data, aes(x = fragility_index, y = volatility)) +
  geom_rect(xmin = 0.5, xmax = 1.5, ymin = 0.5, ymax = 1.5,
            fill = "#36827F", alpha = 0.1) +
  geom_rect(xmin = 0.5, xmax = 1.5, ymin = -0.5, ymax = 0.5,
            fill = "#F9DB6D", alpha = 0.1) +
  geom_rect(xmin = -0.5, xmax = 0.5, ymin = -0.5, ymax = 0.5,
            fill = "#464D77", alpha = 0.1) +
  geom_rect(xmin = -0.5, xmax = 0.5, ymin = 0.5, ymax = 1.5,
            fill = "#FC7753", alpha = 0.1) +
  xlab("<br>Fragility index<br>(before campaign)") +
  ylab("Campaign volatility<br>(during campaign)<br>") +
  geom_vline(xintercept = 0.5, linewidth = 0.3) +
  geom_hline(yintercept = 0.5, linewidth = 0.3) +
  geom_smooth(method = "lm", se = F,
              linewidth = 0.5, linetype = "dashed",
              color = "black") +
  geom_jitter(size = 4, shape = 21, color = "black",
              fill = "grey", aes(alpha = alpha)) +
  geom_text(x = 1.07, y = 0.43, label = paste0("r = ", round(cor, 2)),
            color = "#454545", size = 5) +
  scale_alpha_continuous(
    name = "Number of respondents",
    range = c(0, 1),
    labels = c(0, 50, 100, 150, 200),
    breaks = round(predict(
      object = loess_alpha,
      newdata = data.frame(n_riding = c(5, 50, 100, 150, 200))
    ))
  ) +
  scale_x_continuous(limits = c(-0.1, 1.1),
                     breaks = c(0.25, 0.75),
                     labels = c("Not fragile", "Fragile")) +
  scale_y_continuous(limits = c(-0.1, 1.1),
                     breaks = c(0.15, 0.85),
                     labels = c("Not volatile", "Volatile")) +
  envalysis::theme_publish() +
  theme(text = element_text(lineheight = 0.35),
        axis.ticks = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.title.x = ggtext::element_markdown(size = 20, hjust = 0.5, lineheight = 1.1),
        axis.title.y = ggtext::element_markdown(size = 20, hjust = 0.5, lineheight = 1.1),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15, angle = 90, hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))


ggsave("graphs/beamer/results_slide5.png",
       width = 8, height = 5)
