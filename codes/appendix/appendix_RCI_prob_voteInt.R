# Packages ----------------------------------------------------------------
library(tidyverse)

# Data --------------------------------------------------------------------
rci <- readRDS("data/table1_respondentsRCI.rds")

Data <- readRDS("mrp/data/real_survey_data.rds") %>% 
  ## selecting SES variables
  select(respondent_id, region, male, ageC, lang, educ, income) %>% 
  left_join(., rci, by = "respondent_id")

post_strat <- readRDS("mrp/data/post_strat_table.rds")


# Data wrangling ----------------------------------------------------------
ModelData <- Data %>% 
  pivot_longer(., cols = starts_with("rci"),
               names_to = "party_rci",
               values_to = "vote_solidity",
               names_prefix = "rci_") %>% 
  pivot_longer(., cols = starts_with("voteInt"),
               names_to = "party_voteInt",
               values_to = "vote_intent",
               names_prefix = "voteInt_") %>% 
  filter(party_rci == party_voteInt)


# Models -------------------------------------------------------------------
model_all <- glm(vote_intent ~
               factor(region)*income +
               ageC*educ +
               vote_solidity,
             data = ModelData,
             family = binomial())
summary(model_all)

model_caq <- ModelData %>%
  filter(party_voteInt == "CAQ") %>%
  glm(
    vote_intent ~
      factor(region) * income +
      ageC * educ +
      vote_solidity,
    data = .,
    family = binomial()
  )
summary(model_caq)

model_plq <- ModelData %>%
  filter(party_voteInt == "PLQ") %>%
  glm(
    vote_intent ~
      factor(region) * income +
      ageC * educ +
      vote_solidity,
    data = .,
    family = binomial()
  )
summary(model_plq)

model_qs <- ModelData %>%
  filter(party_voteInt == "QS") %>%
  glm(
    vote_intent ~
      factor(region) * income +
      ageC * educ +
      vote_solidity,
    data = .,
    family = binomial()
  )
summary(model_qs)

model_pq <- ModelData %>%
  filter(party_voteInt == "PQ") %>%
  glm(
    vote_intent ~
      factor(region) * income +
      ageC * educ +
      vote_solidity,
    data = .,
    family = binomial()
  )
summary(model_pq)

model_pcq <- ModelData %>%
  filter(party_voteInt == "PCQ") %>%
  glm(
    vote_intent ~
      factor(region) * income +
      ageC * educ +
      vote_solidity,
    data = .,
    family = binomial()
  )
summary(model_pcq)

# Creating container with post_strat --------------------------------------

post_strat2 <- post_strat[rep(1:nrow(post_strat), times = length(table(ModelData$vote_solidity))),]
post_strat2$vote_solidity <- rep(seq(from = -1, to = 1, by = 0.1), each = nrow(post_strat))

# Predicting --------------------------------------------------------------

post_strat2$prob_vote_all <- predict(model_all, newdata = post_strat2,
                                 type = "response")

hist(post_strat2$prob_vote_all)

post_strat2$prob_vote_CAQ <- predict(model_caq, newdata = post_strat2,
                                     type = "response")

post_strat2$prob_vote_PLQ <- predict(model_plq, newdata = post_strat2,
                                     type = "response")

post_strat2$prob_vote_QS <- predict(model_qs, newdata = post_strat2,
                                     type = "response")

post_strat2$prob_vote_PQ <- predict(model_pq, newdata = post_strat2,
                                     type = "response")

post_strat2$prob_vote_PCQ <- predict(model_pcq, newdata = post_strat2,
                                     type = "response")


# Graph data wrangling ----------------------------------------------------

Graph <- post_strat2 %>%
  pivot_longer(
    .,
    cols = starts_with("prob_vote"),
    names_to = "party",
    values_to = "prob_vote",
    names_prefix = "prob_vote_"
  ) %>%
  mutate(alpha = ifelse(party == "all", 1, 0))


# Graph -------------------------------------------------------------------

breaks_x <- c(-0.8, -0.5, -0.4, -0.3, -0.2, 0.2, 0.3, 0.4, 0.5, 0.8)
gam <- mgcv::gam(prob_vote_all ~ s(vote_solidity),
                      data = post_strat2,
                 weights = riding_prop)

# party colors
party_colors <- c(all = "black", "CAQ" = "#00FFFF","PLQ" = "#FF0024","PQ" = "#099FFF",
                  "QS" = "#FF6600","PCQ"="purple")


segments <- data.frame(
  vote_solidity = breaks_x) %>% 
  mutate(
  gam = predict(gam, newdata = .)*100,
  vote_solidity = vote_solidity*10
)

ggplot(Graph, aes(x = vote_solidity*10, y = prob_vote*100)) +
  geom_line(stat = "smooth",
            method = "gam",
            aes(color = party,
                group = party,
                alpha = alpha,
                linewidth = alpha),
            show.legend = F) +
  geom_vline(xintercept = 0, linewidth = 0.4) +
  geom_segment(data = segments,
               aes(x = vote_solidity,
                   xend = vote_solidity,
                   yend = gam),
               y = 0,
               color = "black",
               linetype = "dashed",
               alpha = 1,
               linewidth = 0.75) +
  geom_text(data = segments %>% filter(!(vote_solidity%in%c(-8,8,-5,-4))),
            aes(x = vote_solidity-0.35,
                y = gam + 2,
                label = paste0(round(gam), "%")),
            size = 3) +
  ylab("Probability of\nvote intent (%)") +
  xlab("RCI of leading party") +
  scale_x_continuous(breaks = breaks_x*10) +
  scale_y_continuous(expand = c(0,0),
                     breaks = c(1,25,50,75,99),
                     labels = c(0,25,50,75,100)) +
  scale_alpha_continuous(range = c(0.35,1)) +
  scale_color_manual(values = party_colors) +
  scale_linewidth_continuous(range = c(1,1.5)) +
  envalysis::theme_publish() +
  theme(axis.ticks = element_blank(),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))

ggsave("graphs/appendix_prob_voteInt.png",
       width = 8, height = 4.5)



#### For beamer
ggplot(Graph, aes(x = vote_solidity*10, y = prob_vote*100)) +
  geom_line(stat = "smooth",
            method = "gam",
            aes(color = party,
                group = party,
                alpha = alpha,
                linewidth = alpha),
            show.legend = F) +
  geom_vline(xintercept = 0, linewidth = 0.4) +
  geom_segment(data = segments,
               aes(x = vote_solidity,
                   xend = vote_solidity,
                   yend = gam),
               y = 0,
               color = "black",
               linetype = "dashed",
               alpha = 1,
               linewidth = 0.75) +
  geom_text(data = segments %>% filter(!(vote_solidity%in%c(-8,8,-5,-4, 5))),
            aes(x = vote_solidity-0.75,
                y = gam + 4,
                label = paste0(round(gam), "%")),
            size = 6) +
  ylab("Probability of\nvote intent (%)") +
  xlab("RCI of leading party") +
  scale_x_continuous(breaks = breaks_x*10) +
  scale_y_continuous(expand = c(0,0),
                     breaks = c(1,25,50,75,99),
                     labels = c(0,25,50,75,100)) +
  scale_alpha_continuous(range = c(0.35,1)) +
  scale_color_manual(values = party_colors) +
  scale_linewidth_continuous(range = c(1,1.5)) +
  envalysis::theme_publish() +
  theme(axis.ticks = element_blank(),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15))

ggsave("graphs/beamer/prob_voteInt.png",
       width = 8, height = 4.5)
