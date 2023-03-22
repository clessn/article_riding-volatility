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


# Model -------------------------------------------------------------------
model <- glm(vote_intent ~
               factor(region)*income +
               ageC*educ +
               vote_solidity,
             data = ModelData,
             family = binomial())
summary(model)



# Creating container with post_strat --------------------------------------

post_strat2 <- post_strat[rep(1:nrow(post_strat), times = length(table(ModelData$vote_solidity))),]
post_strat2$vote_solidity <- rep(seq(from = -1, to = 1, by = 0.1), each = nrow(post_strat))

# Predicting --------------------------------------------------------------

post_strat2$prob_vote <- predict(model, newdata = post_strat2,
                                 type = "response")

hist(post_strat2$prob_vote)


# Graph -------------------------------------------------------------------

breaks_x <- c(-0.8, -0.5, -0.4, -0.3, -0.2, 0.2, 0.3, 0.4, 0.5, 0.8)
gam <- mgcv::gam(prob_vote ~ s(vote_solidity),
                      data = post_strat2,
                 weights = riding_prop)

segments <- data.frame(
  vote_solidity = breaks_x) %>% 
  mutate(
  gam = predict(gam, newdata = .)*100,
  vote_solidity = vote_solidity*10
)

ggplot(post_strat2, aes(x = vote_solidity*10, y = prob_vote*100)) +
  #geom_jitter(alpha = 0.1) +
  geom_smooth(method = "gam",
              aes(weight = riding_prop),
              color = "black") +
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
  ylab("Probabibility of having\nintent to vote for party") +
  xlab("RCI of leading party") +
  scale_x_continuous(breaks = breaks_x*10) +
  scale_y_continuous(expand = c(0,0),
                     breaks = c(1,25,50,75,99),
                     labels = c(0,25,50,75,100)) +
  envalysis::theme_publish() +
  theme(axis.ticks = element_blank(),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))

ggsave("graphs/appendix_prob_voteInt.png",
       width = 8, height = 5)
