library(tidyverse)
library(ggridges)
library(rddtools)

# party colors
party_colors <- c("CAQ" = "#00FFFF","PLQ" = "#FF0024","PQ" = "#099FFF",
                  "QS" = "#FF6600","PCQ"="purple")

Data <- readRDS("_SharedFolder_riding-volatility/Data/warehouse/preCampaign.rds")

Raw <- readRDS("_SharedFolder_riding-volatility/Data/warehouse/preCampaign.rds") %>% 
  filter(!(is.na(party_id_caquiste))) %>% 
  mutate(party_id_cons = ifelse(is.na(party_id_cons),
                                party_id_conservateur,
                                party_id_cons),
         partyId_CAQ = party_id_caquiste,
         partyId_PLQ = party_id_lib,
         partyId_PQ = party_id_pequiste,
         partyId_QS = party_id_solidaire,
         partyId_PCQ = party_id_cons,
         partyId_PVQ = party_id_vert,
         partyId_other = party_id_another,
         partyId_none = party_id_none,
         partyId_dontKnow = party_id_DK)
  
table(Raw$source_id)
table(Raw$partyId_PCQ, Data$source_id)

names(Raw %>% select(starts_with("partyId")))

Data <- Raw %>% 
  select(id, starts_with("irc"), starts_with("partyId")) %>% 
  pivot_longer(., cols = starts_with("partyId"),
               names_to = "partyId",
               values_to = "partisan",
               names_prefix = "partyId_") %>% 
  pivot_longer(., cols = starts_with("irc"),
               names_to = "partyIrc",
               values_to = "irc",
               names_prefix = "irc") %>% 
  filter(partisan == 1)


# irc of partisans
Data %>% 
  filter(partyId == partyIrc) %>% 
  ggplot(aes(x = irc, y = partyIrc)) +
  geom_density_ridges(bandwidth = 0.075,
                      scale = 0.975,
                      aes(fill =  partyIrc,
                          color = partyIrc),
                      alpha = 0.15,
                      show.legend = F,
                      quantile_lines = T,
                      quantiles = c(0.25, 0.5, 0.9)) +
  ggtitle("IRC des répondants qui s'identifient au parti") +
  geom_text(aes(y = partyIrc, label = paste0("n = ", ..count..)),
            stat = "count", x = -1, vjust = -1.5) +
  scale_fill_manual(values = party_colors) +
  scale_color_manual(values = party_colors) +
  clessnverse::theme_clean_light() +
  #ggthemes::theme_fivethirtyeight() +
  geom_vline(xintercept = 0) +
  ylab("")


ggsave("_SharedFolder_riding-volatility/graphs/irc_partyId/irc_des_partisans.png",
       width = 8, height = 6)

# irc of notPartisan
Data %>% 
  mutate(partyId = ifelse(partyId %in% c("dontKnow",
                                         "none"),
                          "notPartisan", partyId)) %>% 
  filter(partyId == "notPartisan") %>% 
  ggplot(aes(x = irc, y = partyIrc)) +
  geom_density_ridges(quantile_line = T,
                      bandwidth = 0.075,
                      scale = 0.99,
                      aes(fill =  partyIrc,
                          color = partyIrc),
                      alpha = 0.15,
                      show.legend = F,
                      quantile_lines = T,
                      quantiles = c(0.25, 0.5, 0.9)) +
  ggtitle("IRC des répondants des partisans des AUTRES partis") +
  scale_fill_manual(values = party_colors) +
  scale_color_manual(values = party_colors) +
  clessnverse::theme_clean_light() +
  geom_vline(xintercept = 0) +
  ylab("")

ggsave("_SharedFolder_riding-volatility/graphs/irc_partyId/irc_autres_partisans.png",
       width = 8, height = 6)



# Prep variables ----------------------------------------------------------
sum(is.na(Raw$age34m))
sum(is.na(Raw$age3554))
table(Raw$age34m)
table(Raw$age3554)
table(Raw$age55p)

sum(is.na(Raw$male))

sum(is.na(Raw$langFr))

sum(is.na(Raw$educBHS))
table(Raw$educBHS)
table(Raw$educCollege)
table(Raw$educUniv)

sum(is.na(Raw$incomeLow))
sum(is.na(Raw$incomeMid))

Raw$age <- NA
Raw$age[Raw$age34m == 1] <- 0
Raw$age[Raw$age3554 == 1] <- 0.5
Raw$age[Raw$age55p == 1] <- 1
table(Raw$age)

Raw$educ <- NA
Raw$educ[Raw$educBHS == 1] <- 0
Raw$educ[Raw$educCollege == 1] <- 0.5
Raw$educ[Raw$educUniv == 1] <- 1
table(Raw$educ)

Raw$income <- 1
Raw$income[Raw$incomeLow == 1] <- 0
Raw$income[Raw$incomeMid == 1] <- 0.5

# Models -------------------------------------------------

Clean <- Raw %>% 
  select(starts_with("partyId"),
         starts_with("irc"),
         age, educ, income, male, langFr) %>% 
  pivot_longer(starts_with("partyId"),
               names_to = "id_party",
               names_prefix = "partyId_",
               values_to = "id_value") %>% 
  filter(!(id_party %in% c("PVQ", "other", "none",
                         "dontKnow"))) %>%
  pivot_longer(starts_with("irc"),
               names_to = "party",
               names_prefix = "irc",
               values_to = "irc_value") %>% 
  filter(party == id_party) %>% 
  select(-id_party) %>% 
  mutate(irc_value = as.numeric(round(irc_value, 1)),
         irc_value = (irc_value+1)/2)
hist(Clean$irc_value)

## CAQ
modelCAQ <- Clean %>% 
  filter(party == "CAQ") %>%
  select(-party) %>% 
  glm(id_value ~ .,
      family = binomial(),
      data = .)
summary(modelCAQ)


## PLQ
modelPLQ <- Clean %>% 
  filter(party == "PLQ") %>%
  select(-party) %>% 
  glm(id_value ~ .,
      family = binomial(),
      data = .)
summary(modelPLQ)


## QS 
modelQS <- Clean %>% 
  filter(party == "QS") %>%
  select(-party) %>% 
  glm(id_value ~ .,
      family = binomial(),
      data = .)
summary(modelQS)


## PQ
modelPQ <- Clean %>% 
  filter(party == "PQ") %>%
  select(-party) %>% 
  glm(id_value ~ .,
      family = binomial(),
      data = .)
summary(modelPQ)

## PCQ
modelPCQ <- Clean %>% 
  filter(party == "PCQ") %>%
  select(-party) %>% 
  glm(id_value ~ .,
      family = binomial(),
      data = .)
summary(modelPCQ)


# Predicted probabilities -------------------------------------------------
length(unique(Clean$irc_value))
sort(unique(Clean$irc_value))
unique(Clean$irc_value)
## 21 levels if we exclude the NAs

rci_levels <- seq(from = 0, to = 1, by = 0.05)
n_levels <- length(rci_levels)
n_profiles <- 6

df <- data.frame(
  irc_value = rep(rci_levels, n_profiles),
  age = c(rep(0, n_levels),
          rep(1, n_levels),
          rep(0.5, n_levels),
          rep(1, n_levels),
          rep(0, n_levels),
          rep(0.5, n_levels)),
  income = c(rep(0, n_levels),
             rep(1, n_levels),
             rep(0.25, n_levels),
             rep(0.25, n_levels),
             rep(0, n_levels),
             rep(0.25, n_levels)),
  male = c(rep(0.5, n_levels),
           rep(0.5, n_levels),
           rep(1, n_levels),
           rep(0.5, n_levels),
           rep(0.5, n_levels),
           rep(0, n_levels)),
  langFr = c(rep(0, n_levels),
             rep(1, n_levels),
             rep(1, n_levels),
             rep(1, n_levels),
             rep(1, n_levels),
             rep(1, n_levels)),
  educ = c(rep(0, n_levels),
           rep(1, n_levels),
           rep(0.3, n_levels),
           rep(mean(Raw$educ, na.rm = T), n_levels),
           rep(0.75, n_levels),
           rep(0.3, n_levels)),
  profile = c(rep("34m, anglo,\nrevenu bas, BHS", n_levels),
              rep("55p, franco,\nrevenu élevé, univ", n_levels),
              rep("Homme 35-54, franco,\nrevenu bas-moyen, BHS-college", n_levels),
              rep("55p, franco, revenu bas-moyen", n_levels),
              rep("34m, franco,\nrevenu bas, college-univ", n_levels),
              rep("Femme 35-54, franco,\nrevenu bas-moyen, BHS-college", n_levels))
  ) %>% 
  mutate(rci_graph = irc_value*2-1)


df$predCAQ <- predict(modelCAQ, newdata = df, type = "response")
df$predPLQ <- predict(modelPLQ, newdata = df, type = "response")
df$predQS <- predict(modelQS, newdata = df, type = "response")
df$predPQ <- predict(modelPQ, newdata = df, type = "response")
df$predPCQ <- predict(modelPCQ, newdata = df, type = "response")

df %>% 
  pivot_longer(cols = starts_with("pred"),
               names_prefix = "pred",
               names_to = "party",
               values_to = "prob") %>% 
ggplot(aes(x = rci_graph*10, y = prob*100,
           color = party)) +
  facet_wrap(~profile) +
  geom_vline(xintercept = 0) +
  geom_point(shape = 21, fill = "grey",
             alpha = 0.2,
             size = 2,
             show.legend = F) +
  geom_line(linewidth = 1, show.legend = F) +
  scale_color_manual(values = party_colors) +
  #scale_x_continuous(limits = c(-6,6)) +
  clessnverse::theme_clean_light() +
  xlab("<br>RCI<br>") +
  ylab("<br>Probabilité prédite:<br>identification partisane<br>") +
  theme(axis.title.x = ggtext::element_markdown(hjust = 0.5),
        axis.title.y = ggtext::element_markdown(hjust = 0.5),
        #axis.title.x = element_text(hjust = 0.5),
        #axis.title.y = element_text(hjust = 0.5),
        panel.grid.major.y = element_line(color = "#DDDDDD"))

ggsave("_SharedFolder_riding-volatility/graphs/irc_partyId/prob_predites.png",
       width = 10, height = 7)


# Do it again for vote intent ---------------------------------------------

Clean2 <- Raw %>% 
  select(starts_with("op_intent"),
         starts_with("irc"),
         age, educ, income, male, langFr, -op_intent) %>% 
  pivot_longer(starts_with("op_intent"),
               names_to = "intent_party",
               names_prefix = "op_intent_",
               values_to = "vote_intent") %>% 
  filter(!(intent_party %in% c("Other", "wontVote",
                           "dontKnow"))) %>%
  pivot_longer(starts_with("irc"),
               names_to = "party",
               names_prefix = "irc",
               values_to = "irc_value") %>% 
  filter(party == intent_party) %>% 
  select(-intent_party) %>% 
  mutate(irc_value = as.numeric(round(irc_value, 1)),
         irc_value = (irc_value+1)/2)

## CAQ
modelCAQ2 <- Clean2 %>% 
  filter(party == "CAQ") %>%
  select(-party) %>% 
  glm(vote_intent ~ .,
      family = binomial(),
      data = .)
summary(modelCAQ2)


## PLQ
modelPLQ2 <- Clean2 %>% 
  filter(party == "PLQ") %>%
  select(-party) %>% 
  glm(vote_intent ~ .,
      family = binomial(),
      data = .)
summary(modelPLQ2)


## QS 
modelQS2 <- Clean2 %>% 
  filter(party == "QS") %>%
  select(-party) %>% 
  glm(vote_intent ~ .,
      family = binomial(),
      data = .)
summary(modelQS2)


## PQ
modelPQ2 <- Clean2 %>% 
  filter(party == "PQ") %>%
  select(-party) %>% 
  glm(vote_intent ~ .,
      family = binomial(),
      data = .)
summary(modelPQ2)

## PCQ
modelPCQ2 <- Clean2 %>% 
  filter(party == "PCQ") %>%
  select(-party) %>% 
  glm(vote_intent ~ .,
      family = binomial(),
      data = .)
summary(modelPCQ2)

df$pred2CAQ <- predict(modelCAQ2, newdata = df, type = "response")
df$pred2PLQ <- predict(modelPLQ2, newdata = df, type = "response")
df$pred2QS <- predict(modelQS2, newdata = df, type = "response")
df$pred2PQ <- predict(modelPQ2, newdata = df, type = "response")
df$pred2PCQ <- predict(modelPCQ2, newdata = df, type = "response")
test <- predict(modelPCQ2, newdata = df, type = "response", se.fit = T)

df %>% 
  pivot_longer(cols = starts_with("pred2"),
               names_prefix = "pred2",
               names_to = "party",
               values_to = "prob") %>% 
  ggplot(aes(x = rci_graph*10, y = prob*100,
             color = party)) +
  facet_wrap(~profile) +
  geom_vline(xintercept = 0) +
  geom_point(shape = 21, fill = "grey",
             alpha = 0.2,
             size = 2,
             show.legend = F) +
  geom_line(linewidth = 1, show.legend = F) +
  scale_color_manual(values = party_colors) +
  #scale_x_continuous(limits = c(-6,6)) +
  clessnverse::theme_clean_light() +
  xlab("<br>RCI<br>") +
  ylab("<br>Probabilité prédite:<br>intention de vote<br>") +
  theme(axis.title.x = ggtext::element_markdown(hjust = 0.5),
        axis.title.y = ggtext::element_markdown(hjust = 0.5),
        #axis.title.x = element_text(hjust = 0.5),
        #axis.title.y = element_text(hjust = 0.5),
        panel.grid.major.y = element_line(color = "#DDDDDD"))

ggsave("_SharedFolder_riding-volatility/graphs/irc_partyId/prob_predites_vote_intent.png",
       width = 10, height = 7)


# Regressions to find threshold ------------------------------------------

modelId <- df %>% 
  select(-profile, -rci_graph,
         -ends_with("2"), -starts_with("pred2")) %>% 
  pivot_longer(cols = starts_with("pred"),
               values_to = "prob") %>%
  select(-name) %>%
  lm(prob ~ .,
      #family = binomial(),
      data = .)
summary(modelId)


# Predicted probs on all combinations -------------------------------------

vars <- c("age", "income", "educ", "male", "langFr")

args <- paste0("unique(Raw$", vars, ")", collapse = ", ")

AllProfiles <- eval(parse(text = paste0(paste0("expand.grid(", args), ", rci_levels)"))) %>% 
  drop_na()

names(AllProfiles) <- c(vars, "irc_value")

AllProfiles$pred_CAQ_Id <- predict(modelCAQ, newdata = AllProfiles, type = "response")
AllProfiles$pred_PLQ_Id <- predict(modelPLQ, newdata = AllProfiles, type = "response")
AllProfiles$pred_QS_Id <- predict(modelQS,   newdata = AllProfiles, type = "response")
AllProfiles$pred_PQ_Id <- predict(modelPQ,   newdata = AllProfiles, type = "response")
AllProfiles$pred_PCQ_Id <- predict(modelPCQ, newdata = AllProfiles, type = "response")

AllProfiles$pred_CAQ_voteInt <- predict(modelCAQ2, newdata = AllProfiles, type = "response")
AllProfiles$pred_PLQ_voteInt <- predict(modelPLQ2, newdata = AllProfiles, type = "response")
AllProfiles$pred_QS_voteInt <- predict(modelQS2,   newdata = AllProfiles, type = "response")
AllProfiles$pred_PQ_voteInt <- predict(modelPQ2,   newdata = AllProfiles, type = "response")
AllProfiles$pred_PCQ_voteInt <- predict(modelPCQ2, newdata = AllProfiles, type = "response")

facet_names <- c("Id" = "Identification partisane",
                 "voteInt" = "Intention de vote")

Graphs <- AllProfiles %>%
  pivot_longer(starts_with("pred"),
               names_to = "vd_type",
               values_to = "prob") %>% 
  mutate(vd_type = gsub("pred_", "", vd_type),
         party = gsub("_Id", "", vd_type),
         party = gsub("_voteInt", "", party),
         vd_type = gsub(names(party_colors)[1], "", vd_type),
         vd_type = gsub(names(party_colors)[2], "", vd_type),
         vd_type = gsub(names(party_colors)[3], "", vd_type),
         vd_type = gsub(names(party_colors)[4], "", vd_type),
         vd_type = gsub(names(party_colors)[5], "", vd_type),
         vd_type = gsub("_", "", vd_type),
         rci_graph = irc_value*2-1,
         facet = facet_names[vd_type])

gam_id <- Graphs %>%
  filter(vd_type == "Id") %>% 
  mgcv::gam(prob ~ s(rci_graph), data = .)

gam_vint <- Graphs %>%
  filter(vd_type == "voteInt") %>% 
  mgcv::gam(prob ~ s(rci_graph), data = .)

Graph <- Graphs %>% 
  #group_by(vd_type) %>% 
  mutate(gam_Id = predict(gam_id, newdata = .),
         gam_voteInt = predict(gam_vint, newdata = .)) %>% 
  pivot_longer(starts_with("gam"),
               names_prefix = "gam_") %>% 
  filter(vd_type == name)

breaks_x <- c(-0.8, -0.5, -0.4, -0.3, -0.2, 0.2, 0.3, 0.4, 0.5, 0.8)
Segments <- Graph %>% 
  group_by(facet, rci_graph) %>% 
  summarise(gam = mean(value)) %>%
  mutate(rci_graph = round(rci_graph, 1)) %>% 
  filter(rci_graph %in% breaks_x)

ggplot(Graph, aes(x = rci_graph*10, y = prob*100)) +
  facet_wrap(~facet) +
  geom_vline(xintercept = 0, linewidth = 1) +
  geom_jitter(size = 0.25, alpha = 0.25,
              shape = 21,
              height = 0.05*100,
              width = 0.048*10,
              color = "darkgrey") +
  geom_smooth(method = "gam", level = 0.999) +
  geom_segment(data = Segments,
               aes(x = rci_graph*10,
                   xend = rci_graph*10,
                   yend = gam*100,
                   group = facet),
               y = 0,
               color = "black",
               alpha = 0.5,
               linewidth = 0.4) +
  scale_x_continuous(breaks = breaks_x*10) +
  clessnverse::theme_clean_light() +
  xlab("<br>RCI<br>") +
  ylab("<br>Probabilité prédite<br>") +
  labs(caption = "Chaque point représente la<br>probabilité prédite d'un profil SES") +
  theme(axis.title.x = ggtext::element_markdown(hjust = 0.5),
        axis.title.y = ggtext::element_markdown(hjust = 0.5),
        plot.caption = ggtext::element_markdown(lineheight = 1.5),
        #axis.title.x = element_text(hjust = 0.5),
        #axis.title.y = element_text(hjust = 0.5),
        panel.grid.major.y = element_line(color = "#DDDDDD"))

ggsave("_SharedFolder_riding-volatility/graphs/irc_partyId/prob_predites_all.png",
       width = 10, height = 5)


ggplot(Graphs, aes(x = rci_graph*10, y = prob*100)) +
  facet_grid(cols = vars(facet), rows = vars(party)) +
  geom_vline(xintercept = 0, linewidth = 1) +
  geom_jitter(size = 0.25, alpha = 0.25,
              shape = 21,
              height = 0.05*100,
              width = 0.049*10,
              color = "darkgrey") +
  geom_smooth(method = "gam", level = 0.999,
              aes(color = party),
              show.legend = F) +
  scale_color_manual(values = party_colors) +
  clessnverse::theme_clean_light() +
  xlab("<br>RCI<br>") +
  ylab("<br>Probabilité prédite<br>") +
  labs(caption = "Chaque point représente la<br>probabilité prédite d'un profil SES") +
  theme(axis.title.x = ggtext::element_markdown(hjust = 0.5),
        axis.title.y = ggtext::element_markdown(hjust = 0.5),
        plot.caption = ggtext::element_markdown(lineheight = 1.5),
        #axis.title.x = element_text(hjust = 0.5),
        #axis.title.y = element_text(hjust = 0.5),
        panel.grid.major.y = element_line(color = "#DDDDDD"))

ggsave("_SharedFolder_riding-volatility/graphs/irc_partyId/prob_predites_all_byparties.png",
       width = 10, height = 9)



# Potential for growth? ---------------------------------------------------

### Use the predicted probabilities as the probability of each respondent
### to vote for each party

Data <- Raw %>% 
  select(age, income, educ, male, langFr,
         starts_with("irc"), riding, riding_id) %>% 
  pivot_longer(cols = starts_with("irc"),
               names_to = "party",
               names_prefix = "irc",
               values_to = "irc") %>% 
  mutate(
    irc_value = (irc+1)/2) %>% 
  mutate(
    predCAQ = predict(modelCAQ2, newdata = ., type = "response"),
    predPLQ = predict(modelPLQ2, newdata = ., type = "response"),
    predQS = predict(modelQS2,   newdata = ., type = "response"),
    predPQ = predict(modelPQ2,   newdata = ., type = "response"),
    predPCQ = predict(modelPCQ2, newdata = ., type = "response")
  ) %>% 
  pivot_longer(cols = starts_with("pred"),
               names_to = "pred_party",
               names_prefix = "pred",
               values_to = "pred") %>% 
  filter(party == pred_party) %>% 
  mutate(pred = round(pred, 3))
  

ggplot(Data, aes(x = pred, y = factor(party))) +
  ggridges::geom_density_ridges(aes(fill = party, color = party),
                                alpha = 0.6) +
  scale_color_manual(values = party_colors) +
  scale_fill_manual(values = party_colors)



# Regression discontinuity ------------------------------------------------

rdd <- Graphs %>% 
  filter(vd_type=="voteInt")

ggplot(rdd, aes(x = irc_value, y = prob)) +
  geom_jitter()

lm_same_slope <- rdd %>% 
  mutate(threshold = ifelse(irc_value >= 0.5, 1, 0)) %$% 
  lm(prob ~ threshold + I(irc_value - 0.5))
summary(lm_same_slope)

rdd_data(y = rdd$prob, 
         x = rdd$irc_value, 
         cutpoint = 0.5) %>% 
  rdd_reg_lm(slope = "same") %>% 
  summary()

rdd_data(y = rdd$prob, 
         x = rdd$irc_value, 
         cutpoint = 0.5) %>% 
  rdd_reg_lm(slope = "separate") %>% 
  summary()

rdd %>%
  mutate(threshold = as.factor(ifelse(irc_value >= 0.5, 1, 0))) %>%
  ggplot(aes(x = irc_value, y = prob)) +
  geom_jitter(alpha = 0.1) +
  geom_smooth(aes(group = threshold, color = threshold),
              method = "lm",
              se = FALSE) +
  geom_vline(xintercept = 0.5, color = "red",
             size = 1, linetype = "dashed")


rdd_f <- rdd %>% filter(irc_value<=0.5)
rdd_data(y = rdd_f$prob, 
         x = rdd_f$irc_value, 
         cutpoint = 0.25) %>% 
  rdd_reg_lm(slope = "separate") %>% 
  summary()

rdd_f %>%
  mutate(threshold = as.factor(ifelse(irc_value >= 0.25, 1, 0))) %>%
  ggplot(aes(x = irc_value, y = prob)) +
  geom_jitter(alpha = 0.1) +
  geom_smooth(aes(group = threshold, color = threshold),
              method = "lm",
              se = FALSE) +
  geom_vline(xintercept = 0.25, color = "red",
             size = 1, linetype = "dashed")

rdd_f %>%
  mutate(threshold = as.factor(ifelse(irc_value >= 0.3, 1, 0))) %>%
  ggplot(aes(x = irc_value, y = prob)) +
  geom_jitter(alpha = 0.1) +
  geom_smooth(aes(group = threshold, color = threshold),
              method = "lm",
              se = FALSE) +
  geom_vline(xintercept = 0.25, color = "red",
             size = 1, linetype = "dashed")


rdd_f %>%
  mutate(threshold = as.factor(ifelse(irc_value >= 0.35, 1, 0))) %>%
  ggplot(aes(x = irc_value, y = prob)) +
  geom_jitter(alpha = 0.1) +
  geom_smooth(aes(group = threshold, color = threshold),
              method = "lm",
              se = FALSE) +
  geom_vline(xintercept = 0.25, color = "red",
             size = 1, linetype = "dashed")

rdd_f %>%
  mutate(threshold = as.factor(ifelse(irc_value >= 0.4, 1, 0))) %>%
  ggplot(aes(x = irc_value, y = prob)) +
  geom_jitter(alpha = 0.1) +
  geom_smooth(aes(group = threshold, color = threshold),
              method = "lm",
              se = FALSE) +
  geom_vline(xintercept = 0.25, color = "red",
             size = 1, linetype = "dashed")
rdd_f %>%
  mutate(threshold = as.factor(ifelse(irc_value >= 0.45, 1, 0))) %>%
  ggplot(aes(x = irc_value, y = prob)) +
  geom_jitter(alpha = 0.1) +
  geom_smooth(aes(group = threshold, color = threshold),
              method = "lm",
              se = FALSE) +
  geom_vline(xintercept = 0.25, color = "red",
             size = 1, linetype = "dashed")





model <- rdd_data(y = rdd_f$prob, 
         x = rdd_f$irc_value, 
         cutpoint = 0.25) %>% 
  rdd_reg_lm(slope = "separate")
summary(model)
model$coefficients
summary(model)$coefficients["x",4]
summary(model)$coefficients["x_right",4]

xs <- c()
xrs <- c()
pxs <- c()
pxrs <- c()
cutpoints <- seq(from=0.25,to=0.45, by = 0.05)

for (i in 1:5){
  i <- 1
  cp <- cutpoints[i]
  model <- rdd_data(y = rdd_f$prob, 
                    x = rdd_f$irc_value, 
                    cutpoint = cp) %>% 
    rdd_reg_lm(slope = "separate")
  xs[] <- model$coefficients["x"]
}


# Competition between partys --------------------------------------------------------------------

### First, use the thresholds from 
