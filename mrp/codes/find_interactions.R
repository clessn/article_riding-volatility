# Packages ----------------------------------------------------------------
library(tidyverse)

# Data --------------------------------------------------------------------
Data <- readRDS("mrp/data/real_survey_data.rds")

# Custom function --------------------------------------------------------

## This function will create a dataframe with data to plot
## the interaction between two variables (x_variable and y_variable)
## and a dependent_variable
## We will run the function across all combinations of SES variables with sapply() or a loop
get_interactions_df <- function(x_variable, y_variable,
                                dependent_variable, data){
  options(dplyr.summarise.inform = F)
  data2 <- data %>% 
    group_by_at(c(x_variable, y_variable)) %>% 
    summarise(mean = mean(.data[[dependent_variable]],
                          na.rm = T)) %>% 
    drop_na()
  names(data2) <- c("value_x", "value_y", "y_axis")
  data2$value_x <- as.character(data2$value_x)
  data2$value_y <- as.character(data2$value_y)
  data2$facet_x <- x_variable
  data2$facet_y <- y_variable
  levels_x <- 1:length(unique(data2$value_x))
  names(levels_x) <- unique(data2$value_x)
  levels_y <- 1:length(unique(data2$value_y))
  names(levels_y) <- unique(data2$value_y)
  data2$factor_x <- factor(levels_x[data2$value_x])
  data2$factor_y <- factor(levels_y[data2$value_y])
  return(data2)
}

get_interactions_df(x_variable = "male",
                    y_variable = "ageC",
                    dependent_variable = "vote_solidity",
                    data = Data)



# Step 1. Interactions between 5 SES variables ------------------------------------------------------

## Iterate through all combinations -------------------------------------------------------------------------

### Create a vector containing all SES variables
ses <- c("male", "ageC", "lang", "educ", "income")

## Matrix of all combinations of SES variables
comb_matrix <- combn(ses, m = 2)

## Put these combinations into a x-vector and a y-vector.
## These vectors will be passed into the sapply()
xs <- c(comb_matrix[1,], comb_matrix[2,])
ys <- c(comb_matrix[2,], comb_matrix[1,])

## loop get_interactions_df for each combination and bind it all
for (i in 1:length(xs)){
  GraphDatai <- get_interactions_df(x_variable = xs[i],
                                    y_variable = ys[i],
                                    dependent_variable = "vote_solidity",
                                    data = Data)
  if (i == 1){
    GraphData <- GraphDatai
  } else {
    GraphData <- rbind(GraphData, GraphDatai)
  }
  print(paste0(i, "/", length(xs)))
}

## Graph it ----------------------------------------------------------------
ggplot(GraphData,
       aes(x = factor_x,
           y = y_axis,
           group = factor_y)) +
  geom_line(aes(linetype = factor_y,
                color = factor_y),
            linewidth = 0.75,
            show.legend = F) +
  facet_grid(cols = vars(facet_x),
             rows = vars(facet_y),
             scales = "free") +
  clessnverse::theme_clean_light() +
  ylab("Mean of vote_solidity") +
  xlab("")

ggsave("mrp/graphs/find_interactions_5ses.png",
       width = 9, height = 7)



# Step 2. Narrowing it down: interactions between 3 SES variables ------------------------------------------------------

### Chosen variables: ageC, educ and income (reasons outlined in article and/or readME)

## Iterate through all combinations -------------------------------------------------------------------------

### Create a vector containing SES variables
ses <- c("ageC", "educ", "income")

## Matrix of all combinations of SES variables
comb_matrix <- combn(ses, m = 2)

## Put these combinations into a x-vector and a y-vector.
## These vectors will be passed into the sapply()
xs <- c(comb_matrix[1,], comb_matrix[2,])
ys <- c(comb_matrix[2,], comb_matrix[1,])

## loop get_interactions_df for each combination and bind it all
for (i in 1:length(xs)){
  GraphDatai <- get_interactions_df(x_variable = xs[i],
                                    y_variable = ys[i],
                                    dependent_variable = "vote_solidity",
                                    data = Data)
  if (i == 1){
    GraphData <- GraphDatai
  } else {
    GraphData <- rbind(GraphData, GraphDatai)
  }
  print(paste0(i, "/", length(xs)))
}

## Graph it ----------------------------------------------------------------
ggplot(GraphData,
       aes(x = factor_x,
           y = y_axis,
           group = factor_y)) +
  geom_line(aes(linetype = factor_y,
                color = factor_y),
            linewidth = 0.75,
            show.legend = F) +
  facet_grid(cols = vars(facet_x),
             rows = vars(facet_y),
             scales = "free") +
  clessnverse::theme_clean_light() +
  ylab("Mean of vote_solidity") +
  xlab("")

ggsave("mrp/graphs/find_interactions_3ses.png",
       width = 9, height = 7)



# Step 3: Interactions between SES and regions ----------------------------

## loop get_interactions_df for each ses with region as the y_variable
for (i in 1:length(ses)){
  GraphDatai <- get_interactions_df(x_variable = ses[i],
                                    y_variable = "region",
                                    dependent_variable = "vote_solidity",
                                    data = Data)
  if (i == 1){
    GraphData <- GraphDatai
  } else {
    GraphData <- rbind(GraphData, GraphDatai)
  }
  print(paste0(i, "/", length(xs)))
}

## Graph it

GraphData %>% 
  mutate(x_axis = as.numeric(factor_x)) %>%
  ggplot(.,
       aes(x = x_axis,
           y = y_axis,
           group = factor_y)) +
  #geom_line(aes(linetype = factor_y,
  #              color = factor_y),
  #          linewidth = 0.75,
  #          show.legend = F) +
  geom_smooth(aes(group = value_y,
                  color = value_y),
              se = F, alpha = 0.3,
              show.legend = F) +
  #geom_smooth(aes(group = value_y, color = value_y)) +
  facet_grid(cols = vars(facet_x),
             rows = vars(facet_y),
             scales = "free") +
  clessnverse::theme_clean_light() +
  ylab("Mean of vote_solidity") +
  xlab("")

ggsave("mrp/graphs/find_interactions_regions.png",
       width = 9, height = 7)

