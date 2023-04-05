irc_df <- function(prepped_data, id_col = "clessnId",
                  former_prefix = "potGrowth",
                  new_prefix = "irc") {
  ncols <- length(names(prepped_data))
  StepOne <- prepped_data %>%
    pivot_longer(., all_of(2:ncols),
                 names_to = "party",
                 values_to = "potGrowth")
  Max <- StepOne %>%
    group_by(!!sym(id_col)) %>%
    summarise(first = max(potGrowth))
  FirstSecond <- left_join(StepOne, Max, by = id_col) %>%
    mutate(ismax = ifelse(potGrowth == first, 1, 0)) %>%
    group_by(!!sym(id_col)) %>%
    mutate(nismax = sum(ismax)) %>%
    filter(potGrowth != first) %>%
    group_by(!!sym(id_col)) %>%
    mutate(second = ifelse(nismax > 1, first, max(potGrowth))) %>%
    group_by(!!sym(id_col)) %>%
    summarise(first = unique(first),
              second = unique(second)) %>%
    select(id_col, first, second)
  ThData <- left_join(StepOne, FirstSecond, by = id_col) %>%
    mutate(irc = ifelse(potGrowth == first, potGrowth - second,
                        potGrowth - first)) %>%
    pivot_wider(., id_cols = !!sym(id_col), 
                names_from = party,
                values_from = irc)
  if (!(is.na(former_prefix))){
    names(ThData) <- gsub(former_prefix, new_prefix, names(ThData))
  } else {
    names(ThData) <- names(ThData)
  }
  return(ThData)
}

ggpairs_jitter_with_smooth <- function(data, mapping, method="loess", ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_jitter(alpha = 0.2) + 
    geom_smooth(method=method, ...)
  p
}

minmaxNormalization <- function(x) {
  return((x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T)))
}


# Map functions ####
#### This function plots a map of one region (or the whole province)
map <- function(data_arg, fill_variable,
                region = c("qc", "mtl", "shb",
                           "gat", "maur", "sag", "prov"),
                r_margin = 0){
  xlim1 <- region_coords["xlim1",region]
  xlim2 <- region_coords["xlim2",region]
  ylim1 <- region_coords["ylim1",region]
  ylim2 <- region_coords["ylim2",region]
  name <- region_names[region]
  plot <- ggplot(data = StLau) + 
    geom_sf(data = data_arg, size = 2.5,
            aes(fill = .data[[fill_variable]]), color = NA) +
    geom_sf(size = 0, color = NA, fill = "lightblue") +
    #geom_sf_label(data = data2, aes(label = NM_CEP),
    #              size = 2) +
    coord_sf(xlim = c(xlim1, xlim2),
             ylim = c(ylim1, ylim2),
             expand = F) +
    #mapcan::theme_mapcan() +
    clessnverse::theme_clean_light() +
    ggtitle(name) +
    scale_fill_gradient2(low = "#3A5985",
                         mid = "#D05985",
                         high = "#FA9009",
                         limits = c(0, 1),
                         midpoint = 0.5,
                         breaks = c(0, 1),
                         #limits = c(min(data_arg[[fill_variable]]),
                         #           max(data_arg[[fill_variable]])),
                         #midpoint = median(data_arg[[fill_variable]]),
                         #breaks = c(min(data_arg[[fill_variable]]),
                         #           max(data_arg[[fill_variable]])),
                         labels = c("\nStable",
                                    "\nVolatile"),
                         guide = guide_colorbar(ticks = F)) +
    theme(#text = element_text(family = "VT323"),
          plot.title = element_text(size = 90,
                                    color = "#000000",
                                    hjust = 0),
          plot.margin = margin(0,r_margin,0,0),
          plot.background = element_rect(fill = "#D3D3D3",
                                         color = NA),
          axis.text = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank())
  return(plot)
}


### This function bind 7 maps together
whole_map <- function(data_arg, fill_variable, title, subtitle){
  showtext::showtext_auto()
  prov <- map(data_arg, fill_variable, "prov")
  qc <-   map(data_arg, fill_variable, "qc", r_margin = 100)
  mtl <-  map(data_arg, fill_variable, "mtl")
  sag <-  map(data_arg, fill_variable, "sag", r_margin = 100)
  gat <-  map(data_arg, fill_variable, "gat", r_margin = 100)
  maur <- map(data_arg, fill_variable, "maur")
  shb <-  map(data_arg, fill_variable, "shb")
  layout <- "
           AABC
           AADE
           AAFG
           "
  graph <- prov+
    mtl+
    qc+
    maur+
    sag+
    shb+
    gat+
    plot_layout(design = layout,
                guides = "collect") &
    theme(#plot.margin = margin(15,20,0,0),
      legend.position = "bottom",
      legend.key.size = unit(3, "cm"),
      legend.text = element_text(size = 180,
                                 #family = "VT323",
                                 lineheight = 0.1)) &
    plot_annotation(title = paste0("\n     ", title),
                    subtitle = paste0("\n         ", subtitle, "\n"),
                    caption = "\n\n    \n") &
    plot_annotation(theme = theme(plot.title = element_text(size = 300,
                                                            #family = "VT323",
                                                            color = "#000000",
                                                            lineheight = 0.1),
                                  plot.subtitle = element_text(size = 180,
                                                               #family = "VT323",
                                                               color = "#000000",
                                                               lineheight = 0.05),
                                  plot.caption = element_text(size = 100,
                                                              #family = "VT323",
                                                              color = "#000000",
                                                              lineheight = 0.35,
                                                              hjust = 0),
                                  plot.background = element_rect(fill = "#D3D3D3")))
  return(graph)
}

map_beamer <- function(data_arg, fill_variable,
                region = c("qc", "mtl", "shb",
                           "gat", "maur", "sag", "prov"),
                r_margin = 0, hjust_arg = 0){
  xlim1 <- region_coords["xlim1",region]
  xlim2 <- region_coords["xlim2",region]
  ylim1 <- region_coords["ylim1",region]
  ylim2 <- region_coords["ylim2",region]
  name <- region_names[region]
  plot <- ggplot(data = StLau) + 
    geom_sf(data = data_arg, size = 2.5,
            aes(fill = .data[[fill_variable]]), color = NA) +
    geom_sf(size = 0, color = NA, fill = "lightblue") +
    #geom_sf_label(data = data2, aes(label = NM_CEP),
    #              size = 2) +
    coord_sf(xlim = c(xlim1, xlim2),
             ylim = c(ylim1, ylim2),
             expand = F) +
    #mapcan::theme_mapcan() +
    clessnverse::theme_clean_light() +
    ggtitle(name) +
    scale_fill_gradient2(low = "#3A5985",
                         mid = "#D05985",
                         high = "#FA9009",
                         limits = c(0, 1),
                         midpoint = 0.5,
                         breaks = c(0, 1),
                         #limits = c(min(data_arg[[fill_variable]]),
                         #           max(data_arg[[fill_variable]])),
                         #midpoint = median(data_arg[[fill_variable]]),
                         #breaks = c(min(data_arg[[fill_variable]]),
                         #           max(data_arg[[fill_variable]])),
                         labels = c("\nStable",
                                    "\nVolatile"),
                         guide = guide_colorbar(ticks = F)) +
    theme(#text = element_text(family = "VT323"),
      plot.title = element_text(size = 180,
                                color = "#000000",
                                hjust = hjust_arg),
      plot.margin = margin(5,r_margin,0,0),
      plot.background = element_rect(fill = "white",
                                     color = NA),
      axis.text = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank())
  return(plot)
}


### This function bind 7 maps together
whole_map_beamer <- function(data_arg, fill_variable, title, subtitle){
  showtext::showtext_auto()
  prov <- map_beamer(data_arg, fill_variable, "prov")
  #data_arg <- MapData
  #fill_variable <- "real_volatility"
  qc <-   map_beamer(data_arg, fill_variable, "qc", hjust_arg = 0.3)
  mtl <-  map_beamer(data_arg, fill_variable, "mtl")
  layout <- "
           AAABB
           AAABB
           AAACC
           AAACC
           "
  graph <- prov+
    mtl+
    qc+
    plot_layout(design = layout,
                guides = "collect") &
    theme(#plot.margin = margin(15,20,0,0),
      legend.position = "bottom",
      legend.key.size = unit(3, "cm"),
      legend.text = element_text(size = 210,
                                 #family = "VT323",
                                 lineheight = 0.1)) &
    #plot_annotation(title = paste0("\n     ", title),
    #                subtitle = paste0("\n         ", subtitle, "\n"),
    #                caption = "\n\n    \n") &
    plot_annotation(theme = theme(plot.title = element_text(size = 300,
                                                            #family = "VT323",
                                                            color = "#000000",
                                                            lineheight = 0.1),
                                  plot.subtitle = element_text(size = 180,
                                                               #family = "VT323",
                                                               color = "#000000",
                                                               lineheight = 0.05),
                                  plot.caption = element_text(size = 100,
                                                              #family = "VT323",
                                                              color = "#000000",
                                                              lineheight = 0.35,
                                                              hjust = 0),
                                  plot.background = element_rect(fill = "white")))
  return(graph)
}



## https://github.com/stan-dev/rstanarm/blob/master/vignettes/mrp.Rmd

simulate_mrp_data <- function(n) {
  J <- c(2, 3, 7, 3, 50) # male or not, eth, age, income level, state
  poststrat <- as.data.frame(array(NA, c(prod(J), length(J)+1))) # Columns of post-strat matrix, plus one for size
  colnames(poststrat) <- c("male", "eth", "age","income", "state",'N')
  count <- 0
  for (i1 in 1:J[1]){
    for (i2 in 1:J[2]){
      for (i3 in 1:J[3]){
        for (i4 in 1:J[4]){
          for (i5 in 1:J[5]){
            count <- count + 1
            # Fill them in so we know what category we are referring to
            poststrat[count, 1:5] <- c(i1-1, i2, i3,i4,i5) 
          }
        }
      }
    }
  }
  # Proportion in each sample in the population
  p_male <- c(0.52, 0.48)
  p_eth <- c(0.5, 0.2, 0.3)
  p_age <- c(0.2,.1,0.2,0.2, 0.10, 0.1, 0.1)
  p_income<-c(.50,.35,.15)
  p_state_tmp<-runif(50,10,20)
  p_state<-p_state_tmp/sum(p_state_tmp)
  poststrat$N<-0
  for (j in 1:prod(J)){
    poststrat$N[j] <- round(250e6 * p_male[poststrat[j,1]+1] * p_eth[poststrat[j,2]] *
                              p_age[poststrat[j,3]]*p_income[poststrat[j,4]]*p_state[poststrat[j,5]]) #Adjust the N to be the number observed in each category in each group
  }
  
  # Now let's adjust for the probability of response
  p_response_baseline <- 0.01
  p_response_male <- c(2, 0.8) / 2.8
  p_response_eth <- c(1, 1.2, 2.5) / 4.7
  p_response_age <- c(1, 0.4, 1, 1.5,  3, 5, 7) / 18.9
  p_response_inc <- c(1, 0.9, 0.8) / 2.7
  p_response_state <- rbeta(50, 1, 1)
  p_response_state <- p_response_state / sum(p_response_state)
  p_response <- rep(NA, prod(J))
  for (j in 1:prod(J)) {
    p_response[j] <-
      p_response_baseline * p_response_male[poststrat[j, 1] + 1] *
      p_response_eth[poststrat[j, 2]] * p_response_age[poststrat[j, 3]] *
      p_response_inc[poststrat[j, 4]] * p_response_state[poststrat[j, 5]]
  }
  people <- sample(prod(J), n, replace = TRUE, prob = poststrat$N * p_response)
  
  ## For respondent i, people[i] is that person's poststrat cell,
  ## some number between 1 and 32
  n_cell <- rep(NA, prod(J))
  for (j in 1:prod(J)) {
    n_cell[j] <- sum(people == j)
  }
  
  coef_male <- c(0,-0.3)
  coef_eth <- c(0, 0.6, 0.9)
  coef_age <- c(0,-0.2,-0.3, 0.4, 0.5, 0.7, 0.8, 0.9)
  coef_income <- c(0,-0.2, 0.6)
  coef_state <- c(0, round(rnorm(49, 0, 1), 1))
  coef_age_male <- t(cbind(c(0, .1, .23, .3, .43, .5, .6),
                           c(0, -.1, -.23, -.5, -.43, -.5, -.6)))
  true_popn <- data.frame(poststrat[, 1:5], cat_pref = rep(NA, prod(J)))
  for (j in 1:prod(J)) {
    true_popn$cat_pref[j] <- plogis(
      coef_male[poststrat[j, 1] + 1] +
        coef_eth[poststrat[j, 2]] + coef_age[poststrat[j, 3]] +
        coef_income[poststrat[j, 4]] + coef_state[poststrat[j, 5]] +
        coef_age_male[poststrat[j, 1] + 1, poststrat[j, 3]]
    )
  }
  
  #male or not, eth, age, income level, state, city
  y <- rbinom(n, 1, true_popn$cat_pref[people])
  male <- poststrat[people, 1]
  eth <- poststrat[people, 2]
  age <- poststrat[people, 3]
  income <- poststrat[people, 4]
  state <- poststrat[people, 5]
  
  sample <- data.frame(cat_pref = y, 
                       male, age, eth, income, state, 
                       id = 1:length(people))
  
  #Make all numeric:
  for (i in 1:ncol(poststrat)) {
    poststrat[, i] <- as.numeric(poststrat[, i])
  }
  for (i in 1:ncol(true_popn)) {
    true_popn[, i] <- as.numeric(true_popn[, i])
  }
  for (i in 1:ncol(sample)) {
    sample[, i] <- as.numeric(sample[, i])
  }
  list(
    sample = sample,
    poststrat = poststrat,
    true_popn = true_popn
  )
}

interaction_data <- function(x_variable, y_variable,
                             dependent_variable, data){
  ##
  #x_variable <- x
  #y_variable <- "riding_group"
  #dependent_variable <- "ircCAQ"
  
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

interaction_ridingGroups <- function(independent_variables,
                                     dependent_variable, data){
  
  #data <- PolData
  #independent_variables <- vis
  #y_variable <- "riding_group"
  #dependent_variable <- "ircCAQ"
  
  options(dplyr.summarise.inform = F)
  
  IntPlotData <- data.frame(value_x = c(),
                            value_y = c(),
                            y_axis = c(),
                            facet_x = c(),
                            facet_y = c())
  for (i in 1:length(independent_variables)){
    x <- independent_variables[i]
    datai <- interaction_data(x,
                              "ridinGroup",
                              dependent_variable,
                              data)
    IntPlotData <- rbind(IntPlotData, datai)
  }
  return(IntPlotData)
}

interaction_grid_data <- function(independent_variables,
                                  dependent_variable,
                                  data){
  IntPlotData <- data.frame(value_x = c(),
                            value_y = c(),
                            y_axis = c(),
                            facet_x = c(),
                            facet_y = c())
  combs <- combn(independent_variables,
                 m = 2)
  x <- c(combs[1,], combs[2,])
  y <- c(combs[2,], combs[1,])
  for (i in 1:length(x)){
    datai <- interaction_data(x[i],
                              y[i],
                              dependent_variable,
                              data)
    IntPlotData <- rbind(IntPlotData, datai)
    print(paste0(i, "/", length(combs)))
  }
  return(IntPlotData)
}

extract_preds <- function(id, prop_entier, preds){
  output <- preds[sample(x = 1:4000, size = prop_entier), id]
  return(output)
}


# postStrat table
# model
# postStrat column giving the number of respondents of each strat

weighted_results <- function(poststrat_df, model, poststrat_column_n){
  #####
  #poststrat_df <- postStrat2
  #model <- modelCAQ
  #poststrat_column_n <- "prop_entier"
  #####
  preds <- posterior_linpred(object = model,
                                 transform = TRUE,
                                 newdata = poststrat_df)
  results <- c()
  for (i in 1:nrow(poststrat_df)){
    predsi <- extract_preds(i,
                            round(poststrat_df[[poststrat_column_n]][i]),
                            preds)
    results <- c(results, predsi)
    if (i %% 1000 == 0){
      print(paste0(i, "/", nrow(poststrat_df), " - ",
                   round(i/nrow(poststrat_df)*100), "%"))
    }
  }
  return(results)
}

dummy_variables <- function(original_data, model){
  classes <- attr(model$terms, "dataClasses")
  vars_to_dummy <- names(classes[classes=="factor"])
  d <- original_data %>% 
    dummy_cols(.,
               select_columns = vars_to_dummy)
  names(d)[(ncol(original_data)+1):ncol(d)] <- gsub("_", "",
                                                    names(d)[(ncol(original_data)+1):ncol(d)])
  return(d)
}

find_interactions <- function(vis){
  interactions <- vis[grepl(":", vis)]
  output <- strsplit(interactions, ":")
  return(output)
}

predictor <- function(variable, coefs, newdata){
  coef <- coefs[variable]
  vec <- coef*newdata[[variable]] 
  return(vec)
}

apply_tresholds <- function(lin_pred, tresholds){
  if (lin_pred < tresholds[1]){
    return(0)
  } else {
    wo_higher <- tresholds[tresholds <= lin_pred]
    treshold <- names(wo_higher)[length(wo_higher)]
    value <- as.numeric(strsplit(treshold, split = "|", fixed = T)[[1]][2])
    return(value) 
  }
}

predict_continuous <- function(model, newdata, sign_treshold){
  #model <- modelCAQ
  #newdata <- postStrat_weighted
  #sign_treshold <- 0.1
  
  ## 1 transform newdata to dummy variables that need to be dummied
  newdata <- dummy_variables(newdata, model)
  ## 2 Keep significative terms with their coefficient
  pvalues <- summary(model)$coefficients[,4]
  s_pvalues <- pvalues[pvalues <= sign_treshold]
  tresholds <- model$alpha
  s_pvalues <- s_pvalues[-c(1:length(tresholds))] ## remove tresholds
  s_coefs <- coef(model)[names(s_pvalues)]
  ## 3 Create interaction variables
  interactions <- find_interactions(names(s_coefs))
  if (length(interactions) > 0){
    for (i in 1:length(interactions)){
      interactioni <- interactions[[i]]
      col_name <- paste0(interactioni[1], ":", interactioni[2])
      newdata[[col_name]] <- newdata[[interactioni[1]]] + newdata[[interactioni[2]]]
      newdata[[col_name]] <- ifelse(newdata[[col_name]] == 2, 1, 0)
    }
  }
  ## 4 Generate linear prediction for each fixed effects
  pred_data <- data.frame(id = 1:nrow(newdata))
  for (i in 1:length(s_coefs)){
    #i <- 1
    variable <- names(s_coefs)[i]
    pred_data[[variable]] <- predictor(variable,
                                       s_coefs,
                                       newdata)
  }
  ## 5 Add riding effect
  riding_effects_df <- ranef(model)$riding_test
  riding_effects <- riding_effects_df$`(Intercept)`
  names(riding_effects) <- rownames(riding_effects_df)
  pred_data$riding_test <- riding_effects[as.character(newdata$riding_test)]
  #### remove id column
  pred_data <- pred_data %>% 
    select(-id)
  ## 6 Generate linear predictions
  linear_preds <- rowSums(pred_data)
  return(linear_preds)
}


predict_clm <- function(model, newdata, sign_treshold){
  ## 1 Get continuous linear prediction from clm model
  linear_preds <- predict_continuous(model, newdata, sign_treshold)
  ## 2 Apply tresholds
  results <- sapply(linear_preds,
                    apply_tresholds,
                    tresholds)
  return(results)
}

predict_weighted_irc <- function(model, postStrat_df, sign_treshold,
                                 weight_column, model_dv_vector,
                                 sd_divider = 2){
  ###
  #model <- modelCAQ
  #postStrat_df <- postStrat2
  #sign_treshold <- 0.1
  #weight_column <- "weight"
  #sd_divider <- 2
  #model_dv_vector <- PolData$ircCAQ
  ###
  ### 1 Create a sample of predictions
  sample_data <- postStrat_df[sample(1:nrow(postStrat_df), 5000),]
  sample_preds <- predict_continuous(model, sample_data, sign_treshold = 0.1)
  ### 2 Get sd of the sample of predictions
  sd_sample <- sd(sample_preds)
  ### 3 Get sd of model (arbitrary)
  sd_model <- sd_sample/sd_divider
  ### 4 Generate distribution of n = pop_weight, mean = prediction and sd = sd_model
  raw_preds <- predict_continuous(model, postStrat_df, sign_treshold = sign_treshold)
  distributions <- list()
  for(i in 1:nrow(postStrat_df)){
    distributions[[i]] <- rnorm(n = postStrat_df[[weight_column]][i],
                                mean = raw_preds[i],
                                sd = sd_model)
    if (i %% 1000 == 0){
      print(i)
    }
  }
  adj_pred <- unlist(distributions)
  ### 5 Synthetize the same tresholds as irc from surveys.. since the overall distribution seems
  ###     to be representative from surveys to survey
  pctile <- ecdf(adj_pred)
  quantile_pred <- pctile(adj_pred)
  results <- quantile(model_dv_vector, quantile_pred,
                      na.rm = T)
  return(results)
}

sim_strat_dist <- function(total_combs, xStrat_n, n){
  sdi <- (xStrat_n*2)/4
  dist <- sn::rsn(n = total_combs, xi = 0,
               omega = sdi, alpha = 50000)
  hist(dist)
  sum <- sum(dist)
  factor <- sum/n
  dist <- round(dist/factor)
  return(dist)
}

mrsp_appropriate <- function(n, n_groups, n_categories){
  group_combs <- prod(n_categories)
  total_combs <- group_combs*n_groups
  xStrat_n <- n/total_combs
  strat_dist <- sim_strat_dist(total_combs, xStrat_n, n)
  empty_strats <- length(strat_dist[strat_dist==0])
  print(paste0("Expected frequency of each strat: ", round(xStrat_n, 1), " respondents"))
  print(paste0("Expected number of empty strats: ", empty_strats, " strats (",
               round(empty_strats/length(strat_dist)*100), "%)"))
  hist(strat_dist)
  return(xStrat_n)
}



# at the start:
mrsp_appropriate(9135, 125, c(2,2,5,3,3))
# then tried:
mrsp_appropriate(9135, 125, c(2,2,5))
# and finally:
mrsp_appropriate(9135, 14, c(2,2,5))
mrsp_appropriate(9135, 14, c(2,2,5,3,3))

# For ville québec, started with
mrsp_appropriate(505, 6, c(5,2,3))
  # not bad for a low sample!

mrsp_appropriate(1500, 6, c(5,2,3))
### not a lot of empty strats, but low frequency


mrsp_appropriate(1500, 6, c(3,2,3))
## a little bit better if we reduce the number
### of categories in the age variable

mrsp_appropriate(3000, 6, c(3,2,3))
## great

mrsp_appropriate(7500, 6, c(5,2,3))

mrsp_appropriate(3000, 6, c(3,2,3))
## great


topdown_fa <- function(df, nfactors = 1) {
  
  #df <- fa
  
  
  # Cronbach's alpha (Test 1)
  cronbachAlpha <<- round(psych::alpha(df)$total$raw_alpha, 2)
  
  # Analyse factorielle (Test 2)
  
  factAnalysis <- factanal(df, factors=nfactors) # Analyse factorielle
  factorVarNames <- names(df)
  
  factorLoadings <- as.numeric(factAnalysis$loadings[,1]) 
  factor1stEigen <<- round(eigen(cor(df))$values[1], digit=2)
  
  
  FAplot <- ggplot(data.frame(factorVarNames,factorLoadings), 
                   aes(x=factorVarNames, y=factorLoadings)) + 
    coord_flip() +
    geom_bar(stat="identity", colour="black", fill="black", size=1, width=0.5) +
    geom_text(aes(label=as.character(round(factorLoadings, 
                                           digits = 2))), vjust=0.35, hjust=-0.3, size = 5) +
    geom_hline(yintercept=0.3, colour="gray", linetype = "longdash") +
    annotate("text", label=paste("Alpha de Cronbach =", as.character(cronbachAlpha)), 
             x=1.1, y=1.28, size=5) +
    annotate("text", label=paste("Première valeur propre =", as.character(factor1stEigen)), 
             x=0.75, y=1.28, size=5) +
    annotate("segment", x = 0.4, xend = 1.45, 
             y = 1, yend = 1, colour = "black") +
    annotate("segment", x = 1.45, xend = 1.45, 
             y = 1, yend = Inf, colour = "black") +
    scale_y_continuous(name="\n Coefficients de saturation \n", 
                       limits=c(0, 1.55), breaks=seq(0, 1, by=0.1),
                       expand = c(0,0)) +
    xlab("\n") + 
    theme_linedraw() +
    theme(axis.text.y = element_text(size=15,
                                     margin = margin(r = 5, l = 3)), 
          axis.title.y = element_text(size = 15), 
          axis.text.x = element_text(size = 15),
          axis.title.x = element_text(hjust=0.3, vjust=-0.17, size=20), 
          panel.grid=element_blank())
  print(FAplot)
  print("What we want:")
  print(paste0("Alpha de Cronbach > 0.6 -> ",cronbachAlpha))
  print(paste0("Première Valeur Propre > 1 -> ",factor1stEigen))
  print(paste0("Tous les coefficients de saturation > 0.3"))
}


finverser <- function(vec_col){
  #vec_col <- df[[col]]
  unique_col <- unique(vec_col)
  unique_col <- unique_col[!is.na(unique_col)]
  n <- length(unique_col)
  max <- max(vec_col, na.rm = T)
  ord <- sort(as.vector(unique_col))
  rev <- rev(ord)
  for (i in 1:n){
    vec_col[vec_col == ord[i]] <- max + rev[i] 
  }
  vec_col <- vec_col - max
  return(vec_col)
}

input_attitudes <- function(
    raw_data = Data,
    clean_data = CleanData,
    raw_varName = "secularism_must_be_encouraged",
    model_type = c("lm", "glm"),
    pctiles = F
  ){
  vec <- raw_data[[raw_varName]]
  if (model_type == "lm"){
    model <- lm(vec ~ factor(age_cat) + factor(educ) +
                 factor(income) + male + lang + ircPCQ_model +
                 ircQS_model + ircPQ_model + ircCAQ_model + ircPLQ_model +
                 factor(ridingGroup, ordered = F),
                 data = clean_data)
  } else if (model_type == "glm"){
    model <- glm(vec ~ factor(age_cat) + factor(educ) +
                  factor(income) + male + lang + ircPCQ_model +
                  ircQS_model + ircPQ_model + ircCAQ_model + ircPLQ_model +
                  factor(ridingGroup, ordered = F),
                  data = clean_data,
                  family = binomial())
  } else {
    print("Choose an appropriate model")
    return(rep(NA, length(vec)))
  }
  preds <- predict(model, clean_data, type = "response")
  output <- minmaxNormalization(coalesce(vec, preds))
  if (isTRUE(pctiles)){
    pctile <- ecdf(output)
    quantile_pred <- pctile(output)
    output <- quantile(vec, quantile_pred, na.rm = T)
  } else {
    output <- output
  }
  return(output)
}


softmax <- function(par){
  n.par <- length(par)
  par1 <- sort(par, decreasing = TRUE)
  Lk <- par1[1]
  for (k in 1:(n.par-1)) {
    Lk <- max(par1[k+1], Lk) + log1p(exp(-abs(par1[k+1] - Lk))) 
  }
  val <- exp(par - Lk)
  return(val)
}

# Example 1
vec <- c(0.5, 0, 0)
sm <- softmax(vec)
sm
sum(sm)


transform_rci <- function(rcis){
  return((rcis+1)/2)
}

scale_quantile_value <- function(quantile_position, scale, data = Data){
  output <- quantile(data[[paste0("scale_", scale)]],
                     quantile_position,
                     na.rm = T)
  return(output)
}

scale_quantile_vector <- function(quantile_positions, scales_vector, data = Data){
  output <- mapply(FUN = scale_quantile_value,
                   quantile_positions,
                   scales_vector,
                   MoreArgs = list(data = Data))
  names(output) <- scales_vector
  return(output)
}

get_party_position <- function(party, scale, party_positions_matrix){
  return(party_positions_matrix[party,scale])
}

get_party_positions <- function(parties, scales, party_positions_matrix){
  output <- mapply(FUN = get_party_position,
                   parties,
                   scales,
                   MoreArgs = list(party_positions_matrix = party_positions_matrix))
  names(output) <- paste0(parties, "_", scales)
  return(output)
}

generate_gaps <- function(party,
                          data,
                          scales_prefix = "scale_",
                          id_col = "id",
                          positions_matrix = party_positions_matrix,
                          gap_type = "reversed"){
  if (!gap_type %in% c("raw", "absolute", "reversed")) {
    stop("gap_type must be either 'raw', 'absolute' or the default 'reversed'")
  }
  gap_types <- c("raw" = "raw_gap",
                 "absolute" = "abs_gap",
                 "reversed" = "rev_gap")
  gap_col <- as.character(gap_types[gap_type])
  long <- data %>% 
    select(all_of(id_col), starts_with(scales_prefix)) %>% 
    pivot_longer(., cols = starts_with(scales_prefix),
                 names_to = "scale",
                 names_prefix = scales_prefix,
                 values_to = "attitude")
  long$position <- positions_matrix[party,][long$scale]
  output <- long %>%
    mutate(raw_gap = attitude-position,
           abs_gap = abs(raw_gap),
           rev_gap = 1-abs_gap) %>% 
    pivot_wider(., id_cols = id_col,
                names_from = "scale",
                values_from = gap_col,
                names_prefix = paste0(gap_col, "_")) %>% 
    mutate(party = party)
  return(output)
}

get_party_formula <- function(party, dv){
  f_base <- paste0(dv, " ~ ")
  f_vector <- if (dv == "voteInt"){
          c("CAQ" = "ridingGroup*age_model + lang*ridingGroup +
                male*ridingGroup + lang*educ + age_model*lang +
                income + sophis_followPolls + sophis_polKnowledge +
                partyId + irc",
                
                "PLQ" = "ridingGroup*age_model + male + educ + age_model*lang +
                income + sophis_followPolls + sophis_polKnowledge +
                partyId + irc",
                
                "PQ" = "ridingGroup*age_model + ridingGroup*male + educ + age_model*lang +
                male*income + sophis_followPolls + sophis_polKnowledge +
                partyId + irc",
                
                "QS" = "ridingGroup*lang + age_model + educ*income +
                male*income + lang*income + sophis_followPolls + sophis_polKnowledge +
                partyId + irc",
                
                "PCQ" = "ridingGroup*lang + age_model*lang + educ*lang +
                male*age_model + sophis_followPolls + sophis_polKnowledge +
                partyId + irc")
  } else if (dv == "irc"){
    c("CAQ" = "ridingGroup*age_model + lang*ridingGroup +
                male*ridingGroup + lang*educ + age_model*lang +
                income + sophis_followPolls + sophis_polKnowledge +
                partyId + rev_gap_leftRight + rev_gap_nationaSouv +
                rev_gap_langFr + rev_gap_laicite + rev_gap_immigration +
                rev_gap_woke + rev_gap_3elien + rev_gap_demoTrust",
      
      "PLQ" = "ridingGroup*age_model + male + educ + age_model*lang +
                income + sophis_followPolls + sophis_polKnowledge +
                partyId + rev_gap_leftRight + rev_gap_nationaSouv +
                rev_gap_langFr + rev_gap_laicite + rev_gap_immigration +
                rev_gap_woke + rev_gap_3elien + rev_gap_demoTrust",
      
      "PQ" = "ridingGroup*age_model + ridingGroup*male + educ + age_model*lang +
                male*income + sophis_followPolls + sophis_polKnowledge +
                partyId + rev_gap_leftRight + rev_gap_nationaSouv +
                rev_gap_langFr + rev_gap_laicite + rev_gap_immigration +
                rev_gap_woke + rev_gap_3elien + rev_gap_demoTrust",
      
      "QS" = "ridingGroup*lang + age_model + educ*income +
                male*income + lang*income + sophis_followPolls + sophis_polKnowledge +
                partyId + rev_gap_leftRight + rev_gap_nationaSouv +
                rev_gap_langFr + rev_gap_laicite + rev_gap_immigration +
                rev_gap_woke + rev_gap_3elien + rev_gap_demoTrust",
      
      "PCQ" = "ridingGroup*lang + age_model*lang + educ*lang +
                male*age_model + sophis_followPolls + sophis_polKnowledge +
                partyId + rev_gap_leftRight + rev_gap_nationaSouv +
                rev_gap_langFr + rev_gap_laicite + rev_gap_immigration +
                rev_gap_woke + rev_gap_3elien + rev_gap_demoTrust")
  } else {
      stop("dv must be either 'voteInt' or 'irc'")
    }
  f_right <- f_vector[party]
  f <- as.formula(paste0(f_base, f_right))
  return(f)
}

generate_model_data <- function(party = "CAQ",
                                list_party_data = listParties,
                                data = Data,
                                scales_prefix = "scale_",
                                id_col = "id",
                                positions_matrix = party_positions_matrix,
                                gap_type = "reversed",
                                controls = c("ridingGroup","age_model","educ","male","income","lang",
                                             "sophis_followPolls", "sophis_polKnowledge")){
  gap_types <- c("raw" = "raw_gap",
                 "absolute" = "abs_gap",
                 "reversed" = "rev_gap")
  gap_col <- as.character(gap_types[gap_type])
  scales_data <- generate_gaps(party = party,
                               data = data,
                               scales_prefix = scales_prefix,
                               id_col = id_col,
                               positions_matrix = positions_matrix,
                               gap_type = gap_type)
  modelData <- data %>%
    select(all_of(id_col), all_of(controls)) %>% 
    left_join(., list_party_data[[party]], by = id_col) %>% 
    select(-party) %>% 
    left_join(., scales_data, by = id_col)
  return(modelData)
}


generate_models <- function(party = "CAQ",
                            list_party_data = listParties,
                            data = Data,
                            scales_prefix = "scale_",
                            id_col = "id",
                            positions_matrix = party_positions_matrix,
                            gap_type = "reversed",
                            controls = c("ridingGroup","age_model","educ","male","income","lang",
                                         "sophis_followPolls", "sophis_polKnowledge")){
  modelData <- generate_model_data(party = party,
                                   list_party_data = list_party_data,
                                   data = data,
                                   scales_prefix = scales_prefix,
                                   id_col = id_col,
                                   positions_matrix = positions_matrix,
                                   gap_type = gap_type,
                                   controls = controls)
  formula1 <- get_party_formula(party, "irc")
  model1 <- modelData %>%
    mutate(irc = factor(irc, ordered = T)) %>% 
    MASS::polr(formula1, data = ., Hess = T)
  formula2 <- get_party_formula(party, "voteInt")
  model2 <- glm(formula2, data = modelData,
                family = binomial())
  model_list <- list()
  model_list[[1]] <- model1
  model_list[[2]] <- model2
  return(model_list)
}

test <- MASS::polr(factor(irc_CAQ) ~ voteInt_CAQ,
                   data = Data)

assemble_newData <- function(party = "CAQ",
                             list_party_data = listParties,
                             data = Data,
                             scales_prefix = "scale_",
                             id_col = "id",
                             positions_matrix = party_positions_matrix,
                             gap_type = "reversed"){
  party_arg <- party
  NewData <- generate_gaps(party = party_arg,
                           data = data,
                           scales_prefix = scales_prefix,
                           id_col = id_col,
                           positions_matrix = positions_matrix,
                           gap_type = gap_type) %>% 
    left_join(data, ., by = id_col) %>% 
    select(-party) %>% 
    left_join(., listParties[[party_arg]], by = id_col)
  return(NewData)
}


