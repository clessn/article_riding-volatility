library(fastDummies)

dummy_variables <- function(original_data, model){
  
  original_data <- post_strat
  
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
  ## 5 Add region effect
  region_effects_df <- ranef(model)$region
  region_effects <- region_effects_df$`(Intercept)`
  names(region_effects) <- rownames(region_effects_df)
  pred_data$region <- region_effects[as.character(newdata$region)]
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
  tresholds <- model$alpha
  ## 2 Apply tresholds
  results <- sapply(linear_preds,
                    apply_tresholds,
                    tresholds)
  return(results)
}
