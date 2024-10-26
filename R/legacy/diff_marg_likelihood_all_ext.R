library(dplyr)
library(checkmate,asserthat)
source("R/utils_diff_marg_likelihood.R")

diff_marg_likelihood_all_ext <- function(labeled_data,
                                     unlabeled_data,
                                     target,
                                     glm_formula, 
                                     crit = "sum",
                                     risk_aversion = NULL) {
  
  # some input checking
  assert_number(n_imp)
  assert_data_frame(data)
  assert_formula(formula)
  assert_character(crit)
  assert_character(target)
  
  if(is.null(risk_aversion) == FALSE)
    assert_numeric(risk_aversion)
  if(crit == "weighted sum" && is.null(risk_aversion))
    stop("When using weihgted sum as criterion, a degree of risk aversion must be specified")
  if(is.null(risk_aversion) == FALSE && between(risk_aversion, 0, 1) == FALSE)
    stop("risk aversion must lie between 0 and 1.")
  
  n_imp = nrow(unlabeled_data)
  
  results = matrix(nrow = n_imp, ncol = 2)
  which_flip = seq(n_imp)
  
  for (i in seq(n_imp)) {
    if(i >= 2){
      which_flip <- which_flip[-(winner)]
    }
    unlabeled_data[,8] <- 1
    data_sets_1 = list()  
    for (flip_count in seq_along(which_flip)) {
      flip = which_flip[flip_count]
      new_data = rbind(labeled_data, unlabeled_data[flip_count,])
      data_sets_1[[flip_count]] = new_data 
    }
    unlabeled_data[,8] <- 0
    data_sets_0 = list()  
    for (flip_count in seq_along(which_flip)) {
      flip = which_flip[flip_count]
      new_data = rbind(labeled_data, unlabeled_data[flip_count,])
      data_sets_0[[flip_count]] = new_data 
    }
    
    # fit models for 1
    marg_l_1 = list()
    models_1 = list()
    for(flip_count in seq_along(which_flip)){
      logistic_model <- glm(formula = formula, 
                            data = data_sets_0[[flip_count]], 
                            family = "binomial")
      n <- data_sets_1[[flip_count]] %>% nrow()
      #logistic_model <- step(logistic_model, k = log(n), trace = 0, direction = "backward")
      marg_l_1[[flip_count]] <- get_log_marg_l(logistic_model)
      models_1[[flip_count]] <- logistic_model
    }
    
    
    # fit models for 0
    marg_l_0 = list()
    models_0 = list()
    for(flip_count in seq_along(which_flip)){
      logistic_model <- glm(formula = formula, 
                            data = data_sets_1[[flip_count]], 
                            family = "binomial")
      n <- data_sets_0[[flip_count]] %>% nrow()
      #logistic_model <- step(logistic_model, k = log(n), trace = 0, direction = "backward")
      marg_l_0[[flip_count]] <- get_log_marg_l(logistic_model)
      models_0[[flip_count]] <- logistic_model
    }

    #marginal AICs/BICs
    logistic_model <- glm(formula = formula, 
                          data = labeled_data, 
                          family = "binomial")
    n <- labeled_data %>% nrow()
    #logistic_model <- step(logistic_model, k = log(n), trace = 0, direction = "backward")
    marg_l_null <- logistic_model %>% get_log_marg_l()
    
    
    switch (crit,
            "sum" = {
              delta_marg_l = abs(marg_l_null - unlist(marg_l_0)) + abs(marg_l_null - unlist(marg_l_1))
            },
            "weighted sum" = {
              delta_marg_l = risk_aversion * min(abs(marg_l_null - unlist(marg_l_0)), abs(marg_l_null - unlist(marg_l_1))) + 
                (1 - risk_aversion) * max(abs(marg_l_null - unlist(marg_l_0)), abs(marg_l_null - unlist(marg_l_1)))
            },
            "min" = {
              delta_marg_l = min(abs(marg_l_null - unlist(marg_l_0)), abs(marg_l_null - unlist(marg_l_1)))
            },
            "max" = {
              delta_marg_l = max(abs(marg_l_null - unlist(marg_l_0)), abs(marg_l_null - unlist(marg_l_1)))
            }
    )
    
    winner = which.min(delta_marg_l)
    
    
    # # predict on it
    # predicted_target <- predict(logistic_model, newdata= unlabeled_data[winner,], type = "response")
    new_labeled_obs <- unlabeled_data[winner,]
    new_labeled_obs[c(target)] <- ifelse(marg_l_0[[winner]] > marg_l_1[[winner]], 0,1)  
    labeled_data <- rbind(labeled_data, new_labeled_obs)
    
    # print(unlabeled_data[winner,]$nr)
    # print(new_labeled_obs$target)
    # 
    results[i,] <- c(unlabeled_data[winner,]$nr, new_labeled_obs[c(target)]) %>% unlist()
    
    
    unlabeled_data <- unlabeled_data[-winner,]
    
    marg_l_null - unlist(marg_l_0)
    marg_l_null - unlist(marg_l_1)
    
    
  }
  # get final model
  final_model <- logistic_model <- glm(formula = formula, 
                                       data = labeled_data, 
                                       family = "binomial")
  # return transductive results (labels) and final model
  list(results, final_model)
  
}


