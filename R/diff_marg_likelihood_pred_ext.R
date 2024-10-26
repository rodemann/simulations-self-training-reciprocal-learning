library(dplyr)
library(checkmate,asserthat)
source("R/utils_diff_marg_likelihood.R")


diff_marg_likelihood_pred_ext <- function(labeled_data,
                                      unlabeled_data,
                                      test_data,
                                      target,
                                      glm_formula) {
  
  # some input checking
  assert_data_frame(labeled_data)
  assert_data_frame(unlabeled_data)
  assert_data_frame(test_data)
  assert_formula(glm_formula)
  assert_character(target)
  
  n_imp = nrow(unlabeled_data)
  results = matrix(nrow = n_imp, ncol = 3)
  which_flip = seq(n_imp)
  params_results = list()
  
  
  for (i in seq(n_imp)) {
    
    # fit model to labeled data
    logistic_model <- glm(formula = formula, 
                          data = labeled_data, 
                          family = "binomial")
    
    # predict on unlabeled data
    predicted_target <- predict(logistic_model, 
                                newdata= unlabeled_data, 
                                type = "response")
    # assign predicted (pseudo) labels to unlabeled data
    unlabeled_data[c(target)] <- ifelse(predicted_target > 0.5, 1,0)  
    
    
    # create datasets that contain labeled data and one predicted instance each
    if(i >= 2){
      which_flip <- which_flip[-(winner)]
    }
    data_sets_pred_init = as.list(seq_along(which_flip)) 
    data_sets_pred = lapply(data_sets_pred_init, function(flip){
      new_data = rbind(labeled_data, unlabeled_data[flip,])
      new_data 
    })
    
    
    # now approximate marginal likelihood for each of the so-created data sets
    
    ## OLD:
    # marg_l_pseudo = list()
    # models_pseudo = list()
    # for(flip_count in seq_along(which_flip)){
    #   logistic_model <- glm(formula = formula, 
    #                         data = data_sets_pred[[flip_count]], 
    #                         family = "binomial")
    #   n <- data_sets_pred[[flip_count]] %>% nrow()
    #   #logistic_model <- step(logistic_model, k = log(n), trace = 0, direction = "backward")
    #   marg_l_pseudo[[flip_count]] <- logistic_model %>% get_log_marg_l()
    #   models_pseudo[[flip_count]] <- logistic_model
    # }
    
    models_pseudo <- lapply(data_sets_pred, function(data){
      logistic_model <- glm(formula = formula,
                            data = data,
                            family = "binomial")
      logistic_model
    })
    
    
    marg_l_pseudo <- lapply(models_pseudo, get_log_marg_l)
    
    
    
    winner <- which.max(unlist(marg_l_pseudo))
    
    
  
    
    
    # predict on it again and add to labeled data
    predicted_target <- predict(logistic_model, newdata= unlabeled_data[winner,], type = "response")
    new_labeled_obs <- unlabeled_data[winner,]
    new_labeled_obs[c(target)] <- ifelse(predicted_target > 0.5, 1,0)  

    # evaluate test error (on-the-fly inductive learning results)
    scores = predict(logistic_model, newdata = test_data, type = "response") 
    prediction_test <- ifelse(scores > 0.5, 1, 0)
    test_acc <- sum(prediction_test == test_data[c(target)])/nrow(test_data)
    
    
    # update labeled data
    labeled_data<- rbind(labeled_data, new_labeled_obs)
    # store results
    results[i,] <- c(unlabeled_data[winner,]$nr, new_labeled_obs[c(target)], test_acc) %>% unlist()
    unlabeled_data <- unlabeled_data[-winner,]
    
    # save parameter vector
    params = logistic_model$coefficients
    params_results[[i]] = params
    
  }
  # get final model
  final_model <- logistic_model <- glm(formula = formula, 
                                       data = labeled_data, 
                                       family = "binomial")
  # return transductive results (labels) and final model
  list(results, final_model,params_results)
  
}


