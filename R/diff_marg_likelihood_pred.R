library(dplyr)
library(checkmate,asserthat)


#see algo 1 in paper 
# 
diff_marg_likelihood_pred <- function(labeled_data,
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
  params_results = list()
  which_flip = seq(n_imp)
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
    
    # for (flip_count in seq_along(which_flip)) {
    #   flip = which_flip[flip_count]
    #   new_data = rbind(labeled_data, unlabeled_data[flip_count,])
    #   data_sets_pred[[flip_count]] = new_data 
    # }
    
    
    # now approximate marginal likelihood for each of the so-created data sets


    
    models_pseudo <- lapply(data_sets_pred, function(data){
      logistic_model <- glm(formula = formula,
                            data = data,
                            family = "binomial")
      logistic_model
      })
    
    
    BICs_pseudo <- lapply(models_pseudo, BIC)
    
    
    ##TODO parallelize stuff
    #res_list = as.list(seq_along(which_flip)) 
    # num_cores <- parallel::detectCores() - 1 # parallelize tuning
    # comp_clusters <- parallel::makeCluster(num_cores) # parallelize tuning
    # object_vec = c(formula)
    # env <- environment()
    # parallel::clusterExport(cl=comp_clusters, varlist = object_vec) 
    # 
    # models_pseudo <- parallel::parLapply(cl = comp_clusters, data_sets_pred, fun = function(data){
    #   logistic_model <- glm(formula = formula, 
    #                         data = data, 
    #                         family = "binomial")
    #   logistic_model
    #   })
    # parallel::stopCluster(comp_clusters)
    # 
    
    
    # model selection:
    #n <- data_sets_pred[[flip_count]] %>% nrow()
    #logistic_model <- step(logistic_model, k = log(n), trace = 0, direction = "backward")
    
## old:    
    # for(flip_count in seq_along(which_flip)){
    #   logistic_model <- glm(formula = formula, 
    #                         data = data_sets_pred[[flip_count]], 
    #                         family = "binomial")
    #   n <- data_sets_pred[[flip_count]] %>% nrow()
    #   #logistic_model <- step(logistic_model, k = log(n), trace = 0, direction = "backward")
    #   BICs_pseudo[[flip_count]] <- logistic_model %>% BIC()
    #   models_pseudo[[flip_count]] <- logistic_model
    # }

    winner <- which.min(unlist(BICs_pseudo))
  

    
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


