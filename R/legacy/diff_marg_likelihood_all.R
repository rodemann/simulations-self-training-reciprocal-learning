library(dplyr)
library(checkmate,asserthat)
source("R/utils_diff_marg_likelihood.R")

  diff_marg_likelihood_all <- function(labeled_data,
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
      BICs_1 = list()
      AICs_1 = list()
      models_1 = list()
      for(flip_count in seq_along(which_flip)){
        logistic_model <- glm(formula = formula, 
                              data = data_sets_0[[flip_count]], 
                              family = "binomial")
        n <- data_sets_1[[flip_count]] %>% nrow()
        #logistic_model <- step(logistic_model, k = log(n), trace = 0, direction = "backward")
        BICs_1[[flip_count]] <- logistic_model %>% BIC()
        AICs_1[[flip_count]] <- logistic_model %>% AIC()
        models_1[[flip_count]] <- logistic_model
      }
      
      
      # fit models for 0
      BICs_0 = list()
      AICs_0 = list()
      models_0 = list()
      for(flip_count in seq_along(which_flip)){
        logistic_model <- glm(formula = formula, 
                              data = data_sets_1[[flip_count]], 
                              family = "binomial")
        n <- data_sets_0[[flip_count]] %>% nrow()
        #logistic_model <- step(logistic_model, k = log(n), trace = 0, direction = "backward")
        BICs_0[[flip_count]] <- logistic_model %>% BIC()
        AICs_0[[flip_count]] <- logistic_model %>% AIC()
        models_0[[flip_count]] <- logistic_model
      }
      
      #marginal AICs/BICs
      logistic_model <- glm(formula = formula, 
                            data = labeled_data, 
                            family = "binomial")
      n <- labeled_data %>% nrow()
      #logistic_model <- step(logistic_model, k = log(n), trace = 0, direction = "backward")
      BIC_null <- logistic_model %>% BIC()
      AIC_null <- logistic_model %>% AIC()
      
      #print(BIC_null - unlist(BICs_0))
      #print(BIC_null - unlist(BICs_1))
      
      switch (crit,
        "sum" = {
        delta_BIC = abs(BIC_null - unlist(BICs_0)) + abs(BIC_null - unlist(BICs_1))
        delta_AIC = abs(AIC_null - unlist(AICs_0)) + abs(AIC_null - unlist(AICs_1))
        },
        "weighted sum" = {
          delta_BIC = risk_aversion * min(abs(BIC_null - unlist(BICs_0)), abs(BIC_null - unlist(BICs_1))) + 
            (1 - risk_aversion) * max(abs(BIC_null - unlist(BICs_0)), abs(BIC_null - unlist(BICs_1)))
          delta_AIC = risk_aversion * min(abs(AIC_null - unlist(AICs_0)), abs(AIC_null - unlist(AICs_1))) + 
            (1 - risk_aversion) * max(abs(AIC_null - unlist(AICs_0)), abs(AIC_null - unlist(AICs_1)))
        },
        "min" = {
          delta_BIC = min(abs(BIC_null - unlist(BICs_0)), abs(BIC_null - unlist(BICs_1)))
          delta_AIC = min((AIC_null - unlist(AICs_0)), abs(AIC_null - unlist(AICs_1)))
        },
        "max" = {
          delta_BIC = max(abs(BIC_null - unlist(BICs_0)), abs(BIC_null - unlist(BICs_1)))
          delta_AIC = max((AIC_null - unlist(AICs_0)), abs(AIC_null - unlist(AICs_1)))
        }
        )

      winner = which.min(delta_BIC)
      
      # # predict on it
      # predicted_target <- predict(logistic_model, newdata= unlabeled_data[winner,], type = "response")
      # 
      new_labeled_obs <- unlabeled_data[winner,]
      new_labeled_obs[c(target)] <- ifelse(BICs_0[[winner]] > BICs_1[[winner]], 0,1)  
      labeled_data <- rbind(labeled_data, new_labeled_obs) 
      
      labeled_data <- rbind(labeled_data, new_labeled_obs)
      
      # print(unlabeled_data[winner,]$nr)
      # print(new_labeled_obs[c(target)])
      # 
      results[i,] <- c(unlabeled_data[winner,]$nr, new_labeled_obs[c(target)]) %>% unlist()
      
      
      unlabeled_data <- unlabeled_data[-winner,]
      
      BIC_null - unlist(BICs_0)
      BIC_null - unlist(BICs_1)
      

    }
    # get final model
    final_model <- logistic_model <- glm(formula = formula, 
                                         data = labeled_data, 
                                         family = "binomial")
    # return transductive results (labels) and final model
    list(results, final_model)
  
  }


