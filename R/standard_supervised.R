library(dplyr)
library(checkmate,asserthat)




standard_supervised <- function(labeled_data,
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
  

 
    logistic_model <- glm(formula = formula, 
                          data = labeled_data, 
                          family = "binomial")
    # predict on unlabeled
    
    predicted_target <- predict(logistic_model, newdata = unlabeled_data, type = "response")
    new_labeled_obs <- unlabeled_data
    new_labeled_obs[c(target)] <- ifelse(predicted_target > 0.5, 1,0)  
    
   
    
    predicted_label <- new_labeled_obs[c(target)] %>% unlist()
    number <- new_labeled_obs$nr %>% unlist()
    results <- cbind(number, predicted_label)
    
  # return transductive results (labels) and final model
  list(results, logistic_model)
  
}


