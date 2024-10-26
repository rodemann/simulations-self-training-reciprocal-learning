library(dplyr)
library(checkmate,asserthat)
source("R/utils_diff_marg_likelihood.R")


# install keras and tfproba from within, make sure version >= 2.10
# this approach works opposed to reticulate + pip/ conda install
# install_tensorflow(
#   extra_packages = c("keras", "tensorflow-hub", "tensorflow-probability"),
#   version = "2.10"
# )


# import tf, tfproba, keras
library(tensorflow)
library(tfprobability)
library(keras)

###### 
# jann BNN helpers
###### 
posterior_mean_field <- function(kernel_size, bias_size = 0, dtype = NULL) {
    n <- kernel_size + bias_size
    c <- log(expm1(1))
    keras_model_sequential(list(
      layer_variable(shape = 2 * n, dtype = dtype),
      layer_distribution_lambda(
        make_distribution_fn = function(t) {
          tfd_independent(tfd_normal(
            loc = t[1:n],
            scale = 1e-5 + tf$nn$softplus(c + t[(n + 1):(2 * n)])
            ), reinterpreted_batch_ndims = 1)
        }
      )
    ))
  }

prior_trainable <- function(kernel_size, bias_size = 0, dtype = NULL) {
    n <- kernel_size + bias_size
    keras_model_sequential() %>%
      layer_variable(n, dtype = dtype, trainable = TRUE) %>%
      layer_distribution_lambda(function(t) {
        tfd_independent(tfd_normal(loc = t, scale = 1),
                        reinterpreted_batch_ndims = 1)
      })
  }


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
  for (i in seq(n_imp)) {
    # fit model to labeled data
    # labeled data: 1 = label, 2-61 = features, 62 = "nr"
    # https://blogs.rstudio.com/ai/posts/2019-06-05-uncertainty-estimates-tfprobability/

    browser()
    # Data wrangling for use in DL models
    x_l <- as.matrix(labeled_data[, 2:(length(labeled_data)-1)])
    y_l <- as.matrix(as.double(labeled_data[, 1])) - 1
    x_u <- as.matrix(unlabeled_data[, 2:(length(unlabeled_data)-1)])

    num_classes <- length(unique(y_l))
    
    bnn <- keras::keras_model_sequential() %>% tfprobability::layer_dense_variational(
        units = 128,
        batch_input_shape=list(NULL, ncol(x_l)), # input shape for first layer
        make_posterior_fn = posterior_mean_field,
        make_prior_fn = prior_trainable,
        activation = NULL
      ) %>% tfprobability::layer_dense_variational(
        units = num_classes - 1, # binary classification case
        make_posterior_fn = posterior_mean_field,
        make_prior_fn = prior_trainable,
        activation = NULL
      # ) %>% tfprobability::layer_one_hot_categorical(
      #   event_size = num_classes - 1
      # )
      ) %>% tfprobability::layer_independent_bernoulli(
        event_shape = num_classes - 1
      )
      
    negloglik <- function(y, model) - (model %>% tfd_log_prob(y))

    # build model
    bnn %>% keras::compile(optimizer = keras::optimizer_adam(learning_rate = 0.01), loss = negloglik)
    # train model
    bnn %>% keras::fit(x_l, y_l, epochs = 10)
    
    # get predictions for single test samples
    yhat = bnn(tf$constant(x_u[1:5, ]))

    # get mean and stddev predictions for these 5 dummy training samples
    # not really meaningful?!?
    yhat %>% tfd_mean()
    yhat %>% tfd_stddev()

    # make inference, dummy: eval on labeled training 
    # from the tutorial, they sample predictive distributions from the model hence the map()
    predicted_target_dist <- purrr::map(1:10, function(x) bnn(tf$constant(x_u)))
    predicted_target_means <- purrr::map(predicted_target_dist, purrr::compose(as.matrix, tfd_mean)) %>% abind::abind()
    predicted_target_stddev <- purrr::map(predicted_target_dist, purrr::compose(as.matrix, tfd_stddev)) %>% abind::abind()

    ######## WORKS UNTIL HERE, MORE EFFORT REQUIRED TO FINISH PL LOOP
    # browser()

    # logistic_model <- glm(formula = formula, 
    #                       data = labeled_data, 
    #                       family = "binomial")
    
    # # predict on unlabeled data
    # predicted_target <- predict(logistic_model, 
    #                             newdata= unlabeled_data, 
    #                             type = "response")

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
    
  }
  # get final model
  final_model <- logistic_model <- glm(formula = formula, 
                                       data = labeled_data, 
                                       family = "binomial")
  # return transductive results (labels) and final model
  list(results, final_model)
  
}


