library(dplyr)
source("R/standard_supervised.R")

set.seed(3405934)
method = "standard_supervised"

trans_res = vector()
ind_res = vector()




for (iter in 1:N) {
  

  test_rows = sample(nrow(data_frame), size = n_test)
  test_data = data_frame[test_rows,]
  
  # data frame for SSL
  data = data_frame[-test_rows,]
  
  # share of unlabeled obs
  n_imp = (nrow(data) * share_unlabeled) %>% round()
  
  
  # create data setup by randomly unlabelling data points
  unlabeled_data_inst <- sample(nrow(data), n_imp)
  labeled_data <- data[-unlabeled_data_inst,]
  labeled_data <- cbind(labeled_data,nr = 0)
  unlabeled_data <- data[unlabeled_data_inst,]
  # add number
  unlabeled_data <- cbind(unlabeled_data, nr = unlabeled_data_inst)
  true_labels = cbind(unlabeled_data$nr, unlabeled_data[c(target)])
  
  results_list <- standard_supervised(labeled_data = labeled_data, 
                                         unlabeled_data = unlabeled_data,
                                          test_data = test_data,
                                         target = target,
                                         glm_formula = formula)
  # get transductive and inductive results
  results <- results_list[[1]]
  # get model
  model <- results_list[[2]]

  sorted_results = results[order(results[,1]),]
  sorted_true_labels = true_labels[order(true_labels[,1]),]
  
  # tranductive learning results
  res = sum(sorted_results[,2] == sorted_true_labels[,2])
  trans_res[iter] = res
  
  # final inductive learning results
  scores = predict(model, newdata = test_data, type = "response") 
  prediction_test <- ifelse(scores > 0.5, 1, 0)
  ind_res_iter <- sum(prediction_test == test_data[c(target)])
  
  ind_res[iter] = ind_res_iter
  
  print(iter)
}


get_CI <- function(mean, sd, alpha = 0.05, N){
  dist <- get_dist(sd, alpha = alpha, N)
  c(mean - dist, mean + dist)
}

get_dist = function(sd, alpha = 0.05, n) {
  qt(p = 1 - alpha / 2, df = n - 1) * sd / sqrt(n)
}


mean(trans_res)
mean(ind_res)

CI_trans <- get_CI(mean(trans_res), sd(trans_res), N = N)
CI_trans

CI_ind <- get_CI(mean(ind_res), sd(ind_res), N = N)
CI_ind

saved_results <- list(#"Inductive on-the-fly mean" = mean_ind_fly,
                      #"Inductive on-the-fly CI" = CI_fly,
                      "Inductive CI" = CI_ind,
                      "Inductive n_test" = nrow(test_data),
                      "Transductive CI" = CI_trans,
                      "Transductive n_test" = nrow(true_labels),
                      "Method" = method,
                      "parameters" =  model$coefficients
                      
)


# save results so that they can be accessed and visualized later
path = paste(getwd(),"/results/standard_supervised_",
             as.character(share_unlabeled),"_",as.character(name_df),
             "_n=", as.character(nrow(data_frame)), "_p=", as.character(p), sep="")
save(saved_results, file = path)
