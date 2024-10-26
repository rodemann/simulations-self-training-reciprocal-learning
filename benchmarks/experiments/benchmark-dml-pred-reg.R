library(dplyr)
source("R/diff_marg_likelihood_pred_reg.R")

set.seed(34059254)
method = "diff_marg_likelihood_pred_reg"


trans_res = vector()
ind_res = vector()

# share of unlabeled obs
n_imp = ((nrow(data_frame) - n_test) * share_unlabeled) %>% round()

ind_res_on_the_fly = matrix(nrow = n_imp, ncol = N)

params_list = list()


for (iter in 1:N) {
  Sys.time() %>% print()
  
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
  
  results_list <- diff_marg_likelihood_pred_reg(labeled_data = labeled_data, 
                                                unlabeled_data = unlabeled_data,
                                                test_data = test_data,
                                                target = target,
                                                glm_formula = formula)
  params_list[[iter]] = results_list[[3]]
  
  # get transductive and inductive results
  results <- results_list[[1]]
  # get model
  model <- results_list[[2]]
  
  # on the fly test results
  ind_res_on_the_fly[,iter] <- results[,3]
  
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

mean_ind_fly <- apply(ind_res_on_the_fly, 1, mean)
sd_ind_fly <- apply(ind_res_on_the_fly, 1, sd)
ind_fly = cbind(mean_ind_fly, sd_ind_fly)
get_CI_vec <- function(mean_sd_vec, alpha = 0.05, N){
  mean = mean_sd_vec[1]
  sd = mean_sd_vec[2]
  dist <- get_dist(sd, alpha = alpha, n=N)
  c(mean - dist, mean + dist) %>% t()
}
CI_fly <- apply(ind_fly, 1, get_CI_vec, alpha = 0.05, N=N)


# Initialize a list to store results for each iteration
iteration_results <- vector("list", n_imp)
iteration_results_sd <- vector("list", n_imp)

# Loop through each iteration
for (iteration in 1:n_imp) {
  # Extract results for the current iteration from all restarts
  iteration_data <- lapply(params_list, function(x) x[[iteration]])
  
  # Combine all restarts' data for the current iteration into a data frame
  iteration_df <- do.call(rbind, iteration_data)
  
  # Calculate the mean for each parameter across all restarts
  iteration_mean <- colMeans(iteration_df)
  
  # Calculate standard deviation
  iteration_sd <- apply(iteration_df, 2, sd)
  
  # Store the mean values in the results list
  iteration_results[[iteration]] <- iteration_mean
  
  # Store the sd values in the results list
  iteration_results_sd[[iteration]] <- iteration_sd
}

# Convert the list to a data frame for better readability
params_means <- do.call(rbind, iteration_results)
#colnames(iteration_means_df) <- c("Intercept", "feature_1", "feature_2", "feature_3")
params_sd <- do.call(rbind, iteration_results_sd)


saved_results <- list("Inductive on-the-fly mean" = mean_ind_fly,
                      "Inductive on-the-fly CI" = CI_fly,
                      "Inductive CI" = CI_ind,
                      "Inductive n_test" = nrow(test_data),
                      "Transductive CI" = CI_trans,
                      "Transductive n_test" = nrow(true_labels),
                      "Method" = method,
                      "parameters" =  params_means,
                      "parameters_sd" =  params_sd
                  
)


# save results so that they can be accessed and visualized later
path = paste(getwd(),"/results/diff_marg_likelihood_pred_reg_",
             as.character(share_unlabeled),"_",as.character(name_df),
             "_n=", as.character(nrow(data_frame)), "_p=", as.character(p), sep="")
save(saved_results, file = path)
