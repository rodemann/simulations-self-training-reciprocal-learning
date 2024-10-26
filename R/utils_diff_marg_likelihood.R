library(dplyr)
# old
get_marg_l <- function(logistic_model) {
  fisher_info <- vcov(logistic_model) %>% solve() 
  n_parameters <- length(logistic_model$coefficients)
  marg_l <- -2 * logLik(logistic_model) + n_parameters + log(det(fisher_info)) -
    n_parameters * log(2*pi) 
  marg_l
}

get_log_marg_l <- function(logistic_model) {
  # check if vcov-matrix has NA (in p>n scenarios)
  try({
      n_parameters <- length(logistic_model$coefficients)
      n_obs <- logistic_model$data %>% nrow
      cov_matrix <- vcov(logistic_model)
      if(sum(is.na(cov_matrix)) != nrow(cov_matrix)*ncol(cov_matrix)){
      not_nas <- vcov(logistic_model)[!is.na(vcov(logistic_model))]
      v_cov_matrix <- matrix(not_nas, ncol = sqrt(length(not_nas)))
      fisher_info <- v_cov_matrix %>% solve() 
      log_marg_l <- logLik(logistic_model) + n_parameters/2 * log(2*pi/n_obs) - 0.5 * log(det(fisher_info))
      }else{
        cat("fisher info containing NAs exclusively. Replacing fisher info by 0 in log margL approx.")
        log_marg_l <- logLik(logistic_model) + n_parameters/2 * log(2*pi/n_obs) 
      }
      log_marg_l
    })
}


