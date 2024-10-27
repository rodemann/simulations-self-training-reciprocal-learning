library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyverse)
#library(wesanderson)
# library(RColorBrewer)
# library(ggsci)
# library(extrafont)
# library(showtext)
#font_add_google(name = "Amatic SC", family = "amatic-sc")


n_methods = 5
n_test = nrow(data_frame)*0.5
n_test = round(n_test)
#number of unlabeled obs
n_imp = ((n - n_test) * share_unlabeled) %>% round()


load(paste(getwd(),"/results/diff_marg_likelihood_pred_", share_unlabeled,"_",data, "_n=", as.character(n), "_p=", as.character(p), sep=""))
likelihood_params <- saved_results$parameters
likelihood_params_sd <- saved_results$parameters_sd
likelihood_L2 = saved_results$l2norms


load(paste(getwd(),"/results/diff_marg_likelihood_pred_ext_",share_unlabeled,"_",data, "_n=", as.character(n), "_p=", as.character(p), sep=""))
Bayes_crit_params <- saved_results$parameters
Bayes_crit_params_sd <- saved_results$parameters_sd
bayes_crit_L2 = saved_results$l2norms


load(paste(getwd(),"/results/diff_marg_likelihood_pred_reg_",share_unlabeled,"_",data, "_n=", as.character(n), "_p=", as.character(p), sep=""))
Bayes_crit_reg_params <- saved_results$parameters
Bayes_crit_reg_params_sd <- saved_results$parameters_sd
bayes_crit_reg_L2 = saved_results$l2norms



# load(paste(getwd(),"/results/diff_marg_likelihood_all_sampl_",share_unlabeled,"_",data, sep=""))
# saved_results
# df[4,] <- saved_results %>% unlist()
# 
# load(paste(getwd(),"/results/diff_marg_likelihood_all_",share_unlabeled,"_",data, sep=""))
# saved_results
# df[5,] <- saved_results %>% unlist()
# 
# load(paste(getwd(),"/results/diff_marg_likelihood_all_ext_",share_unlabeled,"_",data, sep=""))
# saved_results
# df[6,] <- saved_results %>% unlist()

load(paste(getwd(),"/results/standard_self_",share_unlabeled,"_",data, "_n=", as.character(n), "_p=", as.character(p), sep=""))
pred_var_params <- saved_results$parameters
pred_var_params_sd <- saved_results$parameters_sd
pred_var_params_L2 = saved_results$l2norms


load(paste(getwd(),"/results/standard_self_conf_",share_unlabeled,"_",data, "_n=", as.character(n), "_p=", as.character(p), sep=""))
prob_score_params <- saved_results$parameters
prob_score_params_sd <- saved_results$parameters_sd
prob_score_params_L2 = saved_results$l2norms


# Function to compute L2 norm
compute_l2_norms <- function(results_list) {
  # sapply(results_list, function(params) {
  #   sqrt(sum(params^2))
  # })
  apply(results_list, 1,function(params) {
    sqrt(sum(params^2))
     })
}

# Compute L2 norms for each list
likelihood_params <- compute_l2_norms(likelihood_params) 
Bayes_crit_params <- compute_l2_norms(Bayes_crit_params)
Bayes_crit_reg_params <- compute_l2_norms(Bayes_crit_reg_params)
pred_var_params <- compute_l2_norms(pred_var_params)
prob_score_params <- compute_l2_norms(prob_score_params)

l2_means = list()
l2_sd = list()
all = list(likelihood_L2, bayes_crit_L2, bayes_crit_reg_L2,
        pred_var_params_L2, prob_score_params_L2)
for (i in seq_along(all)) {
  l2norms_entries = all[[i]]
  n <- length(l2norms_entries[[1]]) # Number of values in each vector
  l2_means[[i]] <- sapply(1:n, function(i) mean(sapply(l2norms_entries, `[[`, i)))
  l2_sd[[i]] <- sapply(1:n, function(i) sd(sapply(l2norms_entries, `[[`, i)))
}


# Combine data into a single data frame for param values (OLD)
df_params <- data.frame(
  Iteration = rep(1:length(likelihood_params), 5),
  L2_Norm = c(likelihood_params, Bayes_crit_params, Bayes_crit_reg_params,pred_var_params,prob_score_params),
  "Selection Criterion" = rep(c("Likelihood", "Bayes-Crit", "Bayes-crit-Reg", "Predictive Var","Probability Score"), each = length(likelihood_params))
)


# Combine data into a single data frame
df <- data.frame(
  Iteration = rep(1:length(likelihood_params), 5),
  L2_Norm = unlist(l2_means),
  "Selection Criterion" = rep(c("Likelihood", "Bayes-Crit", "Bayes-crit-Reg", "Predictive Var", "Probability Score"), each = length(likelihood_params))
)

# Plot the L2 norm over time for each list
plot= ggplot(df, aes(x = Iteration, y = L2_Norm, color = Selection.Criterion, group = Selection.Criterion)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "L2 Norm of Parameters Over Time",
       x = "Iteration",
       y = "L2 Norm") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  )

plot

# Calculate confidence intervals for each entry (iteration)
df_with_ci <- data.frame(
  Iteration = rep(1:length(l2_means[[1]]), 5),
  L2_Norm = unlist(l2_means),
  Lower_CI = unlist(lapply(seq_along(l2_means), function(i) sapply(1:length(l2_means[[i]]), function(j) l2_means[[i]][j] - 1.96 * (l2_sd[[i]][j] / sqrt(N))))),
  Upper_CI = unlist(lapply(seq_along(l2_means), function(i) sapply(1:length(l2_means[[i]]), function(j) l2_means[[i]][j] + 1.96 * (l2_sd[[i]][j] / sqrt(N))))),
  "Selection Criterion" = rep(c("Likelihood", "Bayes-Crit", "Bayes-crit-Reg", "Predictive Var", "Probability Score"), each = length(l2_means[[1]]))
)

# Plot with confidence intervals
plot_CIs <- ggplot(df_with_ci, aes(x = Iteration, y = L2_Norm, color = Selection.Criterion, group = Selection.Criterion)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2) +
  labs(title = "L2 Norm of Parameters Over Time with 95% Confidence Intervals",
       x = "Iteration",
       y = "L2 Norm") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  )
plot_CIs

filename = paste("plots/res_plot_params_data=", data,N,"_share=",share_unlabeled, "_n=", as.character(n), "_p=", as.character(p),".png", sep = "")
ggsave(filename=filename, plot = plot,  dpi = 300)


filename = paste("plots/res_plot_CIs_params_data=", data,N,"_share=",share_unlabeled, "_n=", as.character(n), "_p=", as.character(p),".png", sep = "")
ggsave(filename=filename, plot = plot_CIs,  dpi = 300)



