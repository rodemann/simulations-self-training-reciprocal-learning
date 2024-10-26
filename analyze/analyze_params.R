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





## IMPORTANT: Please source benchmarks (run_benchmarks_******) first


# data = "abalone" # for results 
# n = 100 
# p = 60
# 
# n_test = 0.5*n


# global settings
# share_unlabeled = 0.8 
#data = "simulated"
# n = 80
# p = 10
n_methods = 5
n_test = nrow(data_frame)*0.5
n_test = round(n_test)
#number of unlabeled obs
n_imp = ((n - n_test) * share_unlabeled) %>% round()



# load(paste(getwd(),"/results/diff_marg_likelihood_pred_sampl_", share_unlabeled,"_",data, "_n=", as.character(n), "_p=", as.character(p), sep=""))
# onthefly_acc_paths[1:n_imp,"iter"] <- 1:n_imp
# onthefly_acc_paths[1:n_imp,"Upper.CB"] <- saved_results$`Inductive on-the-fly CI`[2,]
# onthefly_acc_paths[1:n_imp,"Lower.CB"] <- saved_results$`Inductive on-the-fly CI`[1,]
# onthefly_acc_paths[1:n_imp,"Mean.Accuracy"] <- saved_results$`Inductive on-the-fly mean`
# onthefly_acc_paths[1:n_imp,"Method"] <- "marg-L-pred-mcmc"
# saved_results <- saved_results[-c(1,2)]
# df[1,] <- saved_results %>% unlist()
# onthefly_acc_paths_all <- rbind(onthefly_acc_paths_all, onthefly_acc_paths)
# 


load(paste(getwd(),"/results/diff_marg_likelihood_pred_", share_unlabeled,"_",data, "_n=", as.character(n), "_p=", as.character(p), sep=""))
likelihood_params <- saved_results$parameters
likelihood_params_sd <- saved_results$parameters_sd



load(paste(getwd(),"/results/diff_marg_likelihood_pred_ext_",share_unlabeled,"_",data, "_n=", as.character(n), "_p=", as.character(p), sep=""))
Bayes_crit_params <- saved_results$parameters
Bayes_crit_params_sd <- saved_results$parameters_sd


load(paste(getwd(),"/results/diff_marg_likelihood_pred_reg_",share_unlabeled,"_",data, "_n=", as.character(n), "_p=", as.character(p), sep=""))
Bayes_crit_reg_params <- saved_results$parameters
Bayes_crit_reg_params_sd <- saved_results$parameters_sd



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


load(paste(getwd(),"/results/standard_self_conf_",share_unlabeled,"_",data, "_n=", as.character(n), "_p=", as.character(p), sep=""))
prob_score_params <- saved_results$parameters
prob_score_params_sd <- saved_results$parameters_sd



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


# Combine data into a single data frame
df <- data.frame(
  Iteration = rep(1:length(likelihood_params), 5),
  L2_Norm = c(likelihood_params, Bayes_crit_params, Bayes_crit_reg_params,pred_var_params,prob_score_params),
  "Selection Criterion" = rep(c("Likelihood", "Bayes-Crit", "Bayes-crit-Reg", "Predictive Var","Probability Score"), each = length(likelihood_params))
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

filename = paste("plots/res_plot_params_data=", data,"_share=",share_unlabeled, "_n=", as.character(n), "_p=", as.character(p),".png", sep = "")
ggsave(filename=filename, plot = plot,  dpi = 300)





