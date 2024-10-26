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
n_methods = 6
n_test = nrow(data_frame)*0.5
n_test = round(n_test)
#number of unlabeled obs
n_imp = ((n - n_test) * share_unlabeled) %>% round()


#df_onthefly <- matrix(nrow = 3, ncol = n_imp)
onthefly_acc_paths <- data.frame("iter" = 1:n_imp, 
                      "Upper CB" = 1:n_imp, 
                      "Lower CB" = 1:n_imp,
                      "Mean Accuracy" = 1:n_imp,
                      "Method" = 1:n_imp)

onthefly_acc_paths_all = data.frame()

df <- matrix(nrow = n_methods, ncol = 7)



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
onthefly_acc_paths[1:n_imp,"iter"] <- 1:n_imp
onthefly_acc_paths[1:n_imp,"Upper.CB"] <- saved_results$`Inductive on-the-fly CI`[2,]
onthefly_acc_paths[1:n_imp,"Lower.CB"] <- saved_results$`Inductive on-the-fly CI`[1,]
onthefly_acc_paths[1:n_imp,"Mean.Accuracy"] <- saved_results$`Inductive on-the-fly mean`
onthefly_acc_paths[1:n_imp,"Method"] <- "Likelihood (max-max)"
saved_results <- saved_results[-c(1,2)]
#df[1,] <- saved_results %>% unlist()
onthefly_acc_paths_all <- rbind(onthefly_acc_paths_all, onthefly_acc_paths)


load(paste(getwd(),"/results/diff_marg_likelihood_pred_ext_",share_unlabeled,"_",data, "_n=", as.character(n), "_p=", as.character(p), sep=""))
onthefly_acc_paths[1:n_imp,"iter"] <- 1:n_imp
onthefly_acc_paths[1:n_imp,"Upper.CB"] <- saved_results$`Inductive on-the-fly CI`[2,]
onthefly_acc_paths[1:n_imp,"Lower.CB"] <- saved_results$`Inductive on-the-fly CI`[1,]
onthefly_acc_paths[1:n_imp,"Mean.Accuracy"] <- saved_results$`Inductive on-the-fly mean`
onthefly_acc_paths[1:n_imp,"Method"] <- "PPP (bayes-optimal)"
saved_results <- saved_results[-c(1,2)]
#df[2,] <- saved_results %>% unlist()
onthefly_acc_paths_all <- rbind(onthefly_acc_paths_all, onthefly_acc_paths)


load(paste(getwd(),"/results/diff_marg_likelihood_pred_reg_",share_unlabeled,"_",data, "_n=", as.character(n), "_p=", as.character(p), sep=""))
onthefly_acc_paths[1:n_imp,"iter"] <- 1:n_imp
onthefly_acc_paths[1:n_imp,"Upper.CB"] <- saved_results$`Inductive on-the-fly CI`[2,]
onthefly_acc_paths[1:n_imp,"Lower.CB"] <- saved_results$`Inductive on-the-fly CI`[1,]
onthefly_acc_paths[1:n_imp,"Mean.Accuracy"] <- saved_results$`Inductive on-the-fly mean`
onthefly_acc_paths[1:n_imp,"Method"] <- "PPP (regularized)"
saved_results <- saved_results[-c(1,2)]
#df[2,] <- saved_results %>% unlist()
onthefly_acc_paths_all <- rbind(onthefly_acc_paths_all, onthefly_acc_paths)


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
onthefly_acc_paths[1:n_imp,"iter"] <- 1:n_imp
onthefly_acc_paths[1:n_imp,"Upper.CB"] <- saved_results$`Inductive on-the-fly CI`[2,]
onthefly_acc_paths[1:n_imp,"Lower.CB"] <- saved_results$`Inductive on-the-fly CI`[1,]
onthefly_acc_paths[1:n_imp,"Mean.Accuracy"] <- saved_results$`Inductive on-the-fly mean`
onthefly_acc_paths[1:n_imp,"Method"] <- "Predictive Variance"
saved_results <- saved_results[-c(1,2)]
#df[3,] <- saved_results %>% unlist()
onthefly_acc_paths_all <- rbind(onthefly_acc_paths_all, onthefly_acc_paths)

load(paste(getwd(),"/results/standard_self_conf_",share_unlabeled,"_",data, "_n=", as.character(n), "_p=", as.character(p), sep=""))
onthefly_acc_paths[1:n_imp,"iter"] <- 1:n_imp
onthefly_acc_paths[1:n_imp,"Upper.CB"] <- saved_results$`Inductive on-the-fly CI`[2,]
onthefly_acc_paths[1:n_imp,"Lower.CB"] <- saved_results$`Inductive on-the-fly CI`[1,]
onthefly_acc_paths[1:n_imp,"Mean.Accuracy"] <- saved_results$`Inductive on-the-fly mean`
onthefly_acc_paths[1:n_imp,"Method"] <- "Probability Score"
saved_results <- saved_results[-c(1,2)]
#df[4,] <- saved_results %>% unlist()
onthefly_acc_paths_all <- rbind(onthefly_acc_paths_all, onthefly_acc_paths)


load(paste(getwd(),"/results/standard_supervised_",share_unlabeled,"_",data, "_n=", as.character(n), "_p=", as.character(p), sep=""))
#df[5,] <- saved_results %>% unlist()
onthefly_acc_paths[1:n_imp,"iter"] <- 1:n_imp
onthefly_acc_paths[1:n_imp,"Upper.CB"] <- saved_results$`Inductive CI`[2]/saved_results$`Inductive n_test`
onthefly_acc_paths[1:n_imp,"Lower.CB"] <- saved_results$`Inductive CI`[1]/saved_results$`Inductive n_test`
onthefly_acc_paths[1:n_imp,"Mean.Accuracy"] <- mean(c(saved_results$`Inductive CI`[1], saved_results$`Inductive CI`[2]))/saved_results$`Inductive n_test`
onthefly_acc_paths[1:n_imp,"Method"] <- "Supervised Learning"
onthefly_acc_paths_all <- rbind(onthefly_acc_paths_all, onthefly_acc_paths)

# load(paste(getwd(),"/results/standard_supervised_pen_",share_unlabeled,"_",data, "_n=", as.character(n), "_p=", as.character(p), sep=""))
# df[7,] <- saved_results %>% unlist()
# onthefly_acc_paths[1:n_imp,"iter"] <- 1:n_imp
# onthefly_acc_paths[1:n_imp,"Upper.CB"] <- saved_results$`Inductive CI`[2]/saved_results$`Inductive n_test`
# onthefly_acc_paths[1:n_imp,"Lower.CB"] <- saved_results$`Inductive CI`[1]/saved_results$`Inductive n_test`
# onthefly_acc_paths[1:n_imp,"Mean.Accuracy"] <- mean(c(saved_results$`Inductive CI`[1], saved_results$`Inductive CI`[2]))/saved_results$`Inductive n_test`
# onthefly_acc_paths[1:n_imp,"Method"] <- "standard_supervised_pen"
# onthefly_acc_paths_all <- rbind(onthefly_acc_paths_all, onthefly_acc_paths)
# 
# 
# 
# 
# df = df %>% as.data.frame()
# df[,1:7] <- df[,1:7] %>% unlist() %>% as.numeric()
# colnames(df) <- c("Lower_CI_ind", "Upper_CI_ind", "n_ind", "Lower_CI_trans", "Upper_CI_trans", "n_trans", "method")
# df$Lower_CI_ind <- df$Lower_CI_ind/df$n_ind
# df$Upper_CI_ind <- df$Upper_CI_ind/df$n_ind
# df$Lower_CI_trans <- df$Lower_CI_trans/df$n_trans
# df$Upper_CI_trans <- df$Upper_CI_trans/df$n_trans
# 
# df$mean_ind <- rowMeans(cbind(df$Upper_CI_ind, df$Lower_CI_ind))
# df$mean_trans <- rowMeans(cbind(df$Upper_CI_trans, df$Lower_CI_trans))
# #readability
# df$method <- gsub('diff_marg_likelihood', 'dml', df$method)
# 
# #df = df[-c(1),]
# 
# # Basic error bar
# ggplot2::ggplot(df) +
#   geom_bar( aes(x=method, y=mean_ind), stat="identity", fill="skyblue", alpha=0.6) +
#   geom_errorbar( aes(x=method, ymin=Lower_CI_ind, ymax=Upper_CI_ind), width=0.4, colour="orange", alpha=0.9, size=1.3) +
#   ylab("Inductive accuracy") +
#   xlab("Self-training method") +
#   ylim(0,1)
# 
# # 
# # Basic error bar transductive
# ggplot2::ggplot(df) +
#   geom_bar( aes(x=method, y=mean_trans), stat="identity", fill="magenta", alpha=0.7) +
#   geom_errorbar( aes(x=method, ymin=Lower_CI_trans, ymax=Upper_CI_trans), width=0.4, colour="orange", alpha=0.9, size=1.3) +
#   ylab("Transuctive accuracy") +
#   xlab("Self-training method") +
#   ylim(0,1)
# selection of color palettes
safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#117733", "#AA4499", "#332288",
                             "#44AA99", "#999933", "#882255")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
optimum_col = "#ff00f7"


description_char = paste("data:", data, " | ", "setup:"," n=", as.character(n),
                         ", p=", as.character(p), ", share of unlabeled: ", share_unlabeled,  sep = "")

# data = "Breast Cancer (n = 400, p = 30)"
# data = "Mushrooms (n = 500, p = 3)"
# data = "EEG (n = 182, p = 13)"
# data = "Cars (n = 32, p = 3)"
# data = "Sonar (n = 208, p = 60)"
# data = "Ionosphere (n = 350, p = 33)"
# data = "Banknote (n = 200, p = 7)"
# data = "Abalone (n = 400, p = 4)"
#data = "Simulated (n = 100, p = 60)"


# plot for confidence intervalls on the fly
plot <- ggplot(data = onthefly_acc_paths_all, aes(x = iter, group = Method)) +
  geom_point(data = onthefly_acc_paths_all,aes(x = iter, y = Mean.Accuracy, colour = Method))  +
  geom_line(data = onthefly_acc_paths_all, aes(x = iter, y = Mean.Accuracy, colour = Method)) +
  labs(color = "PLS Method") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
  xlab(data)

pal <- safe_colorblind_palette

#plot <- plot + scale_color_discrete(name = "Kernel", labels = kernel_names)
plot <- plot + theme(panel.background = element_rect(fill = "grey28")) +
  theme(legend.position="right") +
  scale_color_manual(values=pal) #+
  #facet_wrap(vars(Method))

# plot_no_CIs <- annotate_figure(plot,
#                 top = text_grob(description_char, face = "bold"),
#                 bottom = text_grob(paste("generated", Sys.time()), face = "bold"))
# plot_no_CIs

plot

# Define your plot with the Economist theme
plot <- ggplot(data = onthefly_acc_paths_all, aes(x = iter, group = Method)) +
  geom_point(aes(x = iter, y = Mean.Accuracy, colour = Method)) +
  geom_line(aes(x = iter, y = Mean.Accuracy, colour = Method)) +
  labs(color = "PLS Method") +
  ggthemes::theme_economist() + 
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=14, face="bold")) +
  xlab(data) +
  scale_color_manual(values=safe_colorblind_palette) +
  theme(legend.position="right")

# Display the plot
print(plot)



filename = paste("plots/res_plot_data=", data,"_share=",share_unlabeled, "_n=", as.character(n), "_p=", as.character(p),".png", sep = "")
ggsave(filename=filename, plot = plot,  dpi = 300)




# # with CIs
# plot_CIs = plot + geom_errorbar(data = onthefly_acc_paths_all, aes(x = iter, ymin = Lower.CB, ymax = Upper.CB, colour = Method))
# plot_CIs <- annotate_figure(plot_CIs,
#                             top = text_grob(description_char, face = "bold"),
#                             bottom = text_grob(paste("generated", Sys.time()), face = "bold"))
# 
# plot_CIs
# 
# filename = paste("plots/res_plot_CIs_data=", data,"_share=",share_unlabeled, "_n=", as.character(n), "_p=", as.character(p),".png", sep = "")
# ggsave(filename=filename, plot = plot_CIs, dpi = 300)
# 
# 
# 




