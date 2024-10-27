###############
## global setup
###############
library(dplyr)
library(MixGHD)
#N = 100
#share_unlabeled = 0.8
p = 3
n = 200
# read in data frame
data(banknote)
data_frame <- banknote %>% as.data.frame()
names(data_frame)[names(data_frame) == 'Status'] <- 'target'

#get number of instances
data_frame = data_frame[sample(nrow(data_frame), n),]


#train test splict
n_test = nrow(data_frame)*0.5
n_test = round(n_test)


name_df = "banknote" # for results 
data = "banknote"
# formula for glm
formula = target ~1 + Diagonal+ Bottom + Length #Length + Left + Right + Bottom + Top + Diagonal    
target = "target" 
data_frame[c(target)] <- data_frame[c(target)] %>% unlist() %>% as.factor()
levels_present <- levels(data_frame[c(target)] %>% unlist())
# check whether labels are suited for replacement by 0,1
levels_present
levels(data_frame[, which(names(data_frame) %in% target)]) <- c(0,1)

glm(formula = formula, data = data_frame, family = "binomial") %>% summary

# multi model
formula_alt1 =  target ~1 + Diagonal+ Bottom
formula_alt2 =  target ~1 + Diagonal + Length
formula_alt3 =  target ~1 + Bottom + Length
formula_alt4 =  target ~1 + Length
formula_alt5 =  target ~1 + Bottom 
formula_alt6 =  target ~1 + Diagonal

cor(data_frame[,-1])



formula_list = list(formula, formula_alt1, formula_alt2, formula_alt3)#, formula_alt4, formula_alt5, formula_alt6)

##########################
# source experiments files
##########################
path_to_experiments = paste(getwd(),"/benchmarks/experiments", sep = "")
# sequential sourcing
# miceadds::source.all(path_to_experiments) 


# parallel sourcing
files_to_source = list.files(path_to_experiments, pattern="*.R",
                             full.names = TRUE)

num_cores <- parallel::detectCores() - 1
comp_clusters <- parallel::makeCluster(num_cores) # parallelize experiments
doParallel::registerDoParallel(comp_clusters)
object_vec = c("N", "share_unlabeled", "data_frame", "name_df", "formula", "formula_list", "target", "p", "n_test")
env <- environment()
parallel::clusterExport(cl=comp_clusters, varlist = object_vec, envir = env)
parallel::parSapply(comp_clusters, files_to_source, source)

parallel::stopCluster(comp_clusters)



