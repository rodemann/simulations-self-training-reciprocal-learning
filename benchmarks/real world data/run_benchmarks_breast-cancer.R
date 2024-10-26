###############
## global setup
###############
library(dplyr)
library(MixGHD)
#N = 40
#share_unlabeled = 0.8
p = 10
#n = 400

# read in data frame

UCI_data_URL <- RCurl::getURL('https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data')
names <- c('id_number', 'diagnosis', 'radius_mean', 
           'texture_mean', 'perimeter_mean', 'area_mean', 
           'smoothness_mean', 'compactness_mean', 
           'concavity_mean','concave_points_mean', 
           'symmetry_mean', 'fractal_dimension_mean',
           'radius_se', 'texture_se', 'perimeter_se', 
           'area_se', 'smoothness_se', 'compactness_se', 
           'concavity_se', 'concave_points_se', 
           'symmetry_se', 'fractal_dimension_se', 
           'radius_worst', 'texture_worst', 
           'perimeter_worst', 'area_worst', 
           'smoothness_worst', 'compactness_worst', 
           'concavity_worst', 'concave_points_worst', 
           'symmetry_worst', 'fractal_dimension_worst')
breast_cancer <- read.table(textConnection(UCI_data_URL), sep = ',', col.names = names)

breast_cancer$id_number <- NULL


data_frame <- breast_cancer %>% as.data.frame()
names(data_frame)[names(data_frame) == 'diagnosis'] <- 'target'

#MCAR
data_frame = na.omit(data_frame)


#get number of instances
data_frame = data_frame[sample(nrow(data_frame), n),]



#train test splict
n_test = nrow(data_frame)*0.5
n_test = round(n_test)


name_df = "cancer" # for results 
data = "cancer"


# formula for glm
vars <- c("target ~")
for (v in 2:p) {
  vars <- c(vars, colnames(data_frame)[v])
}
#vars = c(vars, "s(radius_mean)")

formula = paste(vars, collapse=" + ") %>% as.formula()
formula

target = "target" 
data_frame[c(target)] <- data_frame[c(target)] %>% unlist() %>% as.factor()
levels_present <- levels(data_frame[c(target)] %>% unlist())
# check whether labels are suited for replacement by 0,1
levels_present
levels(data_frame[, which(names(data_frame) %in% target)]) <- c(0,1)

glm(formula = formula, data = data_frame, family = "binomial") %>% summary

formula_list = list(formula)

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



