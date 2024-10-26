library(dplyr)

set.seed(2037420)

N = 30


share_setups = c(0.7, 0.8)
n_setups = c(200)


for (share_unlabeled in share_setups) {
  for (n in n_setups){
    share_unlabeled %>% print
    n %>% print()

    
    try(
      source(paste(getwd(),"/benchmarks/real world data/run_benchmarks_banknote.R", sep=""))
    )
 
    try(
      source(paste(getwd(),"/analyze/analyze_params.R", sep=""))
    )
    
  }
}

