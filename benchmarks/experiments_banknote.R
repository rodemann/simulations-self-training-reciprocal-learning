library(dplyr)

set.seed(2037420)

N = 40

share_setups = c(0.7,0.8,0.9)


for (share_unlabeled in share_setups) {
    share_unlabeled %>% print

    
    try(
      source(paste(getwd(),"/benchmarks/real world data/run_benchmarks_banknote.R", sep=""))
    )
 
    #try(
      source(paste(getwd(),"/analyze/analyze_params.R", sep=""))
    #)
    

    
  }


