packages <- c("MFPCA", "funData")

## Now load or install&load all
package_check <- lapply(
  packages,
  FUN <- function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
#source("00_malnutrition.R")
source("subsetlist.R")
source("bootstrap_mfpca.R")
source("bootstrap_implementation.R")

mal_sparse <- list(com_mal_height, com_mal_weight)

fun_height <- funData::funData(argvals = seq(0, 1, length.out = length(mal_time)), X = com_mal_height)
fun_weight <- funData::funData(argvals = seq(0, 1, length.out = length(mal_time)), X = com_mal_weight)
fun_malnutrition <- multiFunData(fun_height, fun_weight)

malnutrition_no_bootstrap_mfpca <- MFPCA(fun_malnutrition,
                                         M = 5,
                                         uniExpansions = list(list(type = "uFPCA"),
                                                              list(type = "uFPCA")),
                                         weights = rep(1, length(fun_malnutrition)),
                                         fit = TRUE)

for(i in 1:1000) {
  malnutrition_mfpca1 <- try(bootstrap_implementation(bootstrap_times = 30,
                                                     argval = mal_time,
                                                     true_data = NULL,
                                                     sparse_data = mal_sparse, confid_alpha = 0.05))
  if(inherits(malnutrition_mfpca1, "try-error")) {
    #error handling code, maybe just skip this iteration using
    next
  } else if (max(malnutrition_mfpca1$bootstrap_fit[[1]]) < 100 && min(malnutrition_mfpca1$bootstrap_fit[[1]]) > 0) { 
    stop(i)
  } else {
    next
  }
  #rest of iteration for case of no error
}

pdf(file = "fit_mal.pdf", height = 5.2, width = 10.4)
par(mfrow = c(1, 2), mar = c(5, 3, 2, 0.5))
matplot(t(malnutrition_mfpca1$bootstrap_fit[[1]]), type = "l", main = "Fitted Height Malnutrition")
matplot(t(malnutrition_no_bootstrap_mfpca$fit[[1]]@X), type = "l", main = "Fitted Height Malnutrition")
matplot(t(malnutrition_mfpca1$bootstrap_fit[[2]]), type = "l", main = "Fitted Weight Malnutrition")
matplot(t(malnutrition_no_bootstrap_mfpca$fit[[2]]@X), type = "l", main = "Fitted Height Malnutrition")
dev.off()

save(malnutrition_no_bootstrap_mfpca, malnutrition_mfpca1, file = "malnutrition.RData")
