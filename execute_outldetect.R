packages <- c( "parallel")

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
source("simulation_parameter.R")
source("outl_detection.R")

sparsity <- rep("point", p)
outlierst <- data_type[7]
argval <- sett
simN <- 100

no_case <- mclapply(1:simN, function(SIM) {
  set.seed(SIM)
  result <- outl_detection(outlierst, eigenvalue_type, argval = sett,
                           p_size, p_curve = rep(0, p), sparsity, 
                           bootstrapTimes = 100, confid_alpha = 0.05, sq_vo = FALSE)
  
  return (result)
}, mc.cores = 10)

low_case <- mclapply(1:simN, function(SIM) {
    set.seed(SIM)
    result <- outl_detection(outlierst, eigenvalue_type, argval = sett,
                          p_size, p_curve = rep(0.2, p), sparsity, 
                          bootstrapTimes = 100, confid_alpha = 0.05, sq_vo = FALSE)
    
    return (result)
    }, mc.cores = 10)

median_case <- mclapply(1:simN, function(SIM) {
  set.seed(SIM)
  result <- outl_detection(outlierst, eigenvalue_type, argval = sett,
                           p_size, p_curve = rep(0.4, p), sparsity, 
                           bootstrapTimes = 100, confid_alpha = 0.05, sq_vo = FALSE)
  
  return (result)
}, mc.cores = 10)

high_case <- mclapply(1:simN, function(SIM) {
  set.seed(SIM)
  result <- outl_detection(outlierst, eigenvalue_type, argval = sett,
                           p_size, p_curve = rep(0.6, p), sparsity, 
                           bootstrapTimes = 100, confid_alpha = 0.05, sq_vo = FALSE)
  
  return (result)
}, mc.cores = 10)

result <- list(no = no_case, low = low_case, median = median_case, high = high_case)

newresult <- lapply(result, function(k) {
  mat <- matrix(unlist(k), byrow = TRUE, ncol = 4)
  colnames(mat) <- c("pc_sparse", "pc_twostage", "pf_sparse", "pf_twostage")
  return (mat)
  })

save(newresult, file = paste(outlierst, "_", sparsity[1],"_outlier_detection",".RData", sep = ""))

lapply(newresult, function(k) {
  100 * apply(k, 2, mean)
})

lapply(newresult, function(k) {
  100 * apply(k, 2, sd)
})
