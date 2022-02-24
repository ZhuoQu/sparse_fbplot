packages <- c("parallel", "gdata")

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
source("simul_calculate_depth.R")
confid_alpha <- 0.05
bootstrapTimes <- 40
sparsity <- rep("point", p)
outlierst <- data_type[8]
argval <- sett
simTimes <- 100

trial_different_pcurve <- mclapply(seq(0.1, 0.6, by = 0.1), function(p_c_value) {
  p_curve <- rep(p_c_value, p)
trial_rs_cm <- mclapply(1:simTimes, function(nnr) {
  depthresult <- simul_calculate_depth(outlierst, eigenvalue_type, argval = sett,
                                 p_size, p_curve, sparsity,
                                 confid_alpha, bootstrapTimes)
  spearman_coef <- spearman_stat(depthresult)
  return (spearman_coef)
}, mc.cores = 3)
return (trial_rs_cm)
}, mc.cores = 3)

save(trial_different_pcurve, file = paste(outlierst, "_", sparsity[1],".RData", sep = ""))



