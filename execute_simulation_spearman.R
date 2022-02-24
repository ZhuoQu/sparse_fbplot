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
outlierst <- data_type[1]
simTimes <- 100

trial_different_pcurve <- mclapply(seq(0.1, 0.6, by = 0.1), function(p_c_value) {
  p_curve <- rep(p_c_value, p)
trial_rs_cm <- mclapply(1:simTimes, function(nnr) {
  depthresult <- simul_calculate_depth(outlierst, eigenvalue_type, sett,
                                 p_size, p_curve, sparsity,
                                 confid_alpha, bootstrapTimes)
  return (spearman_stat(depthresult))
}, mc.cores = detectCores() - 2)
trial_rs_cm <- matrix(unlist(trial_rs_cm), ncol = 5, byrow = TRUE)
colnames(trial_rs_cm) <- c("r_mfpc", "r_bmfpc", "r_d_aw", "r_d_naw", "r_d_dm")

depth_list_cm <- unmatrix(trial_rs_cm)
trial_bx_cm <- data.frame(value = depth_list_cm, depth = c(rep("'MFHD'['mfpc']",simTimes), 
                                                     rep("'MFHD'['b-mfpc']",simTimes),
                                                     rep("'RMFHD'['aw']",simTimes),
                                                     rep("'RMFHD'['naw']",simTimes),
                                                     rep("'RMFHD'['dm']",simTimes)))
return (trial_bx_cm)
}, mc.cores = detectCores() - 3)

save(trial_different_pcurve, file = paste(outlierst, "_", sparsity[1],".RData", sep = ""))



