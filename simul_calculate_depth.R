source("one_sample.R")
source("multi_sparse.R")
source("bootstrap_mfpca.R")
source("bootstrap_implementation.R")
source("multi_depth.R")
source("revised_multi_depth.R")
packages <- c("RandomFields")

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
simul_calculate_depth <- function(outlierst, eigenvalue_type, argval,
                         p_size, p_curve, sparsity,
                         confid_alpha, 
                         bootstrapTimes) {
  
  samples <- generate_samples(outlierst, eigenvalue_type, argval)
  true_data <- samples[[1]]
  observed_data <- samples[[2]]
  outlier_index <- samples[[3]]
  sparse_data <- multi_sparse(observed_data, p_size, p_curve, sparsity)

  #################################### Let's draw the first estimation and the confidence interval with two differenet parameters.
  result_bs <- bootstrap_implementation(bootstrapTimes, argval,
                                        true_data, 
                                        #observed_data,
                                        sparse_data, confid_alpha)
  
  true_list <- lapply(1:length(argval), function(k) {
    sapply(true_data, function(l) {l[, k]})
  })
  # observed_list <- lapply(1:length(argval), function(k) {
  #   sapply(observed_data, function(l) {l[, k]})
  # })
  mfpca_list <- lapply(1:length(argval), function(k) {
    sapply(result_bs$full_fit, function(l) {l[, k]})
  })
  bmfpca_list <- lapply(1:length(argval), function(k) {
    sapply(result_bs$bootstrap_fit, function(l) {l[, k]})
  })
  fit_lower_upper <- lapply(1:length(argval), function(k) {
    fit <- sapply(result_bs$bootstrap_fit, function(l) {l[, k]})
    lower <- sapply(result_bs$CI_lower, function(l) {l[, k]})
    upper <- sapply(result_bs$CI_upper, function(l) {l[, k]})
    return (rbind(fit, lower, upper))
    })
  
  depth_true <- multi_depth(true_list, "MFHD")
  
  #depth_observed <- multi_depth(observed_list, "MFHD")
  
  depth_mfpc <- multi_depth(mfpca_list, "MFHD")
  
  depth_bmfpc <- multi_depth(bmfpca_list, "MFHD")
  
  revised_depth_aw <- revised_multi_depth(fit_lower_upper, 
                                          "MFHD", "aw")
  revised_depth_naw <- revised_multi_depth(fit_lower_upper,
                                           "MFHD", "naw")
  revised_depth_dm <- revised_multi_depth(fit_lower_upper, 
                                              "MFHD", "dm")
  # revised_depth_tm <- revised_multi_depth(fit_lower_upper, 
  #                                         "MFHD", "tm")
  return (list(depth_true, depth_mfpc, depth_bmfpc,
               revised_depth_aw, revised_depth_naw,
               revised_depth_dm))
} 
############################# let's see the rank coefficient now!!!

spearman_stat <- function(depth_result) { 
  depth_true <- depth_result[[1]]
  depth_mfpc <- depth_result[[2]]
  depth_bmfpc <- depth_result[[3]]
  revised_depth_aw <- depth_result[[4]]
  revised_depth_naw <- depth_result[[5]]
  revised_depth_dm <- depth_result[[6]]
  
  r0 <- cor(depth_true, depth_mfpc, method = "spearman")
  r1 <- cor(depth_true, depth_bmfpc, method = "spearman")
  r2 <- cor(depth_true, revised_depth_aw, method = "spearman")
  r3 <- cor(depth_true, revised_depth_naw, method = "spearman")
  r4 <- cor(depth_true, revised_depth_dm, method = "spearman")
  return (data.frame(r_mfpc = r0, r_bmfpc = r1, r_d_aw = r2,
                   r_d_naw = r3, r_d_dm = r4))
}
########################################################################################
