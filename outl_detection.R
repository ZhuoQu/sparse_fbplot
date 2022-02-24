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
source("one_sample.R")
source("multi_sparse.R")
source("multi_depth.R")
source("bootstrap_mfpca.R")
source("bootstrap_implementation.R")
source("sparse_fbplot.R")


eigenvalue_type <- "linear"

outl_detection <- function(outlierst, eigenvalue_type, argval,
                           p_size, p_curve, sparsity, 
                           bootstrapTimes = 100, confid_alpha = 0.05, sq_vo = FALSE) {
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
  
  bmfpca_list <- lapply(1:length(argval), function(k) {
    sapply(result_bs$bootstrap_fit, function(l) {l[, k]})
  })
  
  depth_bmfpc <- multi_depth(bmfpca_list, "MFHD")
  
  sp <- sparse_fbplot(fit = result_bs$bootstrap_fit, 
                      sparse = sparse_data, 
                      time_index = NULL, depth = depth_bmfpc,
                      two_stage = FALSE, sq_vo = sq_vo, plot = FALSE,
                      xlab = NULL, ylab = NULL, title = NULL,
                      yrange = NULL,
                      cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3,
                      medlabel = TRUE, outlabel = TRUE,
                      prob = 0.5, factor = 1.5,
                      color = 6, outliercolor.fb = 2, barcol = 4,
                      outliercolor.dir = 3, fullout = FALSE
                      )
  
  outlier_sp <- unique(unlist(sp$fb_outlier_index))
  pc_sp <- length(intersect(outlier_sp, outlier_index))/ length(outlier_index)
  pf_sp <- length(setdiff(outlier_sp, outlier_index))/ (nrow(true_data[[1]]) - length(outlier_index))
  
  sp_twostage <- sparse_fbplot(fit = result_bs$bootstrap_fit, 
                               sparse = sparse_data, 
                               time_index = NULL, depth = depth_bmfpc,
                               two_stage = TRUE, sq_vo = FALSE, plot = FALSE,
                               xlab = NULL, ylab = NULL, title = NULL,
                               yrange = NULL,
                               cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3,
                               medlabel = TRUE, outlabel = TRUE,
                               prob = 0.5, factor = 1.5,
                               color = 6, outliercolor.fb = 2, barcol = 4,
                               outliercolor.dir = 3, fullout = FALSE
                               )
  outlier_sp_ts <- union(unlist(sp_twostage$fb_outlier_index), sp_twostage$dir_outlier_index)
  pc_sp_ts <- length(intersect(outlier_sp_ts, outlier_index)) / length(outlier_index)
  pf_sp_ts <- length(setdiff(outlier_sp_ts, outlier_index)) / (nrow(true_data[[1]]) - length(outlier_index))
  
  result <- data.frame(pc_sparse = pc_sp,
                       pc_twostage = pc_sp_ts,
                       pf_sparse = pf_sp,
                       pf_twostage = pf_sp_ts)
  return (result)
}

