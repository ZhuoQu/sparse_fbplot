#outlierst <-c("no outlier","persistent magnitude outlier","isolated magnitude outlier","shape outlier I","shape outlier II","covariance outlier,"mixed outlier","joint outlier")
source("simulation_parameter.R")
source("one_sample.R")
source("multi_sparse.R")
source("plotsim_sparse.R")

for (mod in 1:length(data_type)) {
  sim <- generate_samples(data_type[mod], eigenvalue_type, argval)
  p <- length(sim$true_curve)
  sparse_data <- multi_sparse(sim$observed_curve, p_size = rep(1, p), 
                              p_curve = rep(0.4, p), sparsity = c("point", "peak", "partial"))
  pdf(file = paste("plot_model_", mod, ".pdf", sep = ""), width = 5.4 * 3, height = 5.2)
  if (mod == 4 || mod == 8) {
    xlab <- rep("Time", p)
  } else {
    xlab <- rep("", p) 
  }
  ylab <- c("Value", rep("", p - 1))
  plotsim_sparse(sim, sparse_data, argval, 
               xlab = xlab, ylab = ylab, title = sapply(1:p, function(k) {
                 paste("Simulation for Model ", mod, ": Variable ", k, sep = "")}) ) 
  dev.off()
 }  

