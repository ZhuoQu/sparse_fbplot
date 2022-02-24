#source("bootstrap_mfpca.R")
packages <- c("scatterplot3d")

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

plotsim_sparse <- function(sim, sparse_data, argval, 
                           xlab, ylab, title) {
  true_curve <- sim[[1]]
  observed_curve <- sim[[2]]
  out_index <- sim[[3]]
  
  #complete_result <- bootstrap_mfpca(30, sparsify, argval)
  par(mai = c(0.7, 0.8, 0.4, 0.1), mar = c(4.5, 4.2, 2.5, 1),
      mgp = c(3, 1.5, 0), mfrow = c(1, ifelse(p <= 3, p, 3)))
  for (j in 1:p) {
    sam <- observed_curve[[j]]
    sp <- sparse_data[[j]]
    plot(argval, sam[1, ], type = "n", 
         xlab = xlab[j], ylab = ylab[j], 
         cex.axis = 1.2, ylim = range(observed_curve), 
         cex.lab = 1.8, cex.main = 2, 
         main = title[j])
    apply(sam, 1, function(i) {
      lines(argval, i, col = "grey", lty = 2)
    })
    
    apply(sp[setdiff(1:n, out_index), ], 1,
         function(i) {
           lines(argval, i)})
    
    apply(sp, 1, function(i) {
      points(argval, i, cex = 0.4, pch = 1)}) ## pure
    if (length(out_index) >= 1) {
      apply(sp[out_index, ], 1, function(i) {
        lines(argval, i, col = "red")}) ## pure
    }
  }
  # plot_3d <- scatterplot3d(observed_curve[[1]][1, ], observed_curve[[2]][1, ], observed_curve[[3]][1, ],
  #                          type = "n", xlim = range(observed_curve[[1]]), ylim = range(observed_curve[[2]]),
  #                          zlim = range(observed_curve[[3]]),
  #               xlab = "Variable 1", ylab = "Variable 2", zlab = "Variable 3", main = "Three-dimensional Data")
  # lapply(1:n, function(k) {
  #   plot_3d$points3d(observed_curve[[1]][k, ], observed_curve[[2]][k, ], observed_curve[[3]][k, ],
  #                    type = "l", col = "grey", lty = 2)
  # })
  # lapply(setdiff(1:n, out_index), function(k) {
  #   data_transform <- data.frame(sparse_data[[1]][k, ], sparse_data[[2]][k, ], sparse_data[[3]][k, ])
  #   plot_3d$points3d(data_transform, type = "l")
  # })
  # lapply(1:n, function(k) {
  #   plot_3d$points3d(sparse_data[[1]][k, ], sparse_data[[2]][k, ], sparse_data[[3]][k, ],
  #                    type = "p", cex = 0.4, pch = 1)
  # })
  # lapply(out_index, function(k) {
  #   data_transform <- data.frame(sparse_data[[1]][k, ], sparse_data[[2]][k, ], sparse_data[[3]][k, ])
  #   plot_3d$points3d(data_transform, type = "l", col = "red")
  # })
  # lapply(out_index, function(k) {
  #   data_transform <- data.frame(observed_curve[[1]][k, ], observed_curve[[2]][k, ], observed_curve[[3]][k, ])
  #   plot_3d$points3d(data_transform, type = "l", col = "red")
  # })
  invisible(sim)
}



