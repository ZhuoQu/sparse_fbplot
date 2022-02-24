packages <- c("MASS", "roahd")

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
source("uni_sparse.R")
source("bootstrap_mfpca.R")
source("sparse_fbplot.R")
source("intensity_sparse_fbplot.R")
gaussprocess <- function(from = 0, to = 1, K = function(s, t) {exp(-abs(s-t))},
                         start = 0, m = 50) {
  # Simulates a Gaussian process with a given kernel
  #
  # args:
  #   from: numeric for the starting location of the sequence
  #   to: numeric for the ending location of the sequence
  #   K: a function that corresponds to the kernel (covariance function) of
  #      the process; must give numeric outputs, and if this won't produce a
  #      positive semi-definite matrix, it could fail; default is a Wiener
  #      process
  #   start: numeric for the starting position of the process
  #   m: positive integer for the number of points in the process to simulate
  #
  # return:
  #   A data.frame with variables "t" for the time index and "xt" for the value
  #   of the process
  
  t <- seq(from = from, to = to, length.out = m)
  Sigma <- sapply(t, function(s1) {
    sapply(t, function(s2) {
      K(s1, s2)
    })
  })
  
  path <- mvrnorm(mu = rep(0, times = m), Sigma = Sigma)
  return(data.frame("t" = t, "xt" = path))
}

uni_sample <- matrix(NA, nrow = 100, ncol = 50)
for (i in 1:100) {
  onesample <- gaussprocess(from = 0, to = 1, K = function(s, t) {exp(-abs(s-t))},
                         start = 0, m = 50)
  uni_sample[i, ] <- onesample$xt
}

depth_0 <- MBD(uni_sample)
sparse_data_0.2 <- uni_sparse(uni_sample, 1, 0.2, "point")
mfpca_0.2 <- bootstrap_mfpca(sett, uni_sample, sparse_data_0.2, isbootstrap = FALSE)
depth_0.2 <- MBD(mfpca_0.2$fit[[1]])

##########################################################
sparse_data_0.6 <- uni_sparse(uni_sample, 1, 0.6, "point")

mfpca_0.6 <- bootstrap_mfpca(sett, uni_sample, sparse_data_0.6, isbootstrap = FALSE)

depth_0.6 <- MBD(mfpca_0.6$fit[[1]])

p <- 1
pdf(file = "uni_org_pc0.pdf", height = 5.2, width = 5.4)
par(mai = c(0.7, 0.8, 0.4, 0.1), mar = c(4.5, 4.2, 2, 1),
    mgp = c(3, 1.5, 0), mfrow = c(1, ifelse(p <= 3, p, 3)))
plot(sett, uni_sample[1, ], ylim = c(-3.1, 3.1), type = "n", 
     xlab = "", ylab = "Value", main = "No Sparseness",
     cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4)
apply(uni_sample, 1, function(k) {lines(sett, k)})
lapply(1:nrow(uni_sample), function(k) {points(sett, uni_sample[k, ], col = 1, cex = 0.4, pch = 1)})
dev.off()

pdf(file = "uni_org_pc2.pdf", height = 5.2, width = 5.4)
par(mai = c(0.7, 0.8, 0.4, 0.1), mar = c(4.5, 4.2, 2, 1),
    mgp = c(3, 1.5, 0), mfrow = c(1, ifelse(p <= 3, p, 3)))
plot(sett, uni_sample[1, ], ylim = c(-3.1, 3.1), type = "n", 
     xlab = "", ylab = "", main = "Medium Sparseness",
     cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4)
lapply(1:nrow(uni_sample), function(k) {lines(sett, uni_sample[k, ], col = "grey", lty = 2)})
# lapply(1:nrow(uni_sample), function(k) {points(sett[is.na(sparse_data_0.2[k, ])], uni_sample[k, is.na(sparse_data_0.2[k, ])], 
#                                                cex = 0.4, pch = 16, col = "grey")})
lapply(1:nrow(uni_sample), function(k) {lines(sett, sparse_data_0.2[k, ], col = 1)})
lapply(1:nrow(uni_sample), function(k) {points(sett, sparse_data_0.2[k, ], col = 1, cex = 0.4, pch = 1)})
dev.off()

pdf(file = "uni_org_pc6.pdf", height = 5.2, width = 5.4)
par(mai = c(0.7, 0.8, 0.4, 0.1), mar = c(4.5, 4.2, 2, 1),
    mgp = c(3, 1.5, 0), mfrow = c(1, ifelse(p <= 3, p, 3)))
plot(sett, uni_sample[1, ], ylim = c(-3.1, 3.1), type = "n", 
     xlab = "", ylab = "", main = "High Sparseness",
     cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4)
lapply(1:nrow(uni_sample), function(k) {lines(sett, uni_sample[k, ], col = "grey", lty = 2)})
lapply(1:nrow(uni_sample), function(k) {lines(sett, sparse_data_0.6[k, ], col = 1)})
lapply(1:nrow(uni_sample), function(k) {points(sett, sparse_data_0.6[k, ], col = 1, cex = 0.4, pch = 1)})
# lapply(1:nrow(uni_sample), function(k) {points(sett[is.na(sparse_data_0.6[k, ])], uni_sample[k, is.na(sparse_data_0.6[k, ])], 
#                                                cex = 0.4, pch = 16, col = "grey")})
dev.off()



pdf(file = "uni_sp_fbplot_pc0.pdf", height = 5.2, width = 5.4)
sp_0 <- sparse_fbplot(uni_sample, uni_sample, time_index = NULL, 
              depth = depth_0,
              two_stage = FALSE, sq_vo = FALSE, plot = TRUE, xlab = "",
              ylab = "Value", title = "Sparse Functional Boxplot",
              yrange = c(-3.1, 3.1),
              cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3,
              medlabel = FALSE, outlabel = FALSE,  
              prob = 0.5, factor = 1.5,
              color = 6, outliercolor.fb = 2, barcol = 4,
              outliercolor.dir = 3, fullout = FALSE) ## if remove is given, then we use MS plot to detect outliers already
dev.off()

pdf(file = "uni_sp_fbplot_pc2.pdf", height = 5.2, width = 5.4)
sparse_fbplot(mfpca_0.2$fit[[1]], sparse_data_0.2, time_index = NULL, 
              depth = depth_0.2,
              two_stage = FALSE, sq_vo = FALSE, plot = TRUE,
              xlab = "", ylab = "",
              title = "Sparse Functional Boxplot",
              yrange = c(-3.1, 3.1),
              cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3,
              medlabel = FALSE, outlabel = FALSE,  
              prob = 0.5, factor = 1.5,
              color = 6, outliercolor.fb = 2, barcol = 4,
              outliercolor.dir = 3, fullout = FALSE) ## if remove is given, then we use MS plot to detect outliers already
dev.off()

pdf(file = "uni_sp_fbplot_pc6.pdf", height = 5.2, width = 5.4)
sparse_fbplot(mfpca_0.6$fit[[1]], sparse_data_0.6, time_index = NULL, 
              depth = depth_0.6,
              two_stage = FALSE, sq_vo = FALSE, plot = TRUE,
              xlab = "", ylab = "",
              title = "Sparse Functional Boxplot",
              yrange = c(-3.1, 3.1),
              cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3,
              medlabel = FALSE, outlabel = FALSE,  
              prob = 0.5, factor = 1.5,
              color = 6, outliercolor.fb = 2, barcol = 4,
              outliercolor.dir = 3, fullout = FALSE) ## if remove is given, then we use MS plot to detect outliers already
dev.off()


pdf(file = "uni_itsp_fbplot_pc0.pdf", height = 5.2, width = 5.4)
int1 <- intensity_sparse_fbplot(uni_sample, uni_sample, time_index = NULL, 
                              depth = depth_0,
                              two_stage = FALSE, sq_vo = FALSE, plot = TRUE, 
                              xlab = "Time", ylab = "Value", title = "Intensity Sparse Functional Boxplot",
                              yrange = c(-3.1, 3.1),
                              cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3,
                              medlabel = FALSE, outlabel = FALSE,  
                              prob = 0.5, factor = 1.5,
                              color = 6, barcol = 4,
                              outliercolor.fb = 2, colorrange = NULL,
                              outliercolor.dir = 3, fullout = FALSE,
                              colorsplit = 40, 
                              showcontour = TRUE, drawlabels = FALSE,
                              showlegend = FALSE) ## if remove is given, then we use MS plot to detect outliers already
dev.off()


pdf(file = "uni_itsp_fbplot_pc2.pdf", height = 5.2, width = 5.4)
int2 <- intensity_sparse_fbplot(mfpca_0.2$fit[[1]], sparse_data_0.2, time_index = NULL, 
                              depth = depth_0.2,
                              two_stage = FALSE, sq_vo = FALSE, plot = TRUE, 
                              xlab = "Time", ylab = "", 
                              title = "Intensity Sparse Functional Boxplot",
                              yrange = c(-3.1, 3.1),
                              cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3,
                              medlabel = FALSE, outlabel = FALSE,  
                              prob = 0.5, factor = 1.5,
                              color = 6, barcol = 4,
                              outliercolor.fb = 2, colorrange = NULL,
                              outliercolor.dir = 3, fullout = FALSE,
                              colorsplit = 40, 
                              showcontour = TRUE, drawlabels = FALSE,
                              showlegend = FALSE) ## if remove is given, then we use MS plot to detect outliers already
dev.off()

pdf(file = "uni_itsp_fbplot_pc6.pdf", height = 5.2, width = 5.5)
int3 <- intensity_sparse_fbplot(mfpca_0.6$fit[[1]], sparse_data_0.6, time_index = NULL, 
                              depth = depth_0.6,
                              two_stage = FALSE, sq_vo = FALSE, plot = TRUE, 
                              xlab = "Time", ylab = "", 
                              title = "Intensity Sparse Functional Boxplot",
                              yrange = c(-3.1, 3.1),
                              cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3,
                              medlabel = FALSE, outlabel = FALSE,  
                              prob = 0.5, factor = 1.5,
                              color = 6, barcol = 4,
                              outliercolor.fb = 2, colorrange = NULL,
                              outliercolor.dir = 3, fullout = FALSE,
                              colorsplit = 40, 
                              showcontour = TRUE, drawlabels = FALSE,
                              showlegend = TRUE) ## if remove is given, then we use MS plot to detect outliers already
dev.off()


colr <- c(0, max(int2$colorrange[[1]],int3$colorrange[[1]]))


intensity_sparse_fbplot(uni_sample, uni_sample, time_index = NULL, 
                        depth = depth_0,
                        two_stage = FALSE, sq_vo = FALSE, plot = TRUE, 
                        xlab = "Time", ylab = "Value", title = "Intensity Sparse Functional Boxplot",
                        yrange = c(-3.1, 3.1),
                        cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3,
                        medlabel = FALSE, outlabel = FALSE,  
                        prob = 0.5, factor = 1.5,
                        color = 6, barcol = 4,
                        outliercolor.fb = 2, colorrange = colr,
                        outliercolor.dir = 3, fullout = FALSE,
                        colorsplit = 40, 
                        showcontour = TRUE, drawlabels = FALSE,
                        showlegend = FALSE) ## if remove is given, then we use MS plot to detect outliers already

pdf(file = "uni_relative_itsp_fbplot_pc2.pdf", height = 5.2, width = 5.4)
int2 <- intensity_sparse_fbplot(mfpca_0.2$fit[[1]], sparse_data_0.2, time_index = NULL, 
                                                        depth = depth_0.2,
                                                        two_stage = FALSE, sq_vo = FALSE, plot = TRUE, 
                                                        xlab = "Time", ylab = "", title = "Intensity Sparse Functional Boxplot",
                                                        yrange = c(-3.1, 3.1),
                                                        cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3,
                                                        medlabel = FALSE, outlabel = FALSE,  
                                                        prob = 0.5, factor = 1.5,
                                                        color = 6, barcol = 4,
                                                        outliercolor.fb = 2, colorrange = colr,
                                                        outliercolor.dir = 3, fullout = FALSE,
                                                        colorsplit = 40, 
                                                        showcontour = TRUE, drawlabels = FALSE,
                                                        showlegend = FALSE) ## if remove is given, then we use MS plot to detect outliers already
dev.off()

pdf(file = "uni_relative_itsp_fbplot_pc6.pdf", height = 5.2, width = 5.8)
int3 <- intensity_sparse_fbplot(mfpca_0.6$fit[[1]], sparse_data_0.6, time_index = NULL, 
                        depth = depth_0.6,
                        two_stage = FALSE, sq_vo = FALSE, plot = TRUE, 
                        xlab = "Time", ylab = "", title = "Intensity Sparse Functional Boxplot",
                        yrange = c(-3.1, 3.1),
                        cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3,
                        medlabel = FALSE, outlabel = FALSE,  
                        prob = 0.5, factor = 1.5,
                        color = 6, barcol = 4,
                        outliercolor.fb = 2, colorrange = colr,
                        outliercolor.dir = 3, fullout = FALSE,
                        colorsplit = 40, 
                        showcontour = TRUE, drawlabels = FALSE,
                        showlegend = TRUE) ## if remove is given, then we use MS plot to detect outliers already
dev.off()
