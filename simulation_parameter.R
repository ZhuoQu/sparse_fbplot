########### simulation data setting
sett <- seq(0, 1, length.out = 50)
n <- 100
p <- 3 ### number of variables
M <- 9
eigenvalue_type <- "linear"
#########outlier setting#####################

contamination_level <- 0.1
# method <- "MFPCA"
data_type <- c("no outlier", "persistent magnitude outlier",
              "isolated magnitude outlier", "shape outlier I",
              "shape outlier II", "mixed outlier", "joint outlier",
              "covariance outlier")

############ outlier setting#############     

############ measurement error #############   
rho_cor <- matrix(1, nrow = p, ncol = p)
for (i in 1:(p - 1)) {
  for (j in (i + 1):p) {
    rho_cor[i, j] <- (i + j) / (i + j + 3) 
    rho_cor[j, i] <- rho_cor[i, j]
  }
}
std <- runif (p, sqrt(0.3), sqrt(0.5))

############ measurement error #############

########### sparseness setting###############
sparse_ls <- c("density", "point", "peak", "partial")
p_size <- rep(1, 3)
p_curve <- rep(0.5, 3)
sparsity <- c("point", "peak", "partial")

###########sparseness setting#################


###########Data Fitting######################

bootstrapTimes <- 100

##########Data Fitting########################