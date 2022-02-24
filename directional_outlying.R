packages <- c("fda.usc", "MASS", "rlist")

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
source("faccal_num.R")

outlying_depth <- function(data_mat, depth.dir) {
  ## data_mat is a matrix with nobs rows and var_nb columns
  if (depth.dir == "RP") {
    out <- 1 / mdepth.RP(data_mat, proj = 200)$dep - 1
  } else if (depth.dir == "MhD") {
    out <- 1 / mdepth.MhD(data_mat)$dep - 1
  } else if (depth.dir == "SD") {
    out <- 1 / mdepth.SD(data_mat)$dep - 1
  } else if (depth.dir == "HS") {
    out <- 1 / mdepth.HS(data_mat)$dep - 1
  }
  return (out)
}

directional_outlying <- function(data, diroutmatrix = FALSE,
                   depth.dir = "RP", d_value = TRUE, sq_vo) {
  #####################
  ## assume that data is a list of variable p
  ## each list is a nobs * length(t) matrix
  #Univariate cases   #
  #####################
  if ("list" %in% class(data)) {
    mat <- data[[1]]
  } else {
    mat <- data
  }
  time_nb <- ncol(mat)
  obs_nb <- nrow(mat)
  
  if ("matrix" %in% class(data) ) {
    medvec <- apply(mat, 1, median)
    madvec <- apply(mat, 1, mad)
    outmat <- abs((mat - medvec) / (madvec))
    signmat <- sign((mat - medvec))
    dirout <- outmat * signmat
    out_avr <- apply(dirout, 1, FUN = function(y) mean(y, na.rm = TRUE))
    out_var <- apply(dirout, 1, FUN = function(y) var(y, na.rm = TRUE))
  }
  #####################
  #Multivariate cases #
  #####################
  if ("list" %in% class(data)) {
    dirout <- array(0, dim = c(obs_nb, time_nb, length(data)))
    for (j in 1:time_nb) {
      data_mat <- sapply(data, function(mm) {mm[, j]})
      out <- outlying_depth(data_mat, depth.dir)
      me <- data_mat[order(out)[1], ]
      dir <- t(apply(data_mat, 1, function(jm) {
        (jm - me) / (sum((jm - me)^2))^(1 / 2)
      }))
      dir[order(out)[1], ] <- 0
      for (i in 1:obs_nb) {
        dirout[i, j, ] <- out[i] * dir[i, ]
      }
    }
    out_avr <- apply(dirout, c(1, 3), FUN = function(y) mean(y, na.rm = TRUE))
    norm_dif <- sapply(1:time_nb, function(kk) {
      apply(dirout[, kk, ] - out_avr, 1, function(w) {
        sum(w^2)})
    })
    out_var <- apply(norm_dif, 1, mean)
  }
  if (sq_vo == TRUE) {
    out_var <- sqrt(out_var)
  }
  result <- list(out_avr = out_avr, out_var = out_var)
  if (diroutmatrix) {
    result <- list.append(result, dirout = dirout)
  } 
  if (d_value) {
    matr <- cbind(out_avr, out_var)
    ans <- cov.rob(matr, method = "mcd", nsamp = "best")
    cov <- ans$cov
    me <- ans$center
    mhd <- mahalanobis(matr, me, cov)
    result <- list.append(result, d_value = mhd, cov = cov)
  }
  return(result)
}

outlier_dirout <- function(data, sq_vo) {

  result <- directional_outlying(data, diroutmatrix = TRUE,
                                 depth.dir = "RP", d_value = TRUE, sq_vo)
  if ("list" %in% class(data)) {
    d <- length(data)
    n <- dim(data[[1]])[1]
  } else {
    d <- 1
    n <- nrow(data)
  }
  ## n is the observation number 
  ## dim is the dimension of the t(MO, vo).
  fact <- faccal_num(n, d + 1) ## result includes S (covariance of the (MO, Vo) (p + 1) * (p + 1) )
  fac <- fact$fac1
  cutoff <- fact$fac2
  cutoff <- cutoff / fac #cut off value for testing/outlier detection#
  mo <- result$out_avr
  vo <- result$out_var
  out.dir <- which(result$d_value > cutoff)
  medcurve <- which.min(result$d_value)
  return(list(outlier = out.dir, median = medcurve, mo = mo, vo = vo))
}

