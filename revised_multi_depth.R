source("multi_depth.R")
revised_multi_depth <- function(x, depth, opt) { #c("MFHD","MFPD","MFDPD","MFSPD","MSBD")
  ## mixed data is turned to a list from a matrix with 600*150 where 200 observations, 3 variables and 50 time points
    ## number of time points
  var_nb <- ncol(x[[1]]) ### number of variables
  obs_nb <- nrow(x[[1]]) / var_nb ## number of observations
  
  if (opt == "aw") {
    w <- rep(1 / 3, 3)
  } else if (opt == "naw") {
    w <- c(1 / 2, 1 / 4, 1 / 4)
  } else if (opt == "bmfpca") {
    w <- c(1, 0, 0)
  }
  if (opt == "aw" || opt == "naw" || opt == "bmfpca") { 
    orgresult <- multi_depth(x, depth)
    rs_depth <- apply(matrix(orgresult, nrow = obs_nb, ncol = 3), 1, function(kk) {
      return (kk %*% w)
    })
  } else if (opt == "dm") {
    newx <- lapply(x, function(ii) {
      lower <- ii[(obs_nb + 1) : (2 * obs_nb), ]
      upper <- ii[(2 * obs_nb + 1) : (3 * obs_nb), ]
      fc <- cbind(lower, upper)
      return (fc)
      })
    rs_depth <- multi_depth(newx, depth)
  } else if (opt == "tm") {
    newx <- lapply(x, function(ii) {
      fit <- ii[1:obs_nb, ]
      lower <- ii[(obs_nb + 1) : (2 * obs_nb), ]
      upper <- ii[(2 * obs_nb + 1) : (3 * obs_nb), ]
      fc <- cbind(lower, fit, upper)
      return (fc)
    })
    rs_depth <- multi_depth(newx, depth)
  }
  return(rs_depth)
}

