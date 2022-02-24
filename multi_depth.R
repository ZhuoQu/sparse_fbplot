packages <- c("mrfDepth", "ddalpha")

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

hdepth <- mrfDepth::hdepth

multi_depth <- function(x, depth_name) { #c("MFHD","MFPD","MFDPD","MFSPD","MSBD")
  ## x is a list of length(sett). In each list, it is a matrix of obs_nb * var_nb
  marg_depth <- function(mat, depth_name) {
    if (depth_name == "MFHD") {
      options <- list(type = "Rotation", approx = TRUE)
      if (ncol(mat) <= 3 ||nrow(mat) < 120) {
       func <- hdepth(mat)$depthZ
      } else {
        func <- hdepth(mat, options = options)$depthZ
      }
    }
    if (depth_name == "MFPD") {
      func <- projdepth(mat)$depthZ
    }
    if (depth_name == "MFSPD") {
      func <- sprojdepth(mat)$depthZ
    }
    if (depth_name == "MFDPD") {
      func <- dprojdepth(mat)$depthZ
    } else if (depth_name == "MSBD") {
      func <- depth.simplicial(mat, mat, k = 0.05, exact = F)
    }
    return (func)
  }
  
  dp <- t(sapply(x, function(x_list) {
    marg_depth(x_list, depth_name)})) ###dp <- matrix(NA, nrow = t, ncol = n)
  result <- apply(dp, 2, function(dp_col) {mean(dp_col, na.rm = TRUE)})
  return (result)
}



