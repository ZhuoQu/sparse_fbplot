exponential_cov <- function(X1, X2, k, c, beta) {
  cov_mat <- matrix(rep(0, length(X1) * length(X2)), nrow = length(X1))
  for (i in 1:nrow(cov_mat)) {
    for (j in 1:ncol(cov_mat)) {
      cov_mat[i, j] <- k * exp(-c * (abs(X1[i] - X2[j]) ^ beta))
    }
  }
  return(cov_mat)
}
