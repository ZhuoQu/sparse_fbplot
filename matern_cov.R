#d: distance
#nu: smoothness parameter
## var: sill
## beta: spatial range
matern_cov <- function(d, nu, var, beta) {
  if (d == 0) {
    mat <- var
  } else {
    mat <- var * (d / beta)^nu * besselK(d / beta, nu) / (2^(nu - 1) * gamma(nu))
  }
  return (mat)
}

