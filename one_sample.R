packages <- c("gdata", "RandomFields")

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

source("sample_pre.R")
source("matern_cov.R")

whittle_block <- function(first_var, second_var, nu_candidate, std, rho_cor) {
  if (first_var == second_var) {
    nu <- nu_candidate[first_var]
    alpha <- (std[first_var])^2
  } else {
    nu <- 1 / 2 * (nu_candidate[first_var] +
                     nu_candidate[second_var])
    alpha <- rho_cor[first_var, second_var] * std[first_var] * std[second_var]
  }
  beta <- 1 / sqrt(2 * nu)
  mat <- sapply(1:length(argval), function(i){
    sapply(1:length(argval), function(j) {
      matern_cov(abs(argval[i] - argval[j]), nu, alpha, beta)
    })
  })
  return (mat)
}


one_sample <- function(outlierst, randomnumber, eigenvalue_type, argval) {
  standard_mu <- c()
  standard_epsilon <- c()
  standard_u <- c()
  for (j in 1:p) {
    standard_mu <- c(standard_mu, mu(j, argval))
    standard_epsilon <- c(standard_epsilon, epsilon(j, argval))
    standard_u <- c(standard_u, u(j, argval, eigenvalue_type))
  }
  k <- rbinom(p, 1, 0.5)
  sign <- sapply(k, function(oo) {
    ifelse(oo == 1, 1, -1)})
  if (randomnumber == 1) {
    if (outlierst == "persistent magnitude outlier") {
    standard_u <- standard_u + 12 * rep(sign, each = length(argval))
    } else if (outlierst == "isolated magnitude outlier") {
      st <- sample(1:(0.9 * length(argval)), p, replace = TRUE)
      period <- rep(0, (p * length(argval)))
      for (i in 1:p) {
        start <- (i - 1) * length(argval) + st[i]
        period[start:(start + round(0.1 * length(argval)))] <- sign[i]
      }
      standard_u <- standard_u + 12 * period
    } else if (outlierst == "shape outlier I") {
      standard_mu <- c()
      shift_val <- c(-0.3, -0.2, -0.5)
      for (j in 1:p) {
        standard_mu <- c(standard_mu, mu(j, argval + shift_val[j]))
       } 
    } else if (outlierst == "mixed outlier") {
        standard_mu <- c()
        for (j in 1:p) {
          interruption <- rexp(1, 2)
          addvalue <- (2 + interruption) * mu(j, argval)
          if (j == p) {
            addvalue <- addvalue - 6
          }
          standard_mu <- c(standard_mu, addvalue)
         }
      } 
  } 
  if (outlierst == "shape outlier II") {
    if (randomnumber == 1) {
      shift_value <- c(2 * sin(4 * pi * argval), 
                       2 * cos(4 * pi * argval),
                       2 * cos(8 * pi * argval))
    } else {
      shift_value <- runif(3 * length(argval), -2.1, 2.1)
    }
    standard_u <- standard_u + shift_value
  } else if (outlierst == "joint outlier") {
    if (randomnumber == 1) {
      z <- runif(p, 2, 8)
     } else {
      rgen <- runif (1, 2, 8)
      #z <- c(rgen, (3 * log(rgen) + 2), (rgen^2 / 4))
      z <- c(rgen, 8 - rgen, rgen - 2)
      }
    addvalue <- c(z[1] * argval * sin(pi * argval), z[2] * argval * cos(pi * argval),
                  z[3] * argval * sin(2* pi * argval))
    standard_u <-  addvalue
    #standard_u <- standard_u + addvalue
  } else if (outlierst == "covariance outlier") {
      if (randomnumber == 1) {
        nu_candidate <- runif(p, 0.1, 0.2)
      } else {
        nu_candidate <- runif(p, 2, 3)
      }
      matern_covar <- c()
      for (i in 1:p) {
        row_mat <- c()
        for (j in 1:p) {
          row_mat <- cbind(row_mat,
                           whittle_block(i, j, nu_candidate, std, rho_cor))
        }
        matern_covar <- rbind(matern_covar, row_mat)
      }
      standard_epsilon <- mvrnorm(n = 1, mu = rep(0, p * length(argval)),
                                  Sigma = matern_covar)
    } 
  true_func <- standard_mu + standard_u    
  observed_func <- standard_mu + standard_u + standard_epsilon
  
  return (list(true_func = true_func,
               observed_func = observed_func))
}


generate_samples <- function(outlierst, eigenvalue_type, argval) {
  rand_nb <- rbinom(n, 1, contamination_level)
  sim <- lapply(1:n, function (i) {
    sample_generate <- one_sample(outlierst, rand_nb[i], eigenvalue_type, argval)
    return(sample_generate)
  })
  mat_true <- matrix(unlist(lapply(sim, function(i) {i$true_func})),
                 nrow = length(argval) * p, ncol = n, byrow = FALSE)
  mat_true_list <- lapply(1:p, function(varr) {
    t(mat_true[((varr - 1) * length(argval) + 1):
                                (varr * length(argval)), ])
  })
  mat_observed <- matrix(unlist(lapply(sim, function(i) {i$observed_func})),
                     nrow = length(argval) * p, ncol = n, byrow = FALSE)
  mat_observed_list <- lapply(1:p, function(varr) {
    t(mat_observed[((varr - 1) * length(argval) + 1):
             (varr * length(argval)), ])
  })
  out_index <- which(rand_nb == 1)
  if (outlierst == "no outlier") {
    out_index <- NULL
  }
  return (list(true_curve = mat_true_list, observed_curve = mat_observed_list, outlier = out_index))
}


