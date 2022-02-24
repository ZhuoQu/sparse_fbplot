
source("bootstrap_mfpca.R")
bootstrap_implementation <- function(bootstrap_times, argval,
                                     true_data, 
                                     #observed_data,
                                     sparse_data, confid_alpha) {
  if ("matrix" %in% class(sparse_data)) {
    p <- 1
    n <- nrow(sparse_data)
  }  else {
    p <- length(sparse_data)
    n <- nrow(sparse_data[[1]])
  }
  mfpca_full <- bootstrap_mfpca(argval, true_data, 
                                #observed_data, 
                                sparse_data, isbootstrap = FALSE)
  if (length(true_data) == 0) {
    true_data <- mfpca_full$fit
  }
  
  train_bootstrap <- lapply(1:bootstrap_times, function(bt) {
    cat(bt, "\n")
    mfpca <- bootstrap_mfpca(argval, true_data, sparse_data, isbootstrap = TRUE)
    mfpca_fit <- lapply(1:p, function(k) {
    temp_mat <- matrix(NA, nrow = n, ncol = length(argval))
    temp_mat[mfpca$sample_index, ] <- mfpca$fit[[k]]
    return (temp_mat)
    })
  curve_specific_bias <- function(obs_index) {
    lapply(1:p, function(k) {
      bias <- mfpca_fit[[k]][obs_index, ] - mfpca_full$fit[[k]][obs_index, ]
      return(bias)
      })
  }
  
  curve_specific_covariance <- function(obs_index) {
    lapply(1:p, function(k) {
      measurement_var <- var(mfpca_fit[[k]][obs_index, ] - mfpca_full$fit[[k]][obs_index, ])
    })
  }
  exp_res <- lapply(1:n, curve_specific_bias)
    return(list(model_dif_cov = mfpca$model_error_variance,
                fit = mfpca_fit, 
                exp_res = exp_res))
  })
  
  bootstrap_fit <- lapply(1:p, function(l) {
    fit <- matrix(0, nrow = n, ncol = length(argval))
    for (obs in 1:n) {
      temp_mat <- t(sapply(train_bootstrap, function(k) {
        k$fit[[l]][obs, ]}))
      fit_value <- apply(temp_mat, 2, function(w) {
        mean(w, na.rm = TRUE)})
      fit[obs, ] <- fit_value
    }
    return(fit)
  })
  
  bootstrap_naive_lower <- lapply(1:p, function(l) {
    fit <- matrix(0, nrow = n, ncol = length(argval))
    for (obs in 1:n) {
      temp_mat <- t(sapply(train_bootstrap, function(k) {
      k$fit[[l]][obs, ]}))
      fit_value <- apply(temp_mat, 2, function(w) {
       quantile(w, prob = confid_alpha / 2, na.rm = TRUE)})
      fit[obs, ] <- fit_value
    }
    return(fit)
  })
  
  bootstrap_naive_upper <- lapply(1:p, function(l) {
    fit <- matrix(0, nrow = n, ncol = length(argval))
    for (obs in 1:n) {
      temp_mat <- t(sapply(train_bootstrap, function(k) {
        k$fit[[l]][obs, ]}))
      fit_value <- apply(temp_mat, 2, function(w) {
        quantile(w, prob = 1 - confid_alpha / 2, na.rm = TRUE)})
      fit[obs, ] <- fit_value
    }
    return(fit)
  })
  
  mrse_numerator <- lapply(1:p, function(k) {apply(bootstrap_fit[[k]] - true_data[[k]], 1, function(m) {sum(m^2)})})
  mrse_denominator <- lapply(1:p, function(k) {apply(true_data[[k]], 1, function(m) {sum(m^2)})})
  numer <- mrse_numerator[[1]]
  denom <- mrse_denominator[[1]]
  if (p > 1) {
    for (nb in 2:p) {
      numer <- numer + mrse_numerator[[nb]]
      denom <- denom + mrse_denominator[[nb]]
    }
  }
  mrse <- mean(numer / denom)
  
  # curve_specific_variance <- function(obs_index) {
  #   mat <- lapply(1:p, function(k) {
  #    exp_var <- matrix(NA, nrow = length(argval), ncol = length(argval))
  #    for (i in 1:length(argval)) {
  #      for (j in 1:length(argval)) {
  #        exp_var[i, j] <- mean(sapply(train_bootstrap, function(l) {
  #          l$model_dif_cov[[obs_index]][[k]][i, j]}))
  #      }
  #    }
  #    exp_mat <- t(sapply(train_bootstrap, function(l) {
  #      l$exp_res[[obs_index]][[k]]}))
  #    var_exp <- var(exp_mat[!is.na(colSums(t(exp_mat))), ])
  #    return (diag(exp_var + var_exp))
  #  })
  #   return(mat)
  # }
  
  # bt_variance <- lapply(1:n, curve_specific_variance) 
  return(list(bootstrap_fit = bootstrap_fit, 
              full_fit = mfpca_full$fit, 
              bootstrap_mrse = mrse, full_mrse = mfpca_full$mrse,
              CI_lower = bootstrap_naive_lower, CI_upper = bootstrap_naive_upper))
}


# bootstrap_confid_level <- function(bootstrap_fit, bootstrap_variance,
#                                    confid_type, confid_alpha) {
#   if (confid_type == "simultaneous") {
#     simultaneous_mat <- lapply(1:p, function(k) {
#       temp_mat <- bootstrap_fit[[k]] - mfpca_full$fit[[k]]
#       t(sapply(1:nrow(temp_mat), function(l) {
#         temp_mat[l, ] / sqrt(bootstrap_variance[[l]][[k]])})) 
#       })
#        
#     simultaneous_quant_value <- lapply(simultaneous_mat, function(l) {
#       apply(l, 1, max)})
#     thres <- sapply(simultaneous_quant_value, function(k) {
#       quantile(k, probs = 1 - confid_alpha)})
#   } else {
#     thres <- rep(qnorm(1 - confid_alpha / 2, 0, 1), p)
#   }
# 
#   bootstrap_lower <- lapply(1:p, function(k) {
#     t(sapply(1:nrow(bootstrap_fit[[k]]), function (l) {
#       bootstrap_fit[[k]][l, ] - 
#         thres[k] * bootstrap_variance[[l]][[k]]}))
#   })
# 
#   bootstrap_upper <- lapply(1:p, function(k) {
#     t(sapply(1:nrow(bootstrap_fit[[k]]), function (l) {
#     bootstrap_fit[[k]][l, ] + 
#       thres[k] * bootstrap_variance[[l]][[k]]}))
#   })
#   return(list(lower = bootstrap_lower,
#               upper = bootstrap_upper))
# }

# fit_mfpca_inserted_bootstrap$scores
# 
# fit_mfpca_inserted_bootstrap$CI
# 
# fit_mfpca_inserted_ci <- list()
# fit_mfpca_inserted_ci$lower <- lapply(1:p, function(k) {
#   t(apply(fit_mfpca_inserted_bootstrap$scores %*% fit_mfpca_inserted_bootstrap$CI$alpha_0.05$lower[[k]]@X, 1, function(w) { 
#     w + as.numeric(fit_mfpca_inserted_bootstrap$meanFunction[[k]]@X) })) })
# fit_mfpca_inserted_ci$upper <- lapply(1:p, function(k) {
#   t(apply(fit_mfpca_inserted_bootstrap$scores %*% fit_mfpca_inserted_bootstrap$CI$alpha_0.05$upper[[k]]@X, 1, function(w) { 
#     w + as.numeric(fit_mfpca_inserted_bootstrap$meanFunction[[k]]@X) })) })
# 
# fit_mfpca_inserted_ci$lower[[1]][1, ]
# fit_mfpca_inserted_ci$upper[[1]][1, ]
# bootstrap_ci_zhuo$lower[[1]][1, ]
# bootstrap_ci_zhuo$upper[[1]][1, ]
# fit_mfpca_inserted_bootstrap$fit@.Data[[1]]@X[1, ]
# 
# plot(argval, fit_mfpca_inserted_ci$lower[[1]][1, ], type = "l", lty = 2, col = "grey", 
#      ylim = c(-7, 7))
# lines(argval, fit_mfpca_inserted_bootstrap$fit@.Data[[1]]@X[1, ])
# lines(argval, fit_mfpca_inserted_ci$upper[[1]][1, ], lty = 2, col = "grey")
# lines(argval, bootstrap_ci_zhuo$lower[[1]][1, ], lty = 2, col = "red")
# lines(argval, bootstrap_ci_zhuo$upper[[1]][1, ], lty = 2, col = "red")
# 
