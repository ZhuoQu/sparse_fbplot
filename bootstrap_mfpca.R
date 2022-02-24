packages = c("funData", "MFPCA", "fdapace")

## Now load or install&load all
package_check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

bootstrap_mfpca <- function(argval, true_data, 
                            #observed_data, 
                            sparse_data, isbootstrap) {
  ## org_data and sparse data are list of p, and each list is a n * length(sett) matrix
  ### isbootstrap is a logical argument
  if ("matrix" %in% class(sparse_data)) {
  sparse_data <- list(sparse_data)
  } 
  nc <- ncol(sparse_data[[1]])
  p <- length(sparse_data)
  n <- nrow(sparse_data[[1]])
  
  obj <- lapply(1:p, function(i) {
    Ly <- sparse_data[[i]]
    res <- funData::funData(list(argval), Ly)
    return(res) }  )
  expres <- list(type = "uFPCA")
  mFData <- multiFunData(obj)
  newmFData <- mFData

  sample_index <- 1:n
  estimated_observed_p <- sapply(1:p, function(i) {
    apply(sparse_data[[i]], 1, function(j) {
      1 - sum(is.na(j)) / nc})})
  
  min_observed_points <- nc * min(estimated_observed_p)
  
  M <- ifelse(min_observed_points <= 3, 4, 9)
  if (length(true_data) == 0) {
    fit_mfpca <- MFPCA::MFPCA(newmFData, M, 
                              uniExpansions = lapply(1:p, function(i) {expres}),
                              fit = TRUE)
    true_data <- lapply(fit_mfpca$fit, function(k) {k@X})
  }
  new_true_data <- true_data
  #new_observed_data <- observed_data
  newsparse_data <- sparse_data
  
  if (isbootstrap == TRUE) {
    sample_nb <- base::sample(1:n, n, replace = TRUE)
    # sample_nb <- base::sample(which(apply(sparse_data[[1]], 1, function(k){sum(!is.na(k))}) > 5),
    #                           round(0.9 * n), replace = TRUE)
    # sample_nb <- c(sample_nb, sample(which(apply(sparse_data[[1]], 1, function(k){sum(!is.na(k))}) < 5),
    #                                   n - round(0.9 * n), replace = TRUE))
    sample_index <- sort(sample_nb)
  
    for (i in 1:p) {
      newmFData[[i]]@X <- mFData[[i]]@X[sample_index, ]
      newsparse_data[[i]] <- sparse_data[[i]][sample_index, ]
      new_true_data[[i]] <- true_data[[i]][sample_index, ]
      #new_observed_data[[i]] <- observed_data[[i]][sample_index, ]
    }
  }

  estimated_observed_p <- sapply(1:p, function(i) {
    apply(sparse_data[[i]], 1, function(j) {
      1 - sum(is.na(j)) / nc})})
  
  fit_mfpca <- MFPCA::MFPCA(newmFData, M, 
                    uniExpansions = lapply(1:p, function(i) {expres}),
                    fit = TRUE)
  
  eigenfun <- fit_mfpca$functions
  gamma <- diag(fit_mfpca$values)
  fitY <- lapply(1:p, function(i) {fit_mfpca$fit[[i]]@X})
  #residual <- lapply(1:p, function(i) {fitY[[i]] - new_observed_data[[i]]}) ## fit minus observation with epsilon
  estimation_error <- lapply(1:p, function(i) {fitY[[i]] - new_true_data[[i]]}) ## fit minus true data with out epsilon
  #meas_error <- sapply(1:p, function(i) {var(unmatrix(residual[[i]]))})
  # curve_specific_covariance <- function(obs_index) {
  #   lapply(1:p, function(k) {
  #     measurement_index <- which(!is.na(sparse_data[[k]][obs_index,]))
  #   t(eigenfun[[k]]@X) %*% 
  #     solve(eigenfun[[k]]@X[, measurement_index] %*% t(eigenfun[[k]]@X[, measurement_index])/meas_error[k] + solve(gamma)) %*%
  #     eigenfun[[k]]@X
  # })
  # }
  # 
  # model_error_variance <- lapply(1:n, curve_specific_covariance)
  mrse_numerator <- lapply(estimation_error, function(k) {apply(k, 1, function(m) {sum(m^2)})})
  if ("list" %in% class(new_true_data)) {
    mrse_denominator <- lapply(new_true_data, function(k) {apply(k, 1, function(m) {sum(m^2)})})
  } else {
    mrse_denominator <- apply(new_true_data, 1, function(m) {sum(m^2)})
    mrse_denominator <- list(mrse_denominator)
  }
  numer <- mrse_numerator[[1]]
  denom <- mrse_denominator[[1]]
  if (p >= 2) {
    for (nb in 2:p) {
      numer <- numer + mrse_numerator[[nb]]
      denom <- denom + mrse_denominator[[nb]]
    }
  }
  mrse <- mean(numer / denom)

  result <- list(
                 #model_error_variance = model_error_variance,
                 #error_var = meas_error_var,
                 fit = fitY, ### p lists of n * length(sett) matrix
                 #residual = residual,  
                 sample_index = sample_index,
                 mrse = mrse)
  ### fit = score %*% Phi + mean
  return (result)
}

