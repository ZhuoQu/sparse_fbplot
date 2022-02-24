packages <- c( "compiler")

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

observed_measure <- function(vec, sparsity, p_curve) {
  nc <- length(sett)
  if (sparsity == "point") {
    sparse_s <- sort(sample.int(nc, ceiling(nc * p_curve))) ## point sparsity with 20% missing in curves
  } else if (sparsity == "partial" || sparsity == "peak") {
    sparse_st <- base::sample(1:ceiling(nc - nc * p_curve), 1)
    sparse_s <- sparse_st:(sparse_st + ceiling(p_curve * nc))
  }
  if (p_curve == 0 || sparsity == "dense") {
    sparse_s <- length(sett) + 1
  }
  vec[sparse_s] <- NA
  vec <- vec[1:nc]
  return (list(sparse_index = sparse_s, value = vec))
}

uni_sparse <- function(dat, p_size, p_curve, sparsity) {
  ### dat is a matrix of n * length(sett) 
  sparsecase <- dat
  nc <- length(sett)
  n <- nrow(dat)
  if (sparsity != "dense" && p_size > 0 && p_curve > 0) {
    sparse_ind <- sample.int(n, ceiling(n * p_size))
  } else {
    sparse_ind <- NULL
  }
  if (length(sparse_ind) > 1) {
    if (sparsity != "partial") {
      vep <- apply(sparsecase[sparse_ind, ], 1, function(l) {
      sp_fun <- observed_measure(l, sparsity, p_curve)
      sp_index <- sp_fun$sparse_index
      sp_data <- sp_fun$value
        return(sp_data)
      })
      sparsecase[sparse_ind, ] <- t(vep)
    } else {
        sp_fun <- observed_measure(sparsecase[sparse_ind[1], ], sparsity, p_curve)
        sp_index <- sp_fun$sparse_index
        sparsecase[sparse_ind, sp_index] <- NA
    }
  }
  return (sparsecase)  ### n * length(sett) matrix
}


#data<-uni_sparse(mat,c(0.5),c(0.4),"partial")


#plot(t,data[1,],type="n",ylim=range(data,na.rm=TRUE))
#apply(data,1, function(x){points(t,x)})

#sparsee<-data[which(is.na(rowSums(data))),]
