#org<-multi_gen(numsample,vale,t,va)
#datacase<-multi_case_index(org,out_p=0.05,sce=c("pure outlier","shape outlier"))
#mat<-lapply(1:length(datacase),function(i){datacase[[i]]$case})
#out_index<-lapply(1:length(datacase),function(i){datacase[[i]]$index})
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
source("uni_sparse.R")
multi_sparse <- function(mat, p_size, p_curve, sparsity) {
  ####### assumes p_size and  p_curve are vectors
  if (length(mat) == length(p_size) && 
      length(p_curve) == length(sparsity) && 
      length(p_size) == length(p_curve)) {
    func <- function(i) {
      y <- uni_sparse(mat[[i]], p_size[i], p_curve[i], sparsity[i])
      return (y)
      }
    myfunc <- cmpfun(func)
    res <- lapply(1:p, FUN = myfunc)
    return (res)
    } else {
    stop ("not valid input")
    }
  ## res is a list where each element contains sparse matrix n * length(sett)
}

#testdata<-multi_sparse(mat,c(0.3,0.5),c(0.2,0.4),c("dense","peak"))

#dataplot<-testdata[[2]]
#dataplot[which(is.na(rowSums(dataplot))),][1,]

