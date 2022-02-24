packages <- c( "compiler", "MASS", "fda", "sjmisc", "tidyverse", "gdata")

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

source("simulation_parameter.R")

mu <- function(j, argval) {
  if (j == 1) {
    func <- 5 * sin(2 * pi * argval)
  } else if (j == 2) {
    func <- 5 * cos(2 * pi * argval)
  } else if (j == 3){
    func <- 5 * (argval - 1)^2
  } else {
    func <- 5 * argval ^ j * log (argval + 1)
  }
  return (func)
}

epsilon <- function(j, argval) {
func <- rnorm(length(argval), 0, sd = std[j])
  return (func)}

eigenscore <- function(m, eigenvalue_type) {
  if (eigenvalue_type == "linear") {
    func <- rnorm(1, 0, sd = sqrt((M + m - 1) / M))
  } else {
    func <- rnorm(1, 0, sd = exp(-(m + 1)/2) )
  }
  return (func)
}

eigenfun <- function(j, argval) {
  whole_time <- unmatrix(sapply(1:p, function(l) {l - 1 + argval}))
  basis <- fourier(whole_time, M)
  k <- runif(1, -1, 1)
  if (k < 0) {
    fu <- basis[((j - 1) * length(argval) + 1):(j * length(argval)), ]
  } else { 
    fu <- (-1) * basis[((j - 1) * length(argval) + 1):(j * length(argval)), ]
   } 
  return (fu)
}

u <- function(j, argval, eigenvalue_type) {
  # if (method == "MCov") {
  #   func <- mvrnorm(1, rep(0, p * length(argval)),
  #                   Sigma = covar()) ## vector 
  # } else if (method == "MFPCA") {
  eig_fun <- eigenfun(j, argval) ### (50*3) * M matrix
  scorevector <- sapply(1:M, function(i) {
      eigenscore(i, eigenvalue_type)})
  func <- as.numeric(eig_fun %*% scorevector) ### vector
  # }
  return (func)
}

# ################################### Execution
# method="MFPCA"
# 
# outlierst<-"persistent magnitude outlier"
# contamination_level=0.05
# samplist<-Sample(sett,method="MFPCA",outlierst="persistent magnitude outlier",contamination_level=0.07)
# par(mfrow = c(1, 3), mar = c(5, 4, 3, 3))
# plot(sett,samplist$func[1:50],ylim=range(samplist$func),xlab="Time",ylab="Value",main="Samples")
# lines(sett,samplist$func[51:100])
# lines(sett,samplist$func[101:150])
# 
# par(mfrow=c(1,3),mar=c(4,4,3,1))
# n=100
# sim1<-lapply(1:n,function(i){
#   func<-Sample(sett,rho,M,"MFPCA","no outlier",contamination_level=0)
#   return(func)
#   })
# plotsim(sett,sim1,"no outlier")
# 
# sim2<-lapply(1:n,function(i){
#   func<-Sample(sett,rho,M,"MFPCA","persistent magnitude outlier",contamination_level)
#   return(func)
# })
# p<-3
# plotsim(sett,sim2,"persistent magnitude outlier")
# 
# 
# sim3<-lapply(1:n,function(i){
#   func<-Sample(sett,rho,M,"MFPCA","isolated magnitude outlier",contamination_level)
#   return(func)
# })
# plotsim(sett,sim3,"isolated magnitude outlier")
# 
# 
# sim4<-lapply(1:n,function(i){
#   func<-Sample(sett,rho,M,"MFPCA","shape outlier I",contamination_level)
#   return(func)
# })
# plotsim(sett,sim4,"shape outlier I")
# 
# ### magenta:slow translation  orange:different in amplitude lines  blue:change in amplitude
# sim5<-lapply(1:n,function(i){
#   func<-Sample(sett,rho,M,"MFPCA","shape outlier II",contamination_level)
#   return(func)
# })
# plotsim(sett,sim5,"shape outlier II")
# 
# sim6<-lapply(1:n,function(i){
#   func<-Sample(sett,rho,M,"MFPCA","mixed outlier",contamination_level=0.05)
#   return(func)
# })
# plotsim(sett,sim6,"mixed outlier")
# 
# sim7<-lapply(1:n,function(i){
#   func<-Sample(sett,rho,M,"MFPCA","joint outlier",contamination_level)
#   return(func)
# })
# plotsim(sett,sim7,"joint outlier")
# 
# sim8<-lapply(1:n,function(i){
#   set.seed(i*sample(1:n,1))
#   func<-Sample(sett,rho,M,"MFPCA","covariance outlier",contamination_level)
#   return(func)
# })
# plotsim(sett,sim8,"covariance outlier")
# 
# library(RandomFields)
# x <- seq(0, 1, len=100)
# model <- RMwhittle(nu=1, Aniso=matrix(nc=2, c(1.5, 3, -3, 4)))
# plot(model, dim=2, xlim=c(-1,1))
# z <- RFsimulate(model=model, x,rep(1,100))
# plot(x,z$variable1)
# 

# psi <- function(j) {
#   if (j == 1) {
#     func <- cbind(sqrt(2) * sin(2 * pi * sett),
#                   sqrt(2) * cos(2 * pi * sett),
#                   sqrt(2) * sin(4 * pi * sett))
#   } else if (j == 2) {
#     func <- cbind(sqrt(2) * cos(pi * sett),
#                   sqrt(2) * cos(2 * pi * sett),
#                   sqrt(2) * cos(3 * pi * sett))
#   } else{
#     func <- cbind(sqrt(2) * sin(pi * sett),
#                   sqrt(2) * sin(2 * pi * sett),
#                   sqrt(2) * sin(3 * pi * sett))
#   }
#   return (func)
# }
# 
# lambda <- function(j) {
#   if (j == 1) {
#     func <- diag(c(3, 1.5, 0.75))
#   } else if (j == 2) {
#     func <- diag(c(3.5, 1.75, 0.5))
#   } else {
#     func <- diag(c(2.5, 2, 1))
#   }
#   return (func)
# }
# 
# subvar <- function(j) {
#   func <- psi(j) %*% lambda(j) %*% t(psi(j))
#   return (func)
# }
# 
# subcov <- function(j, jj) {
#   if (j == jj) {
#     stop(paste(j, "cant be equal to", jj, sep = " "))
#   } else {
#     func <- rho * psi(j) %*% (lambda(j))^{1 / 2} %*% 
#       (lambda(jj))^{1 / 2} %*% t(psi(jj))
#   }
#   return (func)
# }
# 
# covar <- function() {
#   val <- function(l) {
#     rowblock <- c()
#     for (w in 1:p) {
#       if (w == l) {
#         addcov <- subvar(w)
#       } else {
#         addcov <- subcov(l, w)
#       }
#       rowblock <- cbind(rowblock, addcov)
#     }
#     return (rowblock)
#   }
#   mat <- c()
#   for (l in 1:p) {
#     rowblock <- val(l)
#     mat <- rbind(mat, rowblock)
#   }
#   return (mat)
# }