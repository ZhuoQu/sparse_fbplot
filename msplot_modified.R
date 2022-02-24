## data is an array where dim(data)=c(n,length(time),p)
##n is the number of samples
##p is the number of variables
packages <- c( "doParallel", "MASS", "ggplot2", "GGally", "abind")

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
source("directional_outlying.R")
msplot_modified <- function(data, depth.dir = "RP", plot = FALSE, 
                            col.out = "green", col.med = "purple",
                            col.normal = "black", col = "white",
                            plot.type = "scatter", sq_vo) {
  ###pairwise plots of variation of outlyingness (VO) against mean outlyingness (MO)###
  if ("list"%in% class(data)) {
    n <- nrow(data[[1]])
    time_nb <- ncol(data[[1]])
    d <- length(data)
  } else {
    n <- nrow(data)
    time_nb <- ncol(data)
    d <- 1
    }
#####################
  outl_detect <- outlier_dirout(data, sq_vo)
  out.dir <- outl_detect$outlier
  medcurve <- outl_detect$median
  mo <- outl_detect$mo
  vo <- outl_detect$vo
  
  if (plot) {
    M <- cbind(mo, vo)
    col.point <- rep(col.normal, n)
    col.point[medcurve] <- col.med
    col.point[out.dir] <- col.out
    pch <- rep(20, n)
    pch[out.dir] <- 19
    pch[medcurve] <- 17
    outcol <- rep("white", n)
    outcol[out.dir] <- 1:length(out.dir)
    outcol[medcurve] <- "black"
          
    if (plot.type == "parallel") {
      data.ms <- data.frame(MO = mo, VO = vo, col = col.point) 
      paral <- ggparcoord(data.ms, columns = 1:ncol(data.ms), groupColumn = (d + 2), showPoints = TRUE)
      return(r1 = list(mo = mo, vo = vo, out.dir = out.dir, medcurve = medcurve, paral))
      }
          
    if (plot.type == "scatter") {
      if (d > 1) {
        MO <- (apply(mo ^ 2, 1, sum))^(1 / 2)
      } else { 
        MO <- abs(mo)
        }
      outlabel <- rep('', n)
      outlabel[out.dir] <- out.dir
      outlabel[medcurve] <- medcurve
      ms.data <- data.frame(x = MO, y = vo, col = col.point, outlabel = outlabel)
      plot(MO, vo, type = "n", ylab = "VO", xlab = "||MO||", 
          main = "Directional Outlyingness")
      for (i in setdiff(1:length(outlabel), c(out.dir,medcurve))) {
        points(MO[i], vo[i], pch = pch[i], 
               col = col.normal, cex = 0.71)}
      for (i in out.dir) {
        points(MO[i], vo[i], pch = pch[i], 
               col = col.out, cex = 0.7)
        }
      points(MO[medcurve], vo[medcurve], col = col.med, pch = pch[medcurve])
      text(MO + (range(MO)[2] - range(MO)[1]) / 70, 
      vo + runif(length(vo), (range(vo)[2] - range(vo)[1]) / 45,
      (range(vo)[2] - range(vo)[1]) / 45), labels = outlabel,
      cex = 0.7)
      }
  } 
  return (list(mo = mo, vo = vo, out.dir = out.dir, medcurve = medcurve))
}

  




