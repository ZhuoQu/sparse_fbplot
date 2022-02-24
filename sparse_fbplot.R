source("directional_outlying.R")
source("fbplot_min_max_curve.R")
source("fbplot_polygon.R")
source("fbplot_outlier.R")

########################   Arguments    #######################################################
## assume that n is the number of observations
### t is the number of time grid
### p is the number of observations
## 1.fit is a list with p (number of variables). and each list is a n*t matrix 
## 2. sparse is a list with p (number of variables). and each list is a n*t matrix 
## 3. time_index is the coordinate of time or index. If not given, 1,...length(x) is provided.
## 4. depth is the depth value for n observations
## 5. xlab, ylab, and title specify the lab and titles of the figure (It should be a vector of length p)
## 6. cex.main, cex.lab and cex.axis are parameters to adjust the size of the title, label and axis.
## 7. two_stage is an argument of whether two-stage version or not
## 8. yrange is the range of y axis. If it is null, then yrange is the range of the corrsponding variable.
## 9. sq_vo is whether to choose the square root of the vo for directional outlyingness
## 10. plot chooses whether to show the plot.
## 11. medlabel chooses whether to show the label of the median.
## 12. outlabel chooses whether to show the label of the outlier.
## 13. fullout chooses whether to show all the outliers or just outliers outside the 50% central region.
######################    Arguments    #################################################


##################     Value     #################################
## 1. sparse_density_ct is a list of p. 
##Each list is matrix of t*2, where the first column shows the observation proportion,
## and the second column shows the sparse proportion at this time point

## 2. fb_outlier_index is a list of p. In each list, it shows the outlier from functional boxplot if any
## 3. dir_outlier_index is a vector showing possible outliers from the directional outlyingness
## 4. med_index is a index showing the index of the median. 
## It is the observation with the highest depth excluding the directional outlier.
##################     Value     #################################

sparse_fbplot <- function (fit, sparse, time_index = NULL, depth = NULL,
                         two_stage = TRUE, sq_vo = FALSE, plot = TRUE,
                         xlab = NULL, ylab = NULL, title = NULL,
                         yrange = NULL,
                         cex.main = 0.9, cex.lab = 0.85, cex.axis = 0.85,
                         medlabel = TRUE, outlabel = TRUE,  
                         prob = 0.5, factor = 1.5,
                         color = 6, outliercolor.fb = 2, barcol = 4,
                         outliercolor.dir = 3, fullout = FALSE) {
  
  if ("list"%in% class(fit)) {
    n <- nrow(fit[[1]])
    tp <- ncol(fit[[1]])
    p <- length(fit)
  } else {
    n <- nrow(fit)
    tp <- ncol(fit)
    p <- 1
  }
  
  if (length(time_index) == 0) {
    time_index <- 1:tp
  }
  index <- order(depth, decreasing = TRUE)
  ampli_factor <- ifelse(medlabel == TRUE || outlabel == TRUE, 1 / 40,
                         1 / 120)
  xrange <- c(range(time_index)[1] - ampli_factor * (range(time_index)[2] - range(time_index)[1]), 
              range(time_index)[2] + ampli_factor * (range(time_index)[2] - range(time_index)[1]))
  if (plot) {
     colnum <- min(p, 3)
     m <- matrix(1:colnum, nrow = 1, ncol = colnum, byrow = TRUE)
     layout(mat = m, 
           widths = rep(1 / colnum, colnum))
    par(mai = c(0.7, 0.8, 0.4, 0.1), mar = c(4.5, 4.2, 2, 0.5),
        mgp = c(3, 1.5, 0))
  }
  if (two_stage == TRUE) {
     outl_detect <- outlier_dirout(fit, sq_vo)
     dir_outlier_index <- outl_detect$outlier
     #med_index <- outl_detect$median
  } else {
    #med_index <- which(depth == max(depth))
    dir_outlier_index <- NULL
  }
  normal_index <- setdiff(index, dir_outlier_index)
  med_index <- which(depth == max(depth[normal_index]))
  
  sparse_ds <- function(p, variable_index) {
    if (p > 1) {
    subfit <- t(fit[[variable_index]])
    subsparse <- t(sparse[[variable_index]])
    } else {
      subfit <- t(fit)
      subsparse <- t(sparse)
    }
    medavg <- matrix(subfit[, med_index], ncol = length(med_index), nrow = tp)
    med_value <- apply(medavg, 1, mean)
    
    m <- floor(length(normal_index) * prob)
    center <- subfit[, normal_index[1:m]]
    sp_center <- subsparse[, normal_index[1:m]]
    
    inf <- apply(center, 1, min) ### shown in figure
    sup <- apply(center, 1, max) ### shown in figure
    dist <- factor * (sup - inf)
    upper <- sup + dist
    lower <- inf - dist
    outly <- (subfit <= lower) + (subfit >= upper)
    outcol <- colSums(outly)
    removefb <- which(outcol > 0)
    fb_outlier_index <- setdiff(removefb, dir_outlier_index)
    if (length(fb_outlier_index) == 0) {
      good <- subfit[, normal_index]
    } else {
      good <- subfit[, setdiff(normal_index,fb_outlier_index)]
    }
    
    maxcurve <- apply(good, 1, max) ### shown in figure
    mincurve <- apply(good, 1, min) ### shown in figure
    
    sparse_density_center <- t(sapply(1:nrow(sp_center),
                                      function(nnr) {
      resf <- data.frame(obs = length(which(!is.na(sp_center[nnr, ]))) / m,
                       spa = length(which(is.na(sp_center[nnr, ]))) / m)
      }))
    
    if (plot) {
      fbplot_min_max_curve(time_index, xrange, yrange, inf, sup,
                           subfit, mincurve, maxcurve,
                           barcol = 4,
                           xlab[variable_index], 
                          ylab[variable_index], 
                          title[variable_index],
                          cex.main = cex.main, cex.lab = cex.lab, cex.axis = cex.axis)
      if (fullout == FALSE) {
        fbplot_outlier(two_stage, outlabel,
                       outliercolor.fb = 2,
                       outliercolor.dir = 3,
                       fb_outlier_index, dir_outlier_index,
                       time_index,
                       subfit, subsparse)
        fbplot_polygon(time_index, xrange, 
                       inf, sup, 
                       subfit, subsparse,
                       sparse_density_center,
                       med_index, med_value,
                       medlabel, barcol = 4)
      } else {
        fbplot_polygon(time_index, xrange, 
                       inf, sup, 
                       subfit, subsparse,
                       sparse_density_center,
                       med_index, med_value,
                       medlabel, barcol = 4)
        fbplot_outlier(two_stage, outlabel,
                       outliercolor.fb = 2,
                       outliercolor.dir = 3,
                       fb_outlier_index, dir_outlier_index,
                       time_index,
                       subfit, subsparse)
      }
    }
    return(list(fb_outlier_index = fb_outlier_index,
                sparse_density_ct = sparse_density_center))
  }
  #####################################################################################
  execute<- lapply(1:p, function(k) {sparse_ds(p, k)})
  sparse_density <- lapply(execute, function(k) {k$sparse_density_ct})
  fb_outlier_index <- lapply(execute, function(k) {k$fb_outlier_index})
  
  return(list(sparse_density_ct = sparse_density,
              fb_outlier_index = fb_outlier_index, 
              dir_outlier_index = dir_outlier_index, 
              med_index = med_index))
}

