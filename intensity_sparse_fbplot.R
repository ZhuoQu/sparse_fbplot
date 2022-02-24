packages <- c("plotfunctions", "grDevices")

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
source("fbplot_min_max_curve.R")
source("fbplot_intensity.R")
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

## 13. colorrange is the range of the intensity. The default is NULL. 
# If you want to normalize the intensity across variables, 
## run intensity_sparse_fbplot first with yrange = NULL and get colorrange in the value
## and specify yrange = the range of colorrange

## 14. fullout chooses whether to show all the outliers or just outliers outside the 50% central region.
## 15. showcontour chooses whether to show contours of the intensity
## 16. drawlabels chooses whether to show the labels of the contours
### 17. showlegend chooses whether to show the color map legend
######################    Arguments    #################################################


##################     Value     #################################
## 1. fb_outlier_index is a list of p. In each list, it shows the outlier from functional boxplot if any
## 2. dir_outlier_index is a vector showing possible outliers from the directional outlyingness
## 3. med_index is a index showing the index of the median. 
##It is the observation with the highest depth excluding the directional outlier.

## 4. colorrange is a list of p. In each list it shows the minimum and maximum of the intensity of the variable.
##################     Value     #################################

intensity_sparse_fbplot <- function (fit, sparse, time_index = NULL, 
                                     depth = NULL,
                                     two_stage = TRUE, sq_vo = FALSE, plot = TRUE, 
                                     xlab = NULL, ylab = NULL, title = NULL,
                                     yrange = NULL,
                                     cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3,
                                     medlabel = TRUE, outlabel = TRUE,  
                                     prob = 0.5, factor = 1.5,
                                     color = 6, barcol = 4,
                                     outliercolor.fb = 2, colorrange = NULL,
                                     outliercolor.dir = 3, fullout = FALSE,
                                     colorsplit = 40, 
                                     showcontour = TRUE, drawlabels = FALSE,
                                     showlegend = TRUE) { ## if remove is given, then we use MS plot to detect outliers already
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
  select_col <- c("magenta", "tomato", "gold", "yellow", "white")
  grRd <- colorRampPalette(select_col, space = "rgb")  ####################################################################
  ampli_factor <- ifelse(medlabel == TRUE || outlabel == TRUE, 1 / 40,
                         1 / 120)
  xrange <- c(range(time_index)[1] - ampli_factor * (range(time_index)[2] - range(time_index)[1]), 
              range(time_index)[2] + ampli_factor * (range(time_index)[2] - range(time_index)[1]))
  
  if (plot) {
    if (showlegend == TRUE && sum(is.na(sparse[[1]])) > 0) {
      colnum <- p + 1
      if (p >= 2) {
        major_width <- (0.94 + 0.01 * p)
      } 
      if (p == 1) {
        major_width <- (0.91 + 0.01 * p)
      }
      m <- matrix(1:colnum, nrow = 1, ncol = colnum, byrow = TRUE)
      layout(mat = m, 
             widths = c(rep(major_width / (colnum - 1), colnum - 1), 1 - major_width))
    } else {
      colnum <- min(p, 3)
      m <- matrix(1:colnum, nrow = 1, ncol = colnum, byrow = TRUE)
      layout(mat = m, 
             widths = rep(1 /colnum, colnum))
    }
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
  
  sparse_intensity <- function(p, variable_index) {
    if (p > 1) {
      subfit <- t(fit[[variable_index]])
      subsparse <- t(sparse[[variable_index]])
    } else {
      subfit <- t(fit)
      subsparse <- t(sparse)
    }
    
    medavg <- matrix(subfit[, med_index], ncol = length(med_index), nrow = tp)
    med_value <- apply(medavg, 1, mean)
    
    m <- ceiling(length(normal_index) * prob)
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
    mincurve <- apply(good, 1, min)
    
    if (plot) {
      fbplot_min_max_curve(time_index, xrange, yrange,
                           inf, sup,
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
        rg <- fbplot_intensity(time_index, xrange,
                         med_index, inf, sup,
                         center, sp_center, 
                         subfit, subsparse,
                         barcol = 4,
                         colorsplit = 40, colorrange,
                         showcontour, drawlabels)
      } else {
        rg <- fbplot_intensity(time_index, xrange,
                         med_index, inf, sup,
                         center, sp_center, 
                         subfit, subsparse,
                         barcol = 4,
                         colorsplit = 40, colorrange,
                         showcontour, drawlabels)
        fbplot_outlier(two_stage, outlabel,
                       outliercolor.fb = 2,
                       outliercolor.dir = 3,
                       fb_outlier_index, dir_outlier_index,
                       time_index,
                       subfit, subsparse)
      }
    }
    return(list(fb_outlier_index = fb_outlier_index, rg = rg))
  }
  #####################################################################################
  execute <- lapply(1:p, function(k) {sparse_intensity(p, k)})
  fb_outlier_index <- lapply(execute, function(k) {k$fb_outlier_index})
  colorrange <- lapply(execute, function(k) {k$rg})
  
  if (showlegend == TRUE && sum(which(is.na(sparse[[1]]))) > 0) {
    par(mai = c(0.6, 0.22, 0.95, 0.05))
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "%")
    gradientLegend(valRange = c(0, 100), color = select_col,
                   length = 1, depth = 0.45, side = 4, dec = 0,
                   inside = TRUE, n.seg = 4, cex = 0.7,
                   pos = c(0.3, 0, 0.7, 1.07), coords = FALSE)
  }
  
  return(list(fb_outlier_index = fb_outlier_index, 
              dir_outlier_index = dir_outlier_index, 
              med_index = med_index, colorrange = colorrange))
}

