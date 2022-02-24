fbplot_polygon <- function(time_index, xrange,
                           inf, sup, 
                           subfit, subsparse,
                           sparse_density_center,
                           med_index, med_value,
                           medlabel, 
                           barcol = 4) {
  x_adj <- time_index
  x_adj[1] <- time_index[1] - 1 / 120 * (range(time_index)[2] - range(time_index)[1])
  x_adj[length(time_index)] <- time_index[length(time_index)] + 1 / 120 * (range(time_index)[2] - range(time_index)[1])
  xx <- c(x_adj, x_adj[order(time_index, decreasing = TRUE)])
  
  med_center <- inf + 0.5 * (sup - inf)
  sep <- inf + unlist(sparse_density_center[, 1]) * (sup - inf)
  if (sum(unique(unlist(sparse_density_center[, 1]))) > 1) {
    sep <- smooth(sep, kind = "3RS3R")
  }
  
  sep_inv <- sep[order(x_adj, decreasing = TRUE)]
  supinv <- sup[order(x_adj, decreasing = TRUE)]
  yy_lower <- c(inf, sep_inv)
  yy_upper <- c(sep, supinv)
  yy <- c(inf, supinv)
  polygon(xx, yy, col = "grey", border = barcol, 
          lwd = 2)
  if (length(unique(unlist(sparse_density_center[, 1]))) == 1 && unique(unlist(sparse_density_center[, 1])) == 1) {
    polygon(xx, yy, col = "magenta", border = barcol, 
            lwd = 2)
  } else if (length(unique(unlist(sparse_density_center[, 1]))) == 2) {
    neighbour_time <- unlist(sparse_density_center[2:length(time_index), 1]) - 
      unlist(sparse_density_center[1:(length(time_index) - 1), 1])
    sep_time <- which(neighbour_time != 0)
    # sep_timenew <- c(sep_time[1] + 1): (sep_time[2] + 1)
    sep <- sup[(min(sep_time) + 1):(max(sep_time) + 1)]
    sep_inv <- rev(sep)
    yy_lower <- c(inf[(min(sep_time) + 1):(max(sep_time) + 1)], sep_inv)
    selected_x <- x_adj[(min(sep_time) + 1):(max(sep_time) + 1)]
    xx_new <- c(selected_x, rev(sort(selected_x)))
    polygon(xx_new, yy_lower, col = "magenta", border = "magenta", 
            lwd = 2)
  } else if (length(unique(unlist(sparse_density_center[, 1]))) > 2) {
    polygon(xx, yy_lower, col = "magenta", border = "magenta", 
            lwd = 2)
    polygon(xx, yy_upper, col = "grey", border = "grey", 
            lwd = 2)
  }
  polygon(xx, yy, border = barcol, lwd = 2)
  sparse_medindex <- which(is.na(subsparse[, med_index])) ## index where there are observations in the midcurves
  nsparse_medindex <- which(!is.na(subsparse[, med_index]))
  lines(time_index, subfit[, med_index], pch = 16,
        lty = 1, lwd = 2, col = gray(0))
  if (length(sparse_medindex) >= 1) {
    for (cl in 1:length(sparse_medindex)) {
      x1_index <- min(sparse_medindex[cl] + 1, nrow(subfit))
      segments(x = time_index[sparse_medindex[cl]],
               y = subfit[sparse_medindex[cl], med_index],
               x1 = time_index[x1_index],
               y1 = subfit[x1_index, med_index],
               lwd = 2.1, lty = 1, col = grey(0.85))
    }
    lines(time_index, med_center, lty = 3, lwd = 1.3, col = "cyan")
  }
  
  if (medlabel == TRUE) {
    text(rev(time_index)[1] + 
           1 / 25 * (range(time_index)[2] - range(time_index)[1]), 
         subfit[length(time_index), med_index],
         labels = med_index, cex = 0.6)
  }
}
