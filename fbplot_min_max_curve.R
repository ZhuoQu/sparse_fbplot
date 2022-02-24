fbplot_min_max_curve <- function(time_index, xrange, yrange,
                                 inf, sup,
                                 subfit, mincurve, maxcurve,
                                 barcol = 4,
                                 xlab, ylab, title,
                                 cex.main = 1.3, cex.lab = 1.3, cex.axis = 1.3) {
  if (length(yrange) == 0) {
    yrange <- range(subfit)
  }
  plot(time_index, time_index, lty = 1, lwd = 2, 
       col = "white", type = "n", 
       xlim = xrange, ylim = yrange, 
       ylab = ylab, xlab = xlab, 
       main = title, 
       #cex.main = 0.9, cex.lab = 0.85, cex.axis = 0.85
       cex.main = cex.main, cex.lab = cex.lab, cex.axis = cex.axis)
  
  barval <- (min(time_index) + max(time_index)) / 2
  bar <- which(sort(c(time_index, barval)) == barval)[1]
  lines(c(time_index[bar], time_index[bar]), c(maxcurve[bar], sup[bar]), 
        col = barcol, lwd = 2)
  lines(c(time_index[bar], time_index[bar]), c(mincurve[bar], inf[bar]), 
        col = barcol, lwd = 2)
  lines(time_index, maxcurve, type = "l", lwd = 2, col = barcol)
  lines(time_index, mincurve, type = "l", lwd = 2, col = barcol)
}

