packages <- c( "spatstat.geom", "spatstat")

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

fbplot_intensity <- function(time_index, xrange,
                             med_index, inf, sup,
                             center, sp_center, 
                             subfit, subsparse,
                             barcol = 4,
                             colorsplit = 40, colorrange =  NULL,
                             showcontour = TRUE, drawlabels = FALSE) {
  x_adj <- time_index
  x_adj[1] <- time_index[1] - 1 / 120 * (range(time_index)[2] - range(time_index)[1])
  x_adj[length(time_index)] <- time_index[length(time_index)] + 1 / 120 * (range(time_index)[2] - range(time_index)[1])
  xx <- c(x_adj, x_adj[order(time_index, decreasing = TRUE)])
  supinv <- sup[order(x_adj, decreasing = TRUE)]
  yy <- c(inf, supinv)
  
  x_y_match <- function(jk) {
    yvalue <- unique(center[jk, ])
    xtime <- rep(time_index[jk], length(yvalue))
    na_index <- which(is.na(sp_center[jk, ]))
    zna_point <- sapply(1:length(xtime), function(tpp) {
      mat_index <- which(center[jk, ] == yvalue[tpp])
      sp_ind <- intersect(mat_index, na_index)
      val <- ifelse(length(sp_ind) > 0, 1, 0)
    })
    wl <- data.frame(x = xtime, y = yvalue, zna = zna_point)
    return (wl)
  }
  point <- lapply(1:length(time_index), x_y_match)
  xpoint <- unlist(lapply(point, function(ls) {ls["x"]}))
  ypoint <- unlist(lapply(point, function(ls) {ls["y"]}))
  nobs_point <- unlist(lapply(point, function(ls) {ls["zna"]}))
  specific_x <- sort(unique(xpoint[nobs_point == 1]))
  if (length(specific_x) > 0) {
    new_xx <- c(x_adj[which(time_index %in% specific_x)], rev(x_adj[which(time_index %in% specific_x)]))
    new_yy <- c(inf[which(time_index %in%specific_x)], rev(sup[which(time_index %in% specific_x)]))
    pp_na <- ppp(xpoint[nobs_point == 1], ypoint[nobs_point == 1], poly = list(x = new_xx, y = new_yy))
    Q.d <- density.ppp(pp_na, adjust = 1, dimyx = c(200, 200), at = "pixels")
    norm_Q.d <- Q.d
    if (length(colorrange) == 0) {
      rg <- range(Q.d[['v']], na.rm = TRUE)
    } else {
      rg <- range(unlist(colorrange))
    }
    norm_Q.d$v <- Q.d$v / (max(rg, na.rm=TRUE))
    select_col <- c("magenta", "tomato", "gold", "yellow", "white")
    grRd <- colorRampPalette(select_col, space = "rgb")  ####################################################################
    CO <- colourmap(grRd(colorsplit), range = c(0, 1))
    polygon(xx, yy, col = "magenta", border = barcol, lwd = 2)
    plot(norm_Q.d, las = 1, add = TRUE, col = CO)  # Plot density raster
    if (showcontour == TRUE) {
      contour(norm_Q.d, add = TRUE, drawlabels = FALSE, cex = 0.7, levels = c(0, 0.2, 0.4, 0.6, 0.8, 1))
    }
    polygon(xx, yy, border = barcol, lwd = 2)
    } else {
    polygon(xx, yy, col = "magenta", border = barcol, lwd = 2)
  }
  
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
  }
  if (length(specific_x) == 0) {
    return (NULL)
  } else {
  return (rg)
  }
}
