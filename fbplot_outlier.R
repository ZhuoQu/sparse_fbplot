fbplot_outlier <- function(two_stage = TRUE, 
                           outlabel = TRUE,
                           outliercolor.fb = 2,
                           outliercolor.dir = 3,
                           fb_outlier_index, 
                           dir_outlier_index,
                           time_index,
                           subfit, subsparse) {
  out_index <- fb_outlier_index
  if (two_stage == TRUE) {
    out_index <- union(dir_outlier_index, unlist(fb_outlier_index))
  } 
  for (nums in out_index) {
    outl_fit <- subfit[, nums]
    outl_sparse <- subsparse[, nums]
    co <- ifelse(nums %in% dir_outlier_index, outliercolor.dir,
                   outliercolor.fb)
    sparse_outindex <- which(is.na(outl_sparse)) ## index where there are observationums in the midcurves
    nsparse_outindex <- which(!is.na(outl_sparse))
    
    lines(time_index, outl_fit, pch = 16, lty = 2, 
          lwd = 1, col = gray(0.5))
     
    for (cl in 1:length(nsparse_outindex)) {
      sec_index <- min(nsparse_outindex[cl] + 1, length(time_index))
      segments(time_index[nsparse_outindex[cl]], outl_fit[(nsparse_outindex[cl])],
              x1 = time_index[sec_index],
              y1 = outl_fit[sec_index],
              lwd = 1.5, lty = 2, col = co)
     }
  }
  if (outlabel == TRUE) {
    ylocation <- subfit[length(time_index), out_index]
    xlocation <- rep(rev(time_index)[1] + 1 / 40 * (range(time_index)[2] - range(time_index)[1]), 
                     length.out = length(out_index)
                     )
    out_index_index <- order(ylocation)[seq_len(length(out_index)) %% 2 == 0]
    xlocation[out_index_index] <- rep((time_index)[1] - 1 / 40 * (range(time_index)[2] - range(time_index)[1]), 
                                      length.out = length(out_index_index))
    ylocation[out_index_index] <- subfit[1, out_index[out_index_index]]
    text(xlocation, 
         ylocation, out_index, 
         cex = 0.6, col = co)
  }
  invisible(two_stage)
}
