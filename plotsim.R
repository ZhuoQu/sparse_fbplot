source("simulation_parameter.R")
plotsim <- function(sim, outlierst) {
  out_index <- sim[[2]]
  
  setnb <- which(data_type == outlierst)
  
  par(mfrow = c(1, ifelse(p >= 3, 3, p)), mar = c(6, 4, 3, 3), mai = c(0.6, 0.6, 0.4, 0.1))
  
  for (j in 1:p) {
    sam <- sim[[1]][[j]]
    plot(sett, sam[1, ], 
         type = "n", cex.axis = 1.2, 
         cex.lab = 1.3, cex.main = 1.3, 
         xlab = "Time", ylab = "Values", 
         xlim = range(sett), ylim = range(maty),
         main = c(paste("Simulation for Setting", setnb, ": Variable ", j)))
    apply(sam[which(out_index == 0), ], 1, function(i) {
      lines(sett, i, col = "black")
      })
    if (length(which(out_index == 1)) > 1) {
      apply(sam[which(out_index == 1), ], 1,
            function(i) {
              lines(sett, i, col = 2, lwd = 1.3)}) ## pure
    } else if (length(which(out_index == 1)) == 1) {
      lines(sett, sam[which(out_index == 1), ], col = 2, lwd = 1.3)
    }
  }
  invisible(sim)
}

