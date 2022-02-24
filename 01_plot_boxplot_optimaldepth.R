packages <- c("gdata")

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
source("subsetlist.R")
#### given the result of trial_different_pcurve
depth_list_cm <- lapply(1:6, function(k) {
  if ("matrix" %in%class(trial_different_pcurve[[k]])) {
    rawdata_index <- 1:nrow(trial_different_pcurve[[k]])
    rawdata <- trial_different_pcurve[[k]]
  } else {
  rawdata_index <- unlist(sapply(1:length(trial_different_pcurve[[k]]), function(l) {
                    if(length(trial_different_pcurve[[k]][[l]]) == 5) {
                      return (l)
                    }
  }))
  #temp <- trial_different_pcurve[[k]][rawdata_index, ]
  temp <- subsetlist(trial_different_pcurve[[k]], rawdata_index)
  # if (k == 3) {
  #   temp <- temp[c(-21, -81, -46, -49, -60, -79), ]
  # }
  rawdata <- unlist(temp)
  }
  trial_rs_cm <- matrix(rawdata, ncol = 5, byrow = TRUE)
  colnames(trial_rs_cm) <- c("r_mfpc", "r_bmfpc", "r_d_aw", "r_d_naw", "r_d_dm")
  coef <- unmatrix(trial_rs_cm, byrow = FALSE)
  trial_times <- length(rawdata_index)
  depthname <- c(rep("'MFHD'['mfpca']", length(rawdata_index)), 
                 rep("'MFHD'['bmfpca']", length(rawdata_index)),
                 rep("'RMFHD'['aw']", length(rawdata_index)),
                 rep("'RMFHD'['naw']", length(rawdata_index)),
                 rep("'RMFHD'['dm']", length(rawdata_index)))
  p_curve <- rep(k / 10, length(depthname))
  data.frame(coef, depthname, p_curve)
  })

final <- depth_list_cm[[1]]
for (l in 2:6) {
  final <- rbind(final, depth_list_cm[[l]])
}

final <- final[final$depthname!="'RMFHD'['dm']", ]
final$depthname <- factor(final$depthname,
                          levels = c("'MFHD'['mfpca']", "'MFHD'['bmfpca']",
                                     "'RMFHD'['aw']","'RMFHD'['naw']"))
final$coef[final$coef < 0] <- 0

sparsity <- rep("partial", 3)
outlierst <- data_type[8]
showlegend <- TRUE

if (showlegend != TRUE) {
  pdf(file = paste("model", which(data_type == outlierst), "_", sparsity[1],".pdf", sep = ""), 
      width = 10, height = 3.7)
  par(mar = c(2.7, 4, 3, 1), mgp = c(2.2, 1, 0))
} else {
  pdf(file = paste("model", which(data_type == outlierst), "_", sparsity[1],".pdf", sep = ""), 
      width = 10, height = 4.2)
  par(mar = c(5.5, 4, 3, 1), mgp = c(2, 1, 0))
}
myplot <- boxplot(as.numeric(coef) ~ depthname * p_curve, data = final, 
                col = c("red", "green", "blue", "yellow"),
                main = paste("Spearman coefficients from various methods: Model ", which(data_type == outlierst), " (", outlierst, " case)", sep = ""),
                varwidth = FALSE, xlim = c(1, 24), ylim = c(0.1, 1), xaxt = "n", 
                xlab = NULL, ylab = "Spearman coefficient", cex.names = 0.6, 
                cex.lab = 0.9, cex = 0.8)

my_names <- c(expression(italic(p)['curve']), paste(seq(10, 60, length.out = 6),"%", sep = ""))
axis(1, at = c(0.3, seq(2.5, 23, length.out = 6)), 
     labels = my_names, 
     tick = FALSE, cex.axis = 0.8)

# Add the grey vertical lines
for (i in seq(4.5, 20.5, length.out = 5)) { 
  abline(v = i, lty = 1, col = "grey")
}
if (showlegend == TRUE) {
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  # Add a legend
  legend("bottom", title="Methods", 
       legend = expression('MFHD'['mfpca'],'MFHD'['bmfpca'],'RMFHD'['aw'],'RMFHD'['naw']), 
       col = c("red", "green", "blue", "yellow"),
       pch = 15, bty = "o",  cex = 1, text.font = 1, 
       horiz = T, inset = c(0.01, 0.01))
}

dev.off()
