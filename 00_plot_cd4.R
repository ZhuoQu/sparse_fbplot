
library("refund")
month <- c(-18:42)
list("cd4")
CD4 <- cd4 ## load the cd4 data which is a matrix of 366 * 61

pdf("observed_cd4.pdf", height = 4, width = 4)
par(mfrow = c(1, 1), mai = c(0.5, 0.55, 0.3, 0.07), 
    mar = c(3.5, 3.5, 2, 1), mgp = c(2, 1, 0))
plot(month, CD4[1, ], type = "n", 
     ylim = range(CD4, na.rm = TRUE), ylab ="Total CD4 Cell Counts",
     xlab = "Months since seroconversion", main = "Observed CD4 Counts")
na_prop <- apply(CD4, 1, function(ij) {
  cd4_df <- data.frame(month, ij)
  prop_na <- length(which(is.na(ij) == TRUE)) / ncol(CD4)
  lines(na.omit(cd4_df), col = "grey", lty = 2)
  #points(month, ij, col = "black", cex = 0.4)
  #lines(month, ij, col = "black", lty = 1)
  return(prop_na)
})

apply(CD4, 1, function(ij) {
  cd4_df <- data.frame(month, ij)
  #prop_na<-length(which(is.na(ij)==TRUE))/61
  #lines(na.omit(cd4_df),col="grey",lty=2)
  #points(month,ij,col="black",cex=0.4)
  lines(month, ij, col = "black", lty = 1)
  #return(prop_na)
})

apply(CD4, 1, function(ij) {
  cd4_df <- data.frame(month, ij)
  #prop_na<-length(which(is.na(ij)==TRUE))/61
  #lines(na.omit(cd4_df),col="grey",lty=2)
  points(month, ij, col = "black", cex = 0.4)
  #lines(month,ij,col="black",lty=1)
  #return(prop_na)
})
dev.off()
