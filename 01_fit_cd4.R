source("subsetlist.R")
#source("00_plot_cd4.R")
source("bootstrap_implementation.R")

for(i in 1:100) {
  cd4_mfpca <- try(bootstrap_implementation(bootstrap_times = 100,
                                                      argval = month,
                                                      true_data = NULL,
                                                      sparse_data = cd4, confid_alpha = 0.05))
  if(inherits(cd4_mfpca, "try-error")) {
    #error handling code, maybe just skip this iteration using
    next
  } else if (min(cd4_mfpca$bootstrap_fit[[1]]) > 0) { 
    stop(i)
  } else {
    next
  }  #rest of iteration for case of no error
}

pdf("cd4.pdf", width = 6, height = 6)
matplot(t(cd4_mfpca$bootstrap_fit[[1]]), type = "l", ylim = range(cd4_mfpca$bootstrap_fit))
dev.off()

save(cd4_mfpca, file = "cd4.RData")

