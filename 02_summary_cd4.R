#source("01_fit_cd4.R")
source("msplot_modified.R")
library("roahd")

load("cd4.RData")
cd4_depth <- MBD(cd4_mfpca$bootstrap_fit[[1]])

# cd4_ms <- msplot_modified(data = fit_obsedvedgrid, depth.dir = "RP", 
#                         plot = TRUE, 
#                         col.out = "green", col.med = "purple",
#                         col.normal = "black", col = "white",
#                         plot.type = "scatter")
# cd4_out <- cd4_ms$out.dir
# cd4_med <- cd4_ms$medcurve
pdf("ms_plot_cd4.pdf", width = 5, height = 5)
cd4_msplot <- msplot_modified(data = cd4_mfpca$bootstrap_fit[[1]], plot = TRUE, sq_vo = FALSE)
dev.off()
cd4_med <- cd4_msplot$medcurve
cd4_out <- cd4_msplot$out.dir

# cd4_mfpca_fit_depth <- MBD(cd4_mfpca$bootstrap_fit[[1]])
# 
# cd4_mfpca_ms <- msplot_modified(data = cd4_mfpca$bootstrap_fit[[1]], depth.dir = "RP", 
#                           plot = TRUE, 
#                           col.out = "green", col.med = "purple",
#                           col.normal = "black", col = "white",
#                           plot.type = "scatter")
# cd4_mfpca_ms$out.dir
