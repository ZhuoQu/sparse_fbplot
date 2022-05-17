source("multi_depth.R")
source("directional_outlying.R")
source("msplot_modified.R")
load("malnutrition.RData")

mal_depth_data <- lapply(1:ncol(malnutrition_mfpca1$bootstrap_fit[[1]]),
                         function(k) {
                           cbind(malnutrition_mfpca1$bootstrap_fit[[1]][, k],
                                 malnutrition_mfpca1$bootstrap_fit[[2]][, k])
                         })
mal_mfhd <- multi_depth(mal_depth_data, "MFHD")
pdf("ms_plot_malnutrition.pdf", width = 5.4, height = 5.2)
ms_mal <- msplot_modified(data = malnutrition_mfpca1$bootstrap_fit, plot = TRUE, sq_vo = FALSE)
dev.off()
out_mal <- ms_mal$out.dir
nation[out_mal]

med_mal <- ms_mal$medcurve
nation[med_mal]

dir_out_mal <- outlier_dirout(malnutrition_mfpca1$bootstrap_fit, sq_vo = FALSE)
nation[dir_out_mal$outlier]
nation[dir_out_mal$median]

#[1] "Argentina" veryhigh     "Burundi" low      "Bangladesh" median    "Bhutan" median      
#[5] "Chile" veryhigh        "Kuwait" veryhigh       "Malawi" low       "Nepal" low        
#[9] "Romania"  high     "United States" very high  "Viet Nam" median

