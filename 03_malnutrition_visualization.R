

#source("02_malnutrition_depth.R")
source("sparse_fbplot.R")
source("intensity_sparse_fbplot.R")

# spfbplot_mal_sparse <- sparse_fbplot(fit = malnutrition_fit, 
#                               sparse = mal_sparse, 
#                               time_index = mal_time, 
#                               depth = mal_mfhd,
#                               two_stage = FALSE, plot = TRUE,
#                               xlab = rep("Year", 2)
#                               ylab = rep("Prevalence (%)", 2)
#                               title = c("Sparse Two-Stage Functional Boxplot: Stunted Growth (Country Level)",
#"Sparse Two-Stage Functional Boxplot: Low Birth Weight (Country Level)"),
#                               medlabel = FALSE, outlabel = FALSE,  
#                               prob = 0.5, factor = 1.5,
#                               color = 6, outliercolor.fb = 2, barcol = 4,
#                               outliercolor.dir = 3, fullout = FALSE)

pdf("sp_fbplot_malnutrition.pdf", width = 12.2, height = 5)
sp_twostage_fbplot_mal_sparse <- sparse_fbplot(fit = malnutrition_mfpca1$bootstrap_fit, 
                                        sparse = mal_sparse, 
                                        time_index = mal_time, 
                                        depth = mal_mfhd,
                                        two_stage = TRUE, sq_vo = FALSE, plot = TRUE,
                                        xlab = c("", ""),
                                        ylab = c("Prevalence (%)", ""),
                                        title = c("Sparse Two-Stage Functional Boxplot: Stunted Growth (Country Level)",
                                        "Sparse Two-Stage Functional Boxplot: Low Birth Weight (Country Level)"), 
                                        yrange = NULL,
                                        cex.main = 0.9, cex.lab = 1, cex.axis = 1,
                                        medlabel = TRUE, outlabel = TRUE,  
                                        prob = 0.5, factor = 1.5,
                                        color = 6, outliercolor.fb = 2, barcol = 4,
                                        outliercolor.dir = 3, fullout = TRUE)
dev.off()
nation[sp_twostage_fbplot_mal_sparse$dir_outlier_index]
nation[sp_twostage_fbplot_mal_sparse$med_index]

pdf("it_fbplot_malnutrition.pdf", width = 12.2, height = 5)
it_twostage_fbplot_mal_sparse <- intensity_sparse_fbplot(fit = malnutrition_mfpca1$bootstrap_fit, 
                                                         sparse = mal_sparse, 
                                                         time_index = mal_time, 
                                                         depth = mal_mfhd,
                                                         two_stage = TRUE, sq_vo = FALSE, plot = TRUE, 
                                                         xlab = rep("Year", 2),
                                                         ylab = c("Prevalence (%)", ""),
                                                         title = c("Intensity Sparse Two-Stage Functional Boxplot: Stunted Growth (Country Level)",
                                                            "Intensity Sparse Two-Stage Functional Boxplot: Low Birth Weight (Country Level)"), 
                                                         yrange = NULL,
                                                         cex.main = 1.25, cex.lab = 1.3, cex.axis = 1.3,
                                                         medlabel = FALSE, outlabel = FALSE,  
                                                         prob = 0.5, factor = 1.5,
                                                         color = 6, barcol = 4,
                                                         outliercolor.fb = 2, colorrange = NULL,
                                                         outliercolor.dir = 3, fullout = FALSE,
                                                         colorsplit = 40, 
                                                         showcontour = TRUE, drawlabels = FALSE,
                                                         showlegend = TRUE)
dev.off()
