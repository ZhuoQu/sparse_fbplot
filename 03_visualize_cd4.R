#source("02_summary_cd4.R")
source("sparse_fbplot.R")
source("intensity_sparse_fbplot.R")

# spfbplot_cd4 <- sparse_fbplot(fit = fit_obsedvedgrid, 
#                               sparse = cd4, 
#                               time_index = month, 
#                               depth = cd4_depth,
#                               two_stage = FALSE, plot = TRUE,
#                               xlab = "Months since seroconversion", 
#                               ylab = "Total CD4 Counts", 
#                               title = "Sparse Two-Stage Functional Boxplot: Observed CD4 Counts",
#                               medlabel = FALSE, outlabel = FALSE,  
#                               prob = 0.5, factor = 1.5,
#                               color = 6, outliercolor.fb = 2, barcol = 4,
#                               outliercolor.dir = 3, fullout = FALSE)

pdf("sp_fbplot_cd4.pdf", width = 7.2, height = 6)
sp_twostage_fbplot_cd4 <- sparse_fbplot(fit = cd4_mfpca$bootstrap_fit[[1]], 
                                        sparse = cd4, 
                                        time_index = month, 
                                        depth = cd4_depth,
                                        two_stage = TRUE, sq_vo = FALSE, plot = TRUE,
                                        xlab = "Months since seroconversion", 
                                        ylab = "Total CD4 Counts", 
                                        title = "Sparse Two-Stage Functional Boxplot: Observed CD4 Counts",
                                        yrange = NULL,
                                        cex.main = 1.1, cex.lab = 1.2, cex.axis = 1.2,
                                        medlabel = FALSE, outlabel = FALSE,  
                                        prob = 0.5, factor = 1.5,
                                        color = 6, outliercolor.fb = 2, barcol = 4,
                                        outliercolor.dir = 3, fullout = FALSE)
dev.off()
sp_twostage_fbplot_cd4$fb_outlier_index
sp_twostage_fbplot_cd4$dir_outlier_index
sp_twostage_fbplot_cd4$med_index

pdf("it_fbplot_cd4.pdf", width = 7.7, height = 6)
it_twostage_fbplot_cd4 <- intensity_sparse_fbplot(fit = cd4_mfpca$bootstrap_fit[[1]], 
                                                  sparse = cd4, 
                                                  time_index = month, 
                                                  depth = cd4_depth,
                                                  two_stage = TRUE, sq_vo = FALSE, plot = TRUE, 
                                                  xlab = "Months since seroconversion", 
                                                  ylab = "Total CD4 Counts", 
                                                  title = "Intensity Sparse Two-Stage Functional Boxplot: Observed CD4 Counts",
                                                  yrange = NULL,
                                                  cex.main = 1.1, cex.lab = 1.2, cex.axis = 1.2,
                                                  medlabel = FALSE, outlabel = FALSE,  
                                                  prob = 0.5, factor = 1.5,
                                                  color = 6, barcol = 4,
                                                  outliercolor.fb = 2, colorrange = NULL,
                                                  outliercolor.dir = 3, fullout = FALSE,
                                                  colorsplit = 40, 
                                                  showcontour = FALSE, showlegend = TRUE)
dev.off()
it_twostage_fbplot_cd4$fb_outlier_index
it_twostage_fbplot_cd4$dir_outlier_index
