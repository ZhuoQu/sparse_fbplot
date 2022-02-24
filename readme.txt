
1. To obtain Figure 1 for the CD4 cell count application, run "00_plot_cd4.R" in the CD4 folder in the CD4 folder under the application path.
To obtain Figure 6 for the CD4 cell count application, run "00_plot_cd4.R" to "03_visualize_cd4.R" in the CD4 folder in the application path.

2. To obtain Figure 2 for the malnutrition data application, run "00_malnutrition.R" in the malnutrition folder in the application path.
To obtain Figure 7 for the malnutrition data application, run "00_malnutrition.R" to "03_malnutrition_visualization.R" in the malnutrition folder in the application path.

3. To obtain Figure 3, run unidata_visualize.R. The samples are generated randomly, so the specific pattern maybe different.

4. To obtain Figure 4, run exe_plotsim_sparse.R. 
# Only Models 1,2 and 4 are shown in the manuscipts and the remaining models are shown as Figure S1 in the supplementary material.

5. To obtain each plot in Figure 5, first run "00_execute_simulation_spearman.R"" and change the ``outlierst'' setting, second "01_run plot_boxplot_optimaldepth.R".
If we change the ``sparsity'' variable, we can get similar figures shown as Figure S2 in the supplementary material.

6. To obtain table 1, run execute_outl_detect.R, specify ``outlierst" for eight models, and change the ``sparsity'' for Table S1 in the supplementary material.