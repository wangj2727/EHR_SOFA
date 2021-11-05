# EHR_SOFA
This repository copntains R code for manuscript "This repository copntains R code for manuscript "Pre-intubation SOFA Score for Predicting COVID-19 Mortality: External Validation using EHR from 86 U.S. Healthcare Systems to Appraise Current Ventilator Triage Algorithms"

The main data collected from the study is not available at this time; therefore the R code is provided only to give a detailed record of how the main analyses were done, and without the data file that code cannot be run to recreate tables and figures in the manuscript. 

## Overview

**Program** and **output** folders contains R scripts and analysis results, separately. 

In the *Program* folder, R and Rmd files were named by either the table/figure number or by the analysis that was included in the code:

1. `DataClean_DataSplit.R` contains R code to perform data cleaning and derivative/validation set splitting.
2. ` MVdata_creation.R` gives R code to create analysis-ready datasets for the primary and sensitivity analyses.
3. `Table 1 Patient Characteristics.Rmd` contains Rmd code to generate Table 1
4. `Table 2_SupplementalTable1_Table3a_LogisticRegression&TreeModel_PrimaryAnalysis.Rmd` contains Rmd code to perform the primary predction analysis using logisitc regression model and conditional classification tree model. 
5. `Figure 2a 2b SuppFigure1 Calibration Belts + ROC curve analysis.Rmd` contains code to construct calibration belts and to perform ROC curve analysis.

6. `SupplementalTable2a_3b_SensitivityAnalysisExcludeESRD.Rmd` contains Rmd code for sensitivity analysis excluding patients with ESRD (End Stage Renal Disease).
7. `SupplementalTable2b_3c_SensitivityAnalysisExcludeESRD&CKD.Rmd` contains Rmd code for another sensitivity analysis exluding patients with ESRD+CKD (Chronic Kidney Disease).
8. `SupplementalTable4a_4b_logisitcReg&TreeModel_ExcludeImputedZero.Rmd` contains Rmd code for another sensitivity analysis excluding patients whose SOFA values got imputed as 0.

9. `2DHistogram Heatmap showing predicted prob of glm models.R` includes R code to visualize predicted probability of Age and/or total SOFA generated from logistic regression models.
10. `ScatterShaded_colorPlot_BasedOnTree&Glm.R` includes R code to visualize predicted performance of Age and total SOFA based on logistic regression models and conditional classification tree model.

