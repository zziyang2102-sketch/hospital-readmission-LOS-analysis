# Hospital Readmission & LOS Analysis
A statistical modeling project focused on **Length of Stay (LOS) analysis** and **hospital readmission risk prediction** (built for MS in Applied Data Science application).


## Project Overview
This repository contains R code for:
1. Exploratory data analysis (EDA) of patient LOS (transforming data, visualization).
2. Multiple linear regression modeling for LOS prediction.
3. Logistic regression modeling for readmission risk prediction.
4. Model diagnostics (residual analysis, multicollinearity check).

## Quick Access
- [Full Project Report](report.pdf) (PDF: complete analysis, results, and conclusions)
- [Analysis Code](ZiyangZhang2143444.R) (R script: reproducible code for data processing, modeling, and visualization)
- [Patient Dataset](readmission.csv) (Desensitized input data)
```
## Key Skills Highlighted
- Data Wrangling: Log transformation, factor level management, missing value handling.
- Statistical Modeling: Linear regression (LOS) + logistic GLM (readmission), model selection (AIC/stepwise).
- Diagnostics: Residual analysis, VIF (multicollinearity), influential point detection.
- Visualization: Histograms, boxplots, Q-Q plots for exploratory analysis.


## Dependencies (R Packages)
```r
install.packages(c("tidyverse", "car", "statmod", "e1071"))
