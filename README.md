# Hospital Readmission & LOS Analysis
A statistical modeling project focused on **Length of Stay (LOS) analysis** and **hospital readmission risk prediction** (built for MS in Applied Data Science application).


## Project Overview
This repository contains R code for:
1. Exploratory data analysis (EDA) of patient LOS (transforming data, visualization).
2. Multiple linear regression modeling for LOS prediction.
3. Logistic regression modeling for readmission risk prediction.
4. Model diagnostics (residual analysis, multicollinearity check).


## File Structure

hospital-readmission-LOS-analysis/
├── readmission_LOS_analysis.R   
├── readmission.csv             
└── README.md                   

## Key Skills Demonstrated
- **Data Wrangling**: Clean missing values, manage data types (factors/numerics).
- **Custom Functions**: Build reusable functions for group mean calculation.
- **Visualization**: Histograms, boxplots for exploratory analysis.
- **Statistical Modeling**: Linear regression (LOS) & logistic regression (readmission).
- **Model Validation**: Residual diagnostics, VIF for multicollinearity.


## Dependencies (R Packages)
Install required packages before running:
```r
install.packages(c("tidyverse", "car", "statmod"))
