# Final Assignment: Comprehensive Epidemiological Data Analysis

This directory contains the files and scripts for the final project, which builds upon previous assignments and labs. The project incorporates all learned skills, including logistic regression and Odds Ratios (adjusted and unadjusted), to perform an in-depth study using epidemiological data.

## Directory Structure

- **scripts/**  
  Contains the primary R script used to clean, process, and analyze the data. This includes:
  - Data cleaning and preparation  
  - Logistic regression models (unadjusted and adjusted Odds Ratios)  
  - Weighted prevalence estimation  
  - Creation of visualizations and summary tables

- **Final Assignment.Rproj**  
  The R Project file, organizing the working environment for this assignment.

- **ReadMe.md**  
  This file, providing an overview of the assignment and directory contents.

## Objective

The goal of the project is to analyze the **CCHS dataset (2015-2016 Canadian Community Health Survey)** and provide insights into mental health, focusing on relationships, age groups, and community belonging. The analysis includes:

- Cleaning and filtering the dataset for relevant variables and conditions.
- Performing logistic regression to explore associations between independent variables and mental health outcomes.
- Estimating and visualizing weighted prevalence for key variables.
- Summarizing findings in both visual and tabular formats.

## Key Highlights

1. **Data Preparation**:  
   - Cleaning and recoding variables for analysis.  
   - Creating new categories such as age groups.

2. **Logistic Regression**:  
   - Unadjusted Odds Ratios (ORs) calculated for relationships between variables.  
   - Adjusted ORs incorporating additional covariates.

3. **Weighted Prevalence Estimation**:  
   - Calculation of population-weighted proportions and confidence intervals.  
   - Comparison of survey sample estimates with weighted population metrics.

4. **Visualization and Reporting**:  
   - Creation of tables and charts using `flextable`, `gt`, and other R packages.  
   - Export of results in multiple formats (Word, HTML).

## Notes

- **Dataset**: The **CCHS dataset (2015-2016 Canadian Community Health Survey)** is not included in this repository due to its size and GitHub restrictions. You must download it separately to run the analysis.