# Sales Data Analysis: Regression & KNN Classification

---

## Project Overview

1. **Regression Analysis**  
   Predicting log-transformed sales using quantitative and qualitative predictors, with thorough model diagnostics and validation.

2. **K-Nearest Neighbors (KNN) Classification**  
   Classifying deal sizes (Small, Medium, Large) using KNN and identifying the optimal number of neighbors.

---

## Regression Analysis

### Objectives

- Model and predict log-transformed sales.
- Assess and fulfill regression assumptions: normality, homoscedasticity, independence, and multicollinearity.
- Improve model accuracy using quadratic terms and stepwise regression.
- Evaluate model performance on a held-out test set.

### Summary of Approach

- Explored data distributions and variable types.
- Tested and transformed response variable to approximate normality.
- Examined correlations and associations between variables.
- Handled outliers.
- Split data into training (70%) and testing (30%) sets.
- Built and compared linear and quadratic regression models.
- Selected the best model using ANOVA and stepwise selection.
- Diagnosed assumptions using residual analysis, Durbin-Watson test, and VIF.
- Predicted and evaluated on test data using MSE and MAE.

---

## KNN Classification

### Objectives

- Classify `DEALSIZE` into Small, Medium, or Large categories.
- Determine optimal `k` for KNN by minimizing misclassification error.

---
