# PsyMetricTools

![R-CMD-check](https://github.com/username/PsyMetricTools/workflows/R-CMD-check/badge.svg)
[![codecov](https://codecov.io/gh/username/PsyMetricTools/branch/master/graph/badge.svg)](https://codecov.io/gh/username/PsyMetricTools)

## Installation
You can install the latest version of PsyMetricTools from GitHub with the help of the devtools package:
```r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("jventural/PsyMetricTools")
```

## EFA examples with WLSMV using PsyMetricTools library
Here is an example of how to use the library to estimate an EFA model using a WLSMV estimator:
[Estimación de EFA con WLSMV](https://rpubs.com/jventural/EFA_Estimador_WLSMV)

-----

## Example of some functions of the PsyMetricTools library
### Calculate Descriptive Statistics
This is an example of how to calculate descriptive statistics for a given range of variables in a dataset:
```r
library(PsyMetricTools)
#Assuming 'data' is your dataset and 'Depression' and 'Anxiety' are column names
descriptives <- calculate_descriptives(data, start_col = "Depression", end_col = "Anxiety")
print(descriptives)
```
### Calculate Factor Loadings
Here's how you can calculate and threshold the factor loadings of a specified CFA model:
```r
#Assuming 'specifications' is a list of fitted CFA model specifications
result_df <- Standardized_solutions(specifications[[1]], name_items = "CCOV", apply_threshold = TRUE)
print(result_df)
```

### SMOTE for Multi-Class Imbalance
Here's how to apply SMOTE to handle multi-class imbalance:
```r
# Assuming 'data' is your dataset with an 'outcome' column as the class label
balanced_data <- smote_multiclass(data, outcome = "class_label", perc_maj = 100, k = 5)
print(balanced_data)
```

### Quick Likert chart generation
```r
Plot_Likert(df_new_renombrado, "CCOV", 1:27, exclude = c(1))
```

## License
GPL-3

## Citation
Ventura-León, J. (2024). PsyMetricTools [Software]. GitHub. https://github.com/jventural/PsyMetricTools

## Author
Jose Ventura jventuraleon@gmail.com

Packaged: 2024-01-27 10:50:11 UTC; Jose Ventura
