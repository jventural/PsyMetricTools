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

## Examples
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

### Calculate Percentages for Specific Columns
You can calculate percentage distributions for selected columns with:
```r
#Assuming 'data' is your dataset and 'columnas' is a vector of column names
porcentajes <- calcular_porcentajes(data, columnas = c("Gender", "AgeGroup"))
print(porcentajes)
```

### SMOTE for Multi-Class Imbalance
Here's how to apply SMOTE to handle multi-class imbalance:
```r
# Assuming 'data' is your dataset with an 'outcome' column as the class label
balanced_data <- smote_multiclass(data, outcome = "class_label", perc_maj = 100, k = 5)
print(balanced_data)
```

### Rename Survey Items
Quickly rename survey items to a standardized format:
```r
# Assuming 'df' is your dataset with verbose item names
df_renamed <- rename_items(df, prefix1 = "Q", prefix2 = "A", inici = "Question1", final = "Question10", n_items1 = 5, n_items2 = 5)
print(df_renamed)
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
