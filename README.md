# PsyMetricTools

![R-CMD-check](https://github.com/username/PsyMetricTools/workflows/R-CMD-check/badge.svg)
[![codecov](https://codecov.io/gh/username/PsyMetricTools/branch/master/graph/badge.svg)](https://codecov.io/gh/username/PsyMetricTools)

## Installation

You can install the released version of PsyMetricTools from CRAN with:

```r
install.packages("PsyMetricTools")
```

or install the latest version of PsyMetricTools from GitHub with the help of the devtools package:
```r
install.packages("devtools")
devtools::install_github("username/PsyMetricTools")
```

## Examples
### Calculate Descriptive Statistics
This is an example of how to calculate descriptive statistics for a given range of variables in a dataset:
```r
library(PsyMetricTools)
Assuming 'data' is your dataset and 'Depression' and 'Anxiety' are column names
descriptives <- calculate_descriptives(data, start_col = "Depression", end_col = "Anxiety")
print(descriptives)
```
### Calculate Factor Loadings
Here's how you can calculate and threshold the factor loadings of a specified CFA model:
```r
Assuming 'specifications' is a list of fitted CFA model specifications
result_df <- Standardized_solutions(specifications[[1]], name_items = "CCOV", apply_threshold = TRUE)
print(result_df)
```

### Calculate Percentages for Specific Columns
You can calculate percentage distributions for selected columns with:
```r
Assuming 'data' is your dataset and 'columnas' is a vector of column names
porcentajes <- calcular_porcentajes(data, columnas = c("Gender", "AgeGroup"))
print(porcentajes)
```

## License
GPL-3

## URL
PsyMetricTools on GitHub

## Author
Jose Ventura jventuraleon@gmail.com

Packaged: 2024-01-27 10:50:11 UTC; Jose Ventura
