# PsyMetricTools

<p align="center">
  <img src="URL_DE_TU_IMAGEN" alt="Logo de tu proyecto" width="200" height="200"/>
</p>

<h1 align="center">PsyMetricTools</h1>

<p align="center">
    Una paquetería que permite de forma facil calcular diferentes análisis.
    <br />
    <a href="https://joseventuraleon.com/"><strong>Explorar la página web »</strong></a>
    <br />
    <br />
</p>

<!-- BADGES -->
<p align="center">
  <!-- Si tienes badges de CRAN, puedes incluirlos así: -->
  <img src="https://www.r-pkg.org/badges/version/EL_NOMBRE_DEL_PAQUETE" alt="CRAN version"/>
  <!-- Badges de estado del build -->
  <img src="URL_DEL_BADGE_DE_BUILD_STATUS" alt="Build status"/>
  <!-- Otras badges -->
  <img src="URL_DE_OTRA_BADGE" alt="Otra badge"/>
</p>

<!-- ESTADÍSTICAS -->
<p align="center">
  <!-- Estadísticas de descargas -->
  <img src="URL_DE_LA_BADGE_DE_DESCARGAS" alt="downloads"/>
  <!-- Otras estadísticas -->
  <!-- ... -->
</p>


## Installation
You can install the latest version of PsyMetricTools from GitHub with the help of the devtools package:
```r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("jventural/PsyMetricTools")
```

# Examples

## EFA with WLSMV using PsyMetricTools library
Here is an example of how to use the library to estimate an EFA model using a WLSMV estimator:
[Estimación de EFA con WLSMV](https://rpubs.com/jventural/EFA_Estimador_WLSMV)

-----

## Outlier detection for factorial analysis using PsyMetricTools library
Here is an example of how to perform the detection of aberrant data, a previous action for the factor analysis:
[Detección de OL](https://rpubs.com/jventural/Deteccion_OL_AF)

-----

## Generation of bar chart for Likert Scale using PsyMetricTools library
Here is a code to generate a quick graph to describe the behavior of the Likert scale:
[Plot Likert Scale](https://rpubs.com/jventural/Plot_Likert_Scale)

-----

## Using the SMOTE algorithm for data balancing using PsyMetricTools library
Here is an example of how to use the SMOTE algorithm to balance data with comparison groups of 2 or more elements:
[Balanceo de datos](https://rpubs.com/jventural/Balanceo_SMOTE)

-----

## Performing bootstraping from confirmatory factor analysis (CFA)
Here is an example of how to bootstrap from CFA:
[Bootstraping con CFA](https://rpubs.com/jventural/Bootstrapping_CFA)

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
Ventura-León, J. (2024). _PsyMetricTools_ [Software]. GitHub. https://github.com/jventural/PsyMetricTools

## Author
Jose Ventura jventuraleon@gmail.com

Packaged: 2024-01-27 10:50:11 UTC; Jose Ventura
