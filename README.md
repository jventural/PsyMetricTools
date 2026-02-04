<h1 align="center">PsyMetricTools</h1>

<p align="center">
  <strong>Psychometric Analysis Tools for R</strong>
</p>

<p align="center">
  <a href="#installation">Installation</a> •
  <a href="#main-functions">Functions</a> •
  <a href="#examples">Examples</a> •
  <a href="#citation">Citation</a>
</p>

---

## Description

**PsyMetricTools** is an R package designed to facilitate psychometric analysis in research. It includes functions for:

- Exploratory Factor Analysis (EFA) and Confirmatory Factor Analysis (CFA)
- Bootstrap for model stability
- Professional visualizations for publication
- Detection of aberrant response patterns
- Measurement invariance analysis
- Reliability indices calculation

## Installation

```r
# Install from GitHub
if (!require("devtools")) install.packages("devtools")
devtools::install_github("jventural/PsyMetricTools")
```

## Main Functions

### Factor Analysis

| Function | Description |
|----------|-------------|
| `EFA_modern()` | EFA with WLSMV estimator for ordinal data |
| `boot_cfa()` | CFA with bootstrap for stability |
| `boot_efa()` | EFA with bootstrap |
| `easy_invariance()` | Measurement invariance analysis |
| `iterativeModelCFA()` | Iterative CFA with modification indices |
| `iterativeModelEFA()` | Iterative EFA |

### Visualization

| Function | Description |
|----------|-------------|
| `boot_cfa_plot()` | Bootstrap indices boxplot |
| `boot_cfa_density()` | Bootstrap distribution densities |
| `boot_cfa_raincloud()` | Modern raincloud plots |
| `EFA_plot()` | Factor loadings visualization |
| `Plot_Likert()` | Likert response distribution |
| `plot_cfa_stability()` | CFA stability plots |

### Preprocessing

| Function | Description |
|----------|-------------|
| `filtrar_aberrantes()` | Aberrant pattern detection (Mahalanobis) |
| `invertir_items()` | Item inversion |
| `smote_multiclass()` | Data balancing with SMOTE |
| `split_data_two()` | Data splitting (train/test) |

### Results Extraction

| Function | Description |
|----------|-------------|
| `Standardized_solutions()` | EFA standardized solutions |
| `Standardized_solutions_cfa()` | CFA standardized solutions |
| `extract_fit_measures()` | Fit indices |
| `calcula_omega_mcdonald()` | McDonald's omega |

## Examples

### Complete Bootstrap CFA Analysis

```r
library(PsyMetricTools)
library(lavaan)

# Define model
model <- 'Factor =~ item1 + item2 + item3 + item4 + item5'

# Bootstrap CFA (100 replications)
results <- boot_cfa(
  new_df = data,
  model_string = model,
  item_prefix = "item",
  n_replications = 100,
  ordered = TRUE,
  estimator = "WLSMV"
)

# Visualizations
boot_cfa_plot(results)
boot_cfa_density(results)
boot_cfa_raincloud(results, color_scheme = "ocean")
```

### Aberrant Pattern Detection

```r
# Filter cases with aberrant response patterns
result <- filtrar_aberrantes(
  data = data,
  items = c("item1", "item2", "item3"),
  plot = TRUE
)

# Use clean data
clean_data <- result$data_filtrada
```

### Likert Scale Visualization

```r
# Response distribution plot
Plot_Likert(
  Data = data,
  name_items = "item",
  ranges = 1:10,
  text_size = 2.5
)
```

### Invariance Analysis

```r
# Configural, metric, and scalar invariance
invariance <- easy_invariance(
  data = data,
  model = model,
  group = "group",
  ordered = TRUE
)
```

## Tutorials

- [EFA with WLSMV estimator](https://rpubs.com/jventural/EFA_Estimador_WLSMV)
- [Outlier Detection](https://rpubs.com/jventural/Deteccion_OL_AF)
- [Likert Charts](https://rpubs.com/jventural/Plot_Likert_Scale)
- [SMOTE Balancing](https://rpubs.com/jventural/Balanceo_SMOTE)
- [Bootstrap CFA](https://rpubs.com/jventural/Bootstrapping_CFA)

## Citation

```
Ventura-Leon, J. (2024). PsyMetricTools: Psychometric Analysis Tools
for R (Version 1.0.0) [Software].
https://github.com/jventural/PsyMetricTools
```

## Author

**Jose Ventura-Leon, PhD**
ORCID: [0000-0003-2996-4244](https://orcid.org/0000-0003-2996-4244)
Email: jventuraleon@gmail.com
Web: [joseventuraleon.com](https://joseventuraleon.com/)

## License

GPL-3
