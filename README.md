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

### Visualization

| Function | Description |
|----------|-------------|
| `boot_cfa_plot()` | Bootstrap indices boxplot |
| `boot_cfa_density()` | Bootstrap distribution densities |
| `boot_cfa_raincloud()` | Modern raincloud plots |
| `EFA_plot()` | Factor loadings visualization |
| `Plot_Likert()` | Likert response distribution |
| `plot_cfa_stability()` | CFA stability plots |
| `plot_path_mediation()` | Three-column SEM path diagram (distal predictors → mediators → outcome) |
| `plot_mediation_forest()` | Forest plot of direct, indirect, and total effects with 95 % CIs |

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

### SEM Mediation: Path Diagram and Forest Plot

```r
library(lavaan)
library(PsyMetricTools)

# Path-analysis model: distal predictors -> mediators -> outcome
mod <- '
  M1 ~ a1*X1 + a2*X2 + a3*X3
  M2 ~ b1*X1 + b2*X2 + b3*X3
  Y  ~ p1*M1 + p2*M2 + c1*X1 + c2*X2 + c3*X3
  ind_X1 := a1*p1 + b1*p2
  ind_X2 := a2*p1 + b2*p2
  ind_X3 := a3*p1 + b3*p2
  tot_X1 := c1 + ind_X1
  tot_X2 := c2 + ind_X2
  tot_X3 := c3 + ind_X3
'
fit <- sem(mod, data = your_data, estimator = "MLR", missing = "fiml")

# 1) Three-column path diagram (default style)
nodes <- data.frame(
  id    = c("X1","X2","X3","M1","M2","Y"),
  label = c("Predictor 1","Predictor 2","Predictor 3",
            "Mediator 1","Mediator 2","Outcome"),
  block = c("Distal","Distal","Distal","Mediator","Mediator","Outcome"),
  layer = c(1,1,1,2,2,3),
  y     = c(1, 0, -1, 0.5, -0.5, 0)
)
plot_path_mediation(fit, nodes,
                    title = "Mediation model")

# 2) Forest plot of decomposed effects (faceted by effect type)
plot_mediation_forest(
  fit,
  predictors = c("X1","X2","X3"),
  outcome = "Y",
  predictor_labels = c(X1 = "Predictor 1",
                       X2 = "Predictor 2",
                       X3 = "Predictor 3"),
  layout = "facet",            # one panel per effect type
  p_text_position = "panel",   # p-values aligned at panel margin
  title = "Direct, indirect and total effects on the outcome"
)
```

The forest plot is the recommended visualization when the model has eight or more distal predictors, following the recommendation of Fife and Mendoza (2018) on supplementing path diagrams with effect-decomposition panels.

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
