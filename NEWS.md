# PsyMetricTools 1.2.1

## CRAN fixes

* The PDF manual now builds without LaTeX errors: the Unicode symbols
  `>=`, `<=`, `->` and `R^2` are written in portable form in the
  documentation of `boot_cfa_plot_enhanced()`, `plot_mediation_chord()`,
  `plot_mediation_donuts()` and `plot_path_mediation()`. This was the
  error that caused the 1.2.0 submission to be archived by CRAN.
* `DESCRIPTION` now declares `R (>= 4.1.0)`, required by the use of the
  native pipe `|>` / lambda `\(...)` syntax in `invertir_items()` and
  `split_data_stratified_clustered()`.

## Improvements

* `plot_path_cfa()` rewritten: latent variables as circles, observed
  variables as rectangles, fan-out arrows, black-and-white default.

# PsyMetricTools 1.2.0

## Improvements

* `plot_multi_sem()` now supports multi-row panel layouts via the new `nrow` and
  `ncol` arguments. By default it keeps the previous behaviour (a single row with
  one column per model); when a grid is requested, leftover cells are left blank.
  Saved-figure dimensions scale with the grid (`width_per` per column, `height`
  per row).

# PsyMetricTools 1.0.0

* Initial CRAN submission

## Features

### Data Preprocessing
* `filtrar_aberrantes()` - Filter aberrant response patterns using Mahalanobis distance
* `invertir_items()` - Reverse score items
* `split_data_two()` / `split_data_three()` - Split data for cross-validation
* `smote_multiclass()` - SMOTE oversampling for imbalanced multiclass data

### Exploratory Factor Analysis (EFA)
* `EFA_modern()` - Modern EFA with multiple rotation options
* `EFA_plot()` - Visualize factor loadings
* `Standardized_solutions()` - Extract standardized EFA solutions
* `boot_efa()` - Bootstrap EFA for stability analysis
* `efa_with_bootstrap()` - EFA with bootstrap resampling

### Confirmatory Factor Analysis (CFA)
* `multi_cfa()` - Fit multiple CFA models
* `Standardized_solutions_cfa()` - Extract standardized CFA solutions
* `boot_cfa()` - Bootstrap CFA for stability analysis
* `boot_cfa_stability()` - CFA stability across sample sizes

### Measurement Invariance
* `easy_invariance()` - Measurement invariance analysis with ordinal data following Wu and Estabrook (2016)

### Visualization
* `Plot_Likert()` / `Plot_Likert2()` - Likert scale visualizations
* `boot_cfa_plot()` - Bootstrap CFA results visualization
* `boot_cfa_density()` - Bootstrap distribution density plots
* `boot_cfa_raincloud()` - Raincloud plot visualizations
* `boot_efa_plot()` / `boot_efa_forest_plot()` - Bootstrap EFA visualizations
* `plot_cfa_stability()` family - CFA stability visualizations

### Reliability
* `calcula_omega_mcdonald()` - McDonald's omega coefficient

### Utilities
* `save_to_excel_table()` - Export results to Excel
* `create_groups()` - Create item groups for factor definitions
* Model syntax generators for lavaan
