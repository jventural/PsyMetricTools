# PsyMetricTools 1.1.1

## New Features
* Added `boot_cfa_raincloud()` for modern raincloud plot visualizations
* Added `boot_cfa_density()` for bootstrap distribution density plots
* Added `plot_cfa_stability()` family of functions for CFA stability analysis
* Added `EFA_plot()` now accepts data.frames and lists in addition to lavaan objects

## Bug Fixes
* Fixed `boot_cfa_density()` error when combining lavaan.vector with double types
* Fixed `EFA_plot()` to support arbitrary factor names (not just f1, f2, etc.)
* Fixed `filtrar_aberrantes()` ID type mismatch when data has numeric ID column
* Fixed `boot_cfa_plot()` to display automatically when printed

## Improvements
* `boot_cfa_plot()` now returns an object with custom print method
* Improved input validation across all functions
* Better error messages for invalid inputs

# PsyMetricTools 1.1.0

## New Features
* Added `easy_invariance()` for measurement invariance analysis with ordinal data
* Added `boot_cfa()` for bootstrap CFA analysis
* Added `boot_efa()` for bootstrap EFA analysis
* Added `smote_multiclass()` for multi-class data balancing

## Improvements
* Updated documentation with examples
* Improved compatibility with lavaan 0.6+

# PsyMetricTools 1.0.0

* Initial CRAN submission
* Core functions for psychometric analysis
* EFA and CFA utilities
* Likert scale visualization tools
