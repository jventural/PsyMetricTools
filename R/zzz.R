# Global variables declaration to avoid R CMD check NOTEs
# These are column names used in tidyverse pipelines

#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

utils::globalVariables(c(
  # Common column names used in aes() and data manipulation
  "Value", "Variable", "Index", "Type", "Label", "Fit", "Model",
  "Mean", "Median", "SD", "CI_low", "CI_high", "Min", "Max",
  "Reference", "Direction",

  # boot_cfa and related functions
  "obs", "data", "fit_cfa1", "model_cfa1", "fit_measures1",
  "converged1", "validity1", "term", "value",
  "nobs", "estimator", "ngroups", "converged",
  "chisq", "df", "pvalue", "npar", "cfi", "tli", "rmsea",
  "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "wrmr", "crmr",
  "chisq.scaled", "df.scaled", "pvalue.scaled",
  "cfi.scaled", "tli.scaled", "rmsea.scaled",
  "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled",
  "aic", "bic", "AIC", "BIC",

  # boot_cfa_stability
  "Muestras", "Replica", "Porcentaje",

  # EFA functions
  "op", "rhs", "lhs", "est.std", "ci.lower", "ci.upper",
  "item", "factor", "complex", "loadings", "fit_efa",
  "Loading", "item_num", "low_loading", "fit_measures",

  # Invariance functions
  "Chisq", "Df", "Chisq_df", "Chisq diff", "Df diff", "Pr(>Chisq)",
  "CFI", "TLI", "RMSEA", "SRMR", "CRMR",
  "CFI_diff", "TLI_diff", "RMSEA_diff", "SRMR_diff", "CRMR_diff",

  # comparation_metric
  "Measure", "nombre_modelo",

  # Plot functions
  "x", "y", "fill", "color", "colour", "alpha", "group",
  "xmin", "xmax", "ymin", "ymax", "xintercept", "%", "yintercept",
  "x_base", "x_jitter", "x_pos", "stat_text",
  "violinwidth", "width", "xminv", "xmaxv",

  # stability plots
  "metric", "muestra", "valor", "lower", "upper",

  # General
  ".", "n", "name", "count", "conteo", "desc", "reorder",
  "Algorithm", "Correlation_Method", "Simulation", "Sample_Size",
  "TEFI", "Model_ID",

  # Filter and select helpers
  "everything", "starts_with", "ends_with", "where", "all_of", "any_of",

  # Likert plots
  "Response", "Count", "Percentage", "Item", "Category",
  "pos", "neg", "neutral", "Variables", "Var_Ptjes",
  "..count..", "Score", "Proportion", "Side",

  # Multi-class and performance
  "sensitivity", "specificity", "precision", "correlation",
  "abs_cor", "bias", "FDR",

  # Factor analysis
  "Loadings", "Factor", "Items", "loading", "se",
  "excluded_items", "final_items", "final_model",
  "Factores", "factors_data", "Porcentaje_factor",

  # Standardized solutions
  "std.all", "std.lv", "std.nox", "est", "pvalue",
  "Parameter", "Estimate", "SE", "Z", "P",
  "max_factor_index", "max_factor_value",

  # Reliability
  "omega", "alpha", "Rel1", "Rel2", "Rel3", "Rel4", "Rel5",
  "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10",

  # Preliminar_table
  "miss", "1", "5",

  # Get_Fit_Indices_Blavaan
  "fit1",

  # extract functions
  "Edad", "ID", "Indice", "MI", "Medida", "Medida_original",
  "Modification", "MuestraID", "Valor", "ci_lower", "ci_upper",
  "column", "corr", "factor1", "factor2", "flagged", "h2", "high",
  "low", "media", "mi", "mid", "pair1", "pair2", "promedio_MI", "u2",
  "last.warning",

  # openxlsx functions
  "addWorksheet", "createWorkbook", "saveWorkbook", "writeData",

  # Other
  "modifyList", "txtProgressBar"
))

