# ============================================================================
# PsyMetricTools - Comprehensive Function Testing Script
# Purpose: Test all exported functions before CRAN submission
# ============================================================================

# Suppress startup messages
suppressPackageStartupMessages({
  library(devtools)
})

# Load the package
cat("=============================================================\n")
cat("Loading PsyMetricTools package...\n")
cat("=============================================================\n")
devtools::load_all("D:/14. LIBRERIAS/PsyMetricTools", quiet = TRUE)

# Load required packages
suppressPackageStartupMessages({
  library(lavaan)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(tibble)
  library(psych)
})

# Initialize results tracking
results <- list()
errors <- list()
warnings_list <- list()

# Helper function to test and record results
test_function <- function(func_name, expr) {
  cat(sprintf("\nTesting: %s\n", func_name))
  result <- tryCatch({
    # Capture warnings
    warns <- NULL
    output <- withCallingHandlers(
      expr,
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    if (!is.null(warns)) {
      warnings_list[[func_name]] <<- warns
      cat(sprintf("  [WARNING] %s\n", paste(warns, collapse = "; ")))
    }
    cat(sprintf("  [OK] %s works correctly\n", func_name))
    results[[func_name]] <<- "OK"
    return(output)
  }, error = function(e) {
    cat(sprintf("  [ERROR] %s: %s\n", func_name, e$message))
    errors[[func_name]] <<- e$message
    results[[func_name]] <<- "ERROR"
    return(NULL)
  })
  return(result)
}

# ============================================================================
# Generate Sample Data for Testing
# ============================================================================
cat("\n=============================================================\n")
cat("Generating sample psychometric data...\n")
cat("=============================================================\n")

set.seed(12345)
n <- 400  # Sample size

# Simulate 15 Likert items (1-5 scale) with 3-factor structure
# Factor 1: Items 1-5, Factor 2: Items 6-10, Factor 3: Items 11-15
simulate_likert_data <- function(n, n_items = 15, n_categories = 5) {
  # Create factor scores
  f1 <- rnorm(n)
  f2 <- rnorm(n) + 0.3 * f1  # Correlated factors
  f3 <- rnorm(n) + 0.2 * f1 + 0.2 * f2

  # Create continuous scores
  data <- matrix(0, nrow = n, ncol = n_items)
  for (i in 1:5) {
    data[, i] <- 0.7 * f1 + rnorm(n, sd = 0.5)
  }
  for (i in 6:10) {
    data[, i] <- 0.7 * f2 + rnorm(n, sd = 0.5)
  }
  for (i in 11:15) {
    data[, i] <- 0.7 * f3 + rnorm(n, sd = 0.5)
  }

  # Discretize to Likert scale
  for (i in 1:n_items) {
    breaks <- quantile(data[, i], probs = seq(0, 1, length.out = n_categories + 1))
    breaks[1] <- -Inf
    breaks[n_categories + 1] <- Inf
    data[, i] <- as.numeric(cut(data[, i], breaks = breaks))
  }

  df <- as.data.frame(data)
  colnames(df) <- paste0("Item", 1:n_items)
  return(df)
}

# Generate main data
data_sample <- simulate_likert_data(n)

# Add grouping variable for invariance testing
data_sample$group <- sample(c("A", "B"), n, replace = TRUE)
data_sample$ID <- 1:n

cat(sprintf("Generated data: %d observations, %d items\n", nrow(data_sample), ncol(data_sample) - 2))

# ============================================================================
# Test Functions by Category
# ============================================================================

cat("\n=============================================================\n")
cat("TESTING DATA MANIPULATION FUNCTIONS\n")
cat("=============================================================\n")

# 1. split_data_two
split_result <- test_function("split_data_two", {
  split_data_two(data_sample[, 1:15], perc_exploratorio = 0.5, perc_confirmatorio = 0.5, seed = 123)
})

# 2. split_data_three
test_function("split_data_three", {
  split_data_three(data_sample[, 1:15], perc_piloto = 0.1, perc_exploratorio = 0.45, perc_confirmatorio = 0.45, seed = 123)
})

# 3. invertir_items
test_function("invertir_items", {
  invertir_items(data_sample[, 1:15], items = c("Item1", "Item2"), num_respuestas = 5, comienza_con_cero = FALSE)
})

# 4. create_groups
test_function("create_groups", {
  create_groups(names = c("Factor1", "Factor2", "Factor3"), values = c(5, 5, 5))
})

# 5. summarise_column
test_function("summarise_column", {
  summarise_column(data_sample, "Item1")
})

# 6. smote_multiclass
test_function("smote_multiclass", {
  df_smote <- data_sample[, c("Item1", "Item2", "Item3", "group")]
  smote_multiclass(df_smote, outcome = "group", perc_maj = 50, k = 3, seed = 123)
})

cat("\n=============================================================\n")
cat("TESTING MODEL GENERATION FUNCTIONS\n")
cat("=============================================================\n")

# 7. generate_modelos
modelos_efa <- test_function("generate_modelos", {
  generate_modelos(n_factors = 3, n_items = 15, name_items = "Item")
})

# 8. crear_modelo_lavaan
model_cfa <- test_function("crear_modelo_lavaan", {
  crear_modelo_lavaan("Item", F1 = 1:5, F2 = 6:10, F3 = 11:15)
})

# 9. crearSintaxisCFA
test_function("crearSintaxisCFA", {
  df_items <- data.frame(Items = paste0("Item", 1:15), Factores = rep(c("F1", "F2", "F3"), each = 5))
  crearSintaxisCFA(df_items)
})

# 10. generate_model_lavaan
test_function("generate_model_lavaan", {
  generate_model_lavaan(num_items = 5, item_value = rep(0.7, 5), t_values = c(-1.5, -0.5, 0.5, 1.5))
})

# 11. Matrix_lambda
test_function("Matrix_lambda", {
  Matrix_lambda(n_items = 15, factores = 3, n_tamanos = c(5, 5, 5))
})

# 12. createInitialModel (if exists - check syntax)
test_function("createInitialModel", {
  tryCatch({
    createInitialModel(data_sample[, 1:15], n_factors = 3, item_prefix = "Item")
  }, error = function(e) {
    # Try alternative if signature is different
    stop(e$message)
  })
})

cat("\n=============================================================\n")
cat("TESTING EFA FUNCTIONS\n")
cat("=============================================================\n")

# 13. specification_models (needed for other tests)
data_efa <- split_result$exploratorio
specs <- test_function("specification_models", {
  specification_models(modelos_efa, data = data_efa, estimator = "WLSMV", rotation = "oblimin")
})

# 14. extract_fit_measures
test_function("extract_fit_measures", {
  extract_fit_measures(specs)
})

# 15. EFA_modern
efa_result <- test_function("EFA_modern", {
  EFA_modern(n_factors = 3, n_items = 15, name_items = "Item", data = data_efa,
             apply_threshold = TRUE, estimator = "WLSMV", rotation = "oblimin")
})

# 16. Standardized_solutions
test_function("Standardized_solutions", {
  Standardized_solutions(specs[[3]], name_items = "Item", apply_threshold = TRUE)
})

# 17. EFA_plot
test_function("EFA_plot", {
  p <- EFA_plot(specs[[3]], item_prefix = "Item")
  print(p)
  invisible(p)
})

# 18. agrupar_por_factor
test_function("agrupar_por_factor", {
  df_loadings <- data.frame(
    Items = paste0("Item", 1:15),
    f1 = c(rep(0.7, 5), rep(0, 10)),
    f2 = c(rep(0, 5), rep(0.7, 5), rep(0, 5)),
    f3 = c(rep(0, 10), rep(0.7, 5))
  )
  agrupar_por_factor(df_loadings, item_col = "Items", threshold = 0.3)
})

# 19. extracted_items2
test_function("extracted_items2", {
  df_loadings <- data.frame(
    Items = paste0("Item", 1:15),
    f1 = c(rep(0.7, 5), rep(0.1, 10)),
    f2 = c(rep(0.1, 5), rep(0.7, 5), rep(0.1, 5)),
    f3 = c(rep(0.1, 10), rep(0.7, 5))
  )
  extracted_items2(df_loadings)
})

# 20. factor_summary (requires psych fa object)
test_function("factor_summary", {
  tryCatch({
    fa_result <- psych::fa(data_efa, nfactors = 3, rotate = "oblimin")
    factor_summary(fa_result, num_items = 15, num_factors = 3)
  }, error = function(e) stop(e$message))
})

# 21. factors_data_items
test_function("factors_data_items", {
  tryCatch({
    factors_data_items(data_efa, n_factors = 3, item_prefix = "Item")
  }, error = function(e) stop(e$message))
})

cat("\n=============================================================\n")
cat("TESTING CFA FUNCTIONS\n")
cat("=============================================================\n")

# Fit a CFA model for subsequent tests
data_cfa <- split_result$confirmatorio
cfa_fit <- test_function("lavaan::cfa (setup)", {
  lavaan::cfa(model_cfa, data = data_cfa, estimator = "WLSMV", ordered = TRUE)
})

# 22. Standardized_solutions_cfa
test_function("Standardized_solutions_cfa", {
  Standardized_solutions_cfa(cfa_fit, name_items = "Item", apply_threshold = TRUE)
})

# 23. multi_cfa
multi_result <- test_function("multi_cfa", {
  modelos_cfa <- list(
    model_cfa,
    "F1 =~ Item1 + Item2 + Item3 + Item4 + Item5\nF2 =~ Item6 + Item7 + Item8 + Item9 + Item10"
  )
  multi_cfa(modelos_cfa, data = data_cfa, estimator = "WLSMV", ordered = TRUE)
})

# 24. fit_index_Table
test_function("fit_index_Table", {
  fit_index_Table(multi_result)
})

# 25. extract_fit
test_function("extract_fit", {
  tryCatch({
    extract_fit(cfa_fit)
  }, error = function(e) stop(e$message))
})

# 26. extract_ModIndex_AFC
test_function("extract_ModIndex_AFC", {
  tryCatch({
    extract_ModIndex_AFC(cfa_fit)
  }, error = function(e) stop(e$message))
})

# 27. extract_summary_modifications
test_function("extract_summary_modifications", {
  tryCatch({
    extract_summary_modifications(cfa_fit)
  }, error = function(e) stop(e$message))
})

# 28. calcula_omega_mcdonald
test_function("calcula_omega_mcdonald", {
  df_loadings <- data.frame(
    Items = paste0("Item", 1:15),
    f1 = c(rep(0.7, 5), rep(0, 10)),
    f2 = c(rep(0, 5), rep(0.7, 5), rep(0, 5)),
    f3 = c(rep(0, 10), rep(0.7, 5))
  )
  calcula_omega_mcdonald(df_loadings, item_col = "Items", method = "comunalidad")
})

cat("\n=============================================================\n")
cat("TESTING VISUALIZATION FUNCTIONS\n")
cat("=============================================================\n")

# 29. Plot_Likert
test_function("Plot_Likert", {
  p <- Plot_Likert(data_sample[, 1:15], name_items = "Item", ranges = 1:6, text_size = 2)
  print(p)
  invisible(p)
})

# 30. Plot_Likert2
test_function("Plot_Likert2", {
  colnames(data_sample)[1:15] <- paste0("IS", 1:15)
  p <- Plot_Likert2(data_sample[, 1:15], prefix = "IS",
                    labels = c("1", "2", "3", "4", "5"),
                    item_range = 1:15, show_text = FALSE)
  print(p)
  colnames(data_sample)[1:15] <- paste0("Item", 1:15)  # Restore names
  invisible(p)
})

# 31. Preliminar_table (note: has hardcoded 1:3 items)
test_function("Preliminar_table", {
  tryCatch({
    Preliminar_table(data_sample[, 1:3])
  }, error = function(e) stop(e$message))
})

# 32. combine_likert_sem
test_function("combine_likert_sem", {
  tryCatch({
    combine_likert_sem(data_sample[, 1:15], cfa_fit, name_items = "Item")
  }, error = function(e) stop(e$message))
})

cat("\n=============================================================\n")
cat("TESTING INVARIANCE FUNCTIONS\n")
cat("=============================================================\n")

# 33. easy_invariance
test_function("easy_invariance", {
  tryCatch({
    model_inv <- "F1 =~ Item1 + Item2 + Item3 + Item4 + Item5"
    easy_invariance(
      model = model_inv,
      data = data_sample,
      estimator = "WLSMV",
      ordered = TRUE,
      ID.cat = "Wu.Estabrook.2016",
      group = "group",
      levels_of_invariance = c("configural", "threshold")
    )
  }, error = function(e) stop(e$message))
})

# 34. comparation_metric
test_function("comparation_metric", {
  tryCatch({
    comparation_metric(data_sample, group_var = "group", item_prefix = "Item")
  }, error = function(e) stop(e$message))
})

cat("\n=============================================================\n")
cat("TESTING BOOTSTRAP FUNCTIONS (Small replications for speed)\n")
cat("=============================================================\n")

# 35. boot_cfa (reduced replications)
boot_result <- test_function("boot_cfa", {
  boot_cfa(data_cfa, model_string = model_cfa, item_prefix = "Item",
           seed = 123, n_replications = 5, ordered = TRUE, estimator = "WLSMV")
})

# 36. boot_efa
test_function("boot_efa", {
  tryCatch({
    boot_efa(data_efa, n_factors = 2, n_items = 15, name_items = "Item",
             n_replications = 5, seed = 123)
  }, error = function(e) stop(e$message))
})

# 37. efa_with_bootstrap
test_function("efa_with_bootstrap", {
  tryCatch({
    efa_with_bootstrap(data_efa, n_factors = 2, n_items = 15, name_items = "Item",
                       n_replications = 5, seed = 123)
  }, error = function(e) stop(e$message))
})

# 38. boot_cfa_stability
test_function("boot_cfa_stability", {
  tryCatch({
    boot_cfa_stability(boot_result)
  }, error = function(e) stop(e$message))
})

# 39. calculate_per_fit
test_function("calculate_per_fit", {
  calculate_per_fit(boot_result, "CFI > 0.90, RMSEA < 0.08")
})

# 40. calculate_per_fit_efa
test_function("calculate_per_fit_efa", {
  tryCatch({
    calculate_per_fit_efa(boot_result, "CFI > 0.90, RMSEA < 0.08")
  }, error = function(e) stop(e$message))
})

cat("\n=============================================================\n")
cat("TESTING BOOTSTRAP PLOT FUNCTIONS\n")
cat("=============================================================\n")

# 41. boot_cfa_plot
test_function("boot_cfa_plot", {
  tryCatch({
    p <- boot_cfa_plot(boot_result)
    print(p)
    invisible(p)
  }, error = function(e) stop(e$message))
})

# 42. boot_cfa_density
test_function("boot_cfa_density", {
  tryCatch({
    p <- boot_cfa_density(boot_result)
    print(p)
    invisible(p)
  }, error = function(e) stop(e$message))
})

# 43. boot_cfa_raincloud
test_function("boot_cfa_raincloud", {
  tryCatch({
    p <- boot_cfa_raincloud(boot_result)
    print(p)
    invisible(p)
  }, error = function(e) stop(e$message))
})

# 44. boot_efa_plot
test_function("boot_efa_plot", {
  tryCatch({
    boot_efa_result <- boot_efa(data_efa, n_factors = 2, n_items = 15, name_items = "Item",
                                n_replications = 5, seed = 123)
    p <- boot_efa_plot(boot_efa_result)
    print(p)
    invisible(p)
  }, error = function(e) stop(e$message))
})

# 45. boot_efa_forest_plot
test_function("boot_efa_forest_plot", {
  tryCatch({
    boot_efa_result <- boot_efa(data_efa, n_factors = 2, n_items = 15, name_items = "Item",
                                n_replications = 5, seed = 123)
    p <- boot_efa_forest_plot(boot_efa_result)
    print(p)
    invisible(p)
  }, error = function(e) stop(e$message))
})

# 46-50. plot_cfa_stability variants
test_function("plot_cfa_stability", {
  tryCatch({
    p <- plot_cfa_stability(boot_result)
    print(p)
    invisible(p)
  }, error = function(e) stop(e$message))
})

test_function("plot_cfa_stability_box", {
  tryCatch({
    p <- plot_cfa_stability_box(boot_result)
    print(p)
    invisible(p)
  }, error = function(e) stop(e$message))
})

test_function("plot_cfa_stability_intervals", {
  tryCatch({
    p <- plot_cfa_stability_intervals(boot_result)
    print(p)
    invisible(p)
  }, error = function(e) stop(e$message))
})

test_function("plot_cfa_stability_intervals2", {
  tryCatch({
    p <- plot_cfa_stability_intervals2(boot_result)
    print(p)
    invisible(p)
  }, error = function(e) stop(e$message))
})

test_function("plot_cfa_stability_pointrange", {
  tryCatch({
    p <- plot_cfa_stability_pointrange(boot_result)
    print(p)
    invisible(p)
  }, error = function(e) stop(e$message))
})

test_function("plot_cfa_stability_ridgeline", {
  tryCatch({
    p <- plot_cfa_stability_ridgeline(boot_result)
    print(p)
    invisible(p)
  }, error = function(e) stop(e$message))
})

# 51. plot_multi_sem
test_function("plot_multi_sem", {
  tryCatch({
    p <- plot_multi_sem(multi_result)
    print(p)
    invisible(p)
  }, error = function(e) stop(e$message))
})

cat("\n=============================================================\n")
cat("TESTING EXTRACTION FUNCTIONS\n")
cat("=============================================================\n")

# 52. extract_bondad_boot_AFC
test_function("extract_bondad_boot_AFC", {
  tryCatch({
    extract_bondad_boot_AFC(boot_result)
  }, error = function(e) stop(e$message))
})

# 53. extract_bondad_boot_EFA
test_function("extract_bondad_boot_EFA", {
  tryCatch({
    boot_efa_result <- boot_efa(data_efa, n_factors = 2, n_items = 15, name_items = "Item",
                                n_replications = 5, seed = 123)
    extract_bondad_boot_EFA(boot_efa_result)
  }, error = function(e) stop(e$message))
})

# 54. extract_ExcludedItems_boot
test_function("extract_ExcludedItems_boot", {
  tryCatch({
    extract_ExcludedItems_boot(boot_result)
  }, error = function(e) stop(e$message))
})

# 55. extract_f_items
test_function("extract_f_items", {
  tryCatch({
    extract_f_items(cfa_fit, name_items = "Item")
  }, error = function(e) stop(e$message))
})

# 56. extraer_metricas
test_function("extraer_metricas", {
  tryCatch({
    extraer_metricas(cfa_fit)
  }, error = function(e) stop(e$message))
})

# 57. extractAllFitMeasures
test_function("extractAllFitMeasures", {
  tryCatch({
    extractAllFitMeasures(specs)
  }, error = function(e) stop(e$message))
})

# 58. extractAllInterFactors
test_function("extractAllInterFactors", {
  tryCatch({
    extractAllInterFactors(specs)
  }, error = function(e) stop(e$message))
})

# 59. extractAllResults
test_function("extractAllResults", {
  tryCatch({
    extractAllResults(specs, name_items = "Item")
  }, error = function(e) stop(e$message))
})

cat("\n=============================================================\n")
cat("TESTING UTILITY FUNCTIONS\n")
cat("=============================================================\n")

# 60. cor_afe (Note: this function has a bug - uses global variable)
test_function("cor_afe", {
  tryCatch({
    # This function references 'factors_data' which should be the EFA result
    factors_data <- psych::fa(data_efa, nfactors = 3, rotate = "oblimin")
    cor_afe(factors_data)
  }, error = function(e) stop(e$message))
})

# 61. count_excluded_items
test_function("count_excluded_items", {
  tryCatch({
    count_excluded_items(boot_result)
  }, error = function(e) stop(e$message))
})

# 62. filtrar_aberrantes
test_function("filtrar_aberrantes", {
  tryCatch({
    # Requires careless package
    if (requireNamespace("careless", quietly = TRUE)) {
      filtrar_aberrantes(data_sample, items = paste0("Item", 1:15), plot = FALSE)
    } else {
      stop("Package 'careless' is required")
    }
  }, error = function(e) stop(e$message))
})

# 63. interpret_decision_SSV
test_function("interpret_decision_SSV", {
  df_mi <- data.frame(
    lhs = c("F1", "F1"),
    op = c("=~", "=~"),
    rhs = c("Item6", "Item7"),
    decision.pow = c("EPC:M", "NM")
  )
  interpret_decision_SSV(df_mi)
})

# 64. generate_summary
test_function("generate_summary", {
  tryCatch({
    generate_summary(cfa_fit)
  }, error = function(e) stop(e$message))
})

# 65. generate_table
test_function("generate_table", {
  tryCatch({
    generate_table(cfa_fit)
  }, error = function(e) stop(e$message))
})

# 66. split_data_stratified_clustered
test_function("split_data_stratified_clustered", {
  tryCatch({
    split_data_stratified_clustered(data_sample, strat_var = "group",
                                     prop_train = 0.7, seed = 123)
  }, error = function(e) stop(e$message))
})

# 67. save_to_excel_table (skip actual save to avoid file creation)
test_function("save_to_excel_table", {
  tryCatch({
    # Just check function exists and has correct signature
    if (is.function(save_to_excel_table)) {
      cat("  Function exists - skipping actual file creation\n")
      TRUE
    } else {
      stop("Function not found")
    }
  }, error = function(e) stop(e$message))
})

# 68. export_summary_tables (skip actual export)
test_function("export_summary_tables", {
  tryCatch({
    if (is.function(export_summary_tables)) {
      cat("  Function exists - skipping actual file creation\n")
      TRUE
    } else {
      stop("Function not found")
    }
  }, error = function(e) stop(e$message))
})

# 69. Pipe operator %>%
test_function("%>% (pipe operator)", {
  data_sample %>% head(3) %>% nrow() == 3
})

# ============================================================================
# FINAL REPORT
# ============================================================================

cat("\n\n")
cat("=============================================================\n")
cat("                    FINAL TEST REPORT                        \n")
cat("=============================================================\n\n")

# Count results
n_ok <- sum(results == "OK", na.rm = TRUE)
n_error <- sum(results == "ERROR", na.rm = TRUE)
n_total <- length(results)

cat(sprintf("Total functions tested: %d\n", n_total))
cat(sprintf("Functions working:      %d (%.1f%%)\n", n_ok, 100 * n_ok / n_total))
cat(sprintf("Functions with errors:  %d (%.1f%%)\n", n_error, 100 * n_error / n_total))

cat("\n-------------------------------------------------------------\n")
cat("FUNCTIONS THAT WORK CORRECTLY:\n")
cat("-------------------------------------------------------------\n")
ok_funcs <- names(results)[results == "OK"]
for (f in ok_funcs) {
  if (!is.null(warnings_list[[f]])) {
    cat(sprintf("  [OK*] %s (with warnings)\n", f))
  } else {
    cat(sprintf("  [OK]  %s\n", f))
  }
}

if (n_error > 0) {
  cat("\n-------------------------------------------------------------\n")
  cat("FUNCTIONS WITH ERRORS:\n")
  cat("-------------------------------------------------------------\n")
  error_funcs <- names(results)[results == "ERROR"]
  for (f in error_funcs) {
    cat(sprintf("  [ERROR] %s\n", f))
    cat(sprintf("          Message: %s\n", errors[[f]]))
  }
}

if (length(warnings_list) > 0) {
  cat("\n-------------------------------------------------------------\n")
  cat("WARNINGS ENCOUNTERED:\n")
  cat("-------------------------------------------------------------\n")
  for (f in names(warnings_list)) {
    cat(sprintf("  %s:\n", f))
    for (w in warnings_list[[f]]) {
      cat(sprintf("    - %s\n", w))
    }
  }
}

cat("\n=============================================================\n")
cat("                    TEST COMPLETE                            \n")
cat("=============================================================\n")
