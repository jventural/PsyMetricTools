summary_metrics <- function(results) {
  # Calcular las medianas de las métricas agrupadas por Algorithm y Correlation_Method
  averaged_results <- results %>%
    group_by(Algorithm, Correlation_Method, Sample_Size) %>%
    summarise(
      avg_sensitivity = median(sensitivity, na.rm = TRUE),
      avg_specificity = median(specificity, na.rm = TRUE),
      avg_precision = median(precision, na.rm = TRUE),
      avg_correlation = median(correlation, na.rm = TRUE),
      avg_abs_cor = median(abs_cor, na.rm = TRUE),
      avg_bias = median(bias, na.rm = TRUE),
      avg_TEFI = median(TEFI, na.rm = TRUE),
      avg_FDR = median(FDR, na.rm = TRUE),
      .groups = 'drop'
    )
  return(averaged_results)
}
