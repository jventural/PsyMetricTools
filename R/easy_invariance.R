easy_invariance <- function(model, data, estimator, ordered, ID.cat, group, levels_of_invariance, group.partial = NULL) {
  # Cargar las librerías necesarias
  library(dplyr)
  library(semTools)
  library(lavaan.mi)
  # Definir una función interna para ejecutar semTools::measEq.syntax con diferentes niveles de invarianza
  run_invariance <- function(model, data, estimator, ordered, ID.cat, group, group.equal, long.equal = NULL, group.partial = NULL) {
    result <- semTools::measEq.syntax(
      configural.model = model,
      data = data,
      estimator = estimator,
      ordered = ordered,
      parameterization = "theta",
      ID.fac = "std.lv",
      ID.cat = ID.cat,
      group = group,
      group.equal = group.equal,
      long.equal = long.equal,
      group.partial = group.partial,
      return.fit = TRUE
    )
    return(result)
  }

  # Lista para almacenar los resultados
  results <- list()

  # Ejecutar las diferentes pruebas de invarianza según los niveles proporcionados
  for (level in levels_of_invariance) {
    if (level == "configural") {
      results$data_configural <- run_invariance(model, data, estimator, ordered, ID.cat, group, "configural", group.partial = group.partial)
    } else if (level == "threshold") {
      results$data_threshold <- run_invariance(model, data, estimator, ordered, ID.cat, group, "thresholds", "thresholds", group.partial = group.partial)
    } else if (level == "metric") {
      results$data_metric <- run_invariance(model, data, estimator, ordered, ID.cat, group, c("thresholds", "loadings"), c("thresholds", "loadings"), group.partial = group.partial)
    } else if (level == "scalar") {
      results$data_scalar <- run_invariance(model, data, estimator, ordered, ID.cat, group, c("thresholds", "loadings", "intercepts"), c("thresholds", "loadings", "intercepts"), group.partial = group.partial)
    } else if (level == "strict") {
      results$data_strict <- run_invariance(model, data, estimator, ordered, ID.cat, group, c("thresholds", "loadings", "intercepts", "residuals"), c("thresholds", "loadings", "intercepts", "residuals"), group.partial = group.partial)
    }
  }

  # Comparar los modelos
  a <- semTools::compareFit(results$data_configural, results$data_threshold, results$data_metric, results$data_scalar, results$data_strict)

  # Función para extraer CFI y RMSEA
  extract_cfi_rmsea <- function(fit) {
    cfi <- round(fit[, "cfi.scaled"], 3)
    rmsea <- round(fit[, "rmsea.scaled"], 3)
    result <- data.frame(
      Model = rownames(fit),
      CFI = cfi,
      RMSEA = rmsea
    )
    return(result)
  }

  # Extraer los valores de ajuste
  fit_values <- extract_cfi_rmsea(a@fit)
  fit.diff_values <- extract_cfi_rmsea(a@fit.diff)

  # Ajustar los nombres de los modelos en fit_values y fit.diff_values para que coincidan con nested_data
  fit_values$Model <- c("data_configural", "data_threshold", "data_metric", "data_scalar", "data_strict")
  fit.diff_values$Model <- c("data_threshold", "data_metric", "data_scalar", "data_strict")

  # Unir los dataframes por la columna Model
  final_values <- fit_values %>%
    left_join(fit.diff_values, by = "Model", suffix = c("", "_diff"))

  # Convertir a@nested en un dataframe y agregar la columna Model
  nested_data <- as.data.frame(a@nested)
  nested_data$Model <- c("data_configural", "data_threshold", "data_metric", "data_scalar", "data_strict")

  # Combinar ambos dataframes en uno solo
  combined_data <- nested_data %>%
    left_join(final_values, by = "Model") %>%
    relocate(Model) %>%
    select(-AIC, -BIC) %>%
    mutate(Chisq_df = paste0(round(Chisq, 3), " (", Df, ")")) %>%
    mutate(`Chisq diff` = round(`Chisq diff`, 3)) %>%
    mutate(`Pr(>Chisq)` = round(`Pr(>Chisq)`, 3)) %>%
    relocate(Model, Chisq_df, `Chisq diff`, `Df diff`, `Pr(>Chisq)`, CFI, RMSEA, CFI_diff, RMSEA_diff) %>%
    select(-Df, -Chisq)

  # Renombrar las columnas
  colnames(combined_data) <- c("Model", "χ²(df)", "Δχ²", "Δdf", "p", "CFI", "RMSEA", "ΔCFI", "ΔRMSEA")

  # Retornar el dataframe combinado y los resultados individuales
  return(list(
    combined_data = combined_data,
    data_configural = results$data_configural,
    data_threshold = results$data_threshold,
    data_metric = results$data_metric,
    data_scalar = results$data_scalar,
    data_strict = results$data_strict
  ))
}
