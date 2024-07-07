comparation_metric <- function(...) {
  library(dplyr)
  library(tidyr)

  # Capturar los modelos y sus nombres
  modelos_fit <- list(...)
  nombres_modelos <- names(modelos_fit)

  extraer_metricas <- function(modelo_fit, nombre_modelo) {
    metricas <- tibble(Measure = modelo_fit$Measure, Value = modelo_fit$Value)

    metricas_filtradas <- metricas %>%
      filter(Measure %in% c("df", "chisq", "aic", "bic", "ebic", "rmsea", "tli", "cfi")) %>%
      mutate(Measure = case_when(
        Measure == "aic" ~ "AIC",
        Measure == "bic" ~ "BIC",
        Measure == "ebic" ~ "EBIC",
        Measure == "rmsea" ~ "RMSEA",
        Measure == "tli" ~ "TLI",
        Measure == "cfi" ~ "CFI",
        TRUE ~ Measure
      ))

    orden_metricas <- c("df", "chisq", "AIC", "BIC", "EBIC", "RMSEA", "TLI", "CFI")
    metricas_ordenadas <- metricas_filtradas %>%
      mutate(Measure = factor(Measure, levels = orden_metricas)) %>%
      arrange(Measure)

    metricas_df <- metricas_ordenadas %>%
      pivot_wider(names_from = Measure, values_from = Value) %>%
      mutate(Model = nombre_modelo) %>%
      select(Model, everything())

    return(metricas_df)
  }

  resultados <- mapply(extraer_metricas, modelos_fit, nombres_modelos, SIMPLIFY = FALSE)
  resultados_combinados <- bind_rows(resultados)

  return(resultados_combinados)
}
