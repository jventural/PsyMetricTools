#' @name comparation_metric
#' @export
comparation_metric <- function(...) {
  # Capturar los modelos y sus nombres
  modelos_fit <- list(...)
  nombres_modelos <- names(modelos_fit)

  extraer_metricas <- function(modelo_fit, nombre_modelo) {
    metricas <- tibble::tibble(Measure = modelo_fit$Measure, Value = modelo_fit$Value)

    metricas_filtradas <- metricas %>%
      dplyr::filter(Measure %in% c("df", "chisq", "aic", "bic", "ebic", "rmsea", "tli", "cfi")) %>%
      dplyr::mutate(Measure = dplyr::case_when(
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
      dplyr::mutate(Measure = factor(Measure, levels = orden_metricas)) %>%
      dplyr::arrange(Measure)

    metricas_df <- metricas_ordenadas %>%
      tidyr::pivot_wider(names_from = Measure, values_from = Value) %>%
      dplyr::mutate(Model = nombre_modelo) %>%
      dplyr::select(Model, everything())

    return(metricas_df)
  }

  resultados <- mapply(extraer_metricas, modelos_fit, nombres_modelos, SIMPLIFY = FALSE)
  resultados_combinados <- dplyr::bind_rows(resultados)

  return(resultados_combinados)
}
