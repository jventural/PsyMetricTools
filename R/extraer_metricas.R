extraer_metricas <- function(modelo_fit) {
  library(dplyr)
  library(tidyr)

  metricas <- tibble(Measure = modelo_fit$Measure, Value = modelo_fit$Value)

  metricas_filtradas <- metricas %>%
    filter(Measure %in% c("df", "chisq", "aic", "bic", "ebic1", "rmsea", "tli", "cfi")) %>%
    mutate(Measure = case_when(
      Measure == "aic" ~ "AIC",
      Measure == "bic" ~ "BIC",
      Measure == "ebic1" ~ "EBIC",
      Measure == "rmsea" ~ "RMSEA",
      Measure == "tli" ~ "TLI",
      Measure == "cfi" ~ "CFI",
      TRUE ~ Measure
    ))

  orden_metricas <- c("df", "chisq", "AIC", "BIC", "EBIC", "RMSEA", "TLI", "CFI")
  metricas_ordenadas <- metricas_filtradas %>%
    mutate(Measure = factor(Measure, levels = orden_metricas)) %>%
    arrange(Measure)

  nombre_modelo <- deparse(substitute(modelo_fit))
  metricas_df <- metricas_ordenadas %>%
    pivot_wider(names_from = Measure, values_from = Value) %>%
    mutate(Model = nombre_modelo) %>%
    dplyr::select(Model, everything())

  return(metricas_df)
}
