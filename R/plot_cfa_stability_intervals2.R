plot_cfa_stability_intervals2 <- function(resultados,
                                          num_factors = 1,
                                          alpha_ribbon = 0.2,
                                          rename_factors = NULL,
                                          ...) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)

  # 1. Lista completa de índices disponibles
  available_indices <- c(
    "chisq.scaled", "srmr", "wrmr",
    "cfi.scaled", "crmr", "tli.scaled", "rmsea.scaled",
    paste0("F", 1:num_factors)
  )

  # Usar todos los índices disponibles
  final_indices <- available_indices

  # 2. Asegurarse de que 'Porcentaje' sea numérico
  resultados <- resultados %>%
    mutate(Porcentaje = as.numeric(Porcentaje))

  # 3. Convertir a formato largo sólo las columnas de interés
  resultados_long <- resultados %>%
    pivot_longer(
      cols = all_of(final_indices),
      names_to = "Medida",
      values_to = "Valor"
    )

  # Guardar el nombre original antes de renombrar (para agrupar)
  resultados_long <- resultados_long %>%
    mutate(Medida_original = Medida)

  # 4. Renombrar factores si corresponde
  if (!is.null(rename_factors)) {
    resultados_long <- resultados_long %>%
      mutate(Medida = ifelse(Medida %in% names(rename_factors),
                             rename_factors[Medida],
                             Medida))
  }

  # Ajustar niveles en 'Medida' según el orden de final_indices (respetando el renombrado)
  if (!is.null(rename_factors)) {
    new_levels <- sapply(final_indices, function(x) {
      if (x %in% names(rename_factors)) {
        rename_factors[x]
      } else {
        x
      }
    }, USE.NAMES = FALSE)
  } else {
    new_levels <- final_indices
  }

  resultados_long <- resultados_long %>%
    mutate(Medida = factor(Medida, levels = new_levels))

  # 5. Calcular intervalos (percentiles 2.5%, 97.5%) y la media
  resultados_summary <- resultados_long %>%
    group_by(Porcentaje, Medida, Medida_original) %>%
    summarise(
      low  = quantile(Valor, probs = 0.025, na.rm = TRUE),
      mid  = mean(Valor, na.rm = TRUE),
      high = quantile(Valor, probs = 0.975, na.rm = TRUE),
      .groups = "drop"
    )

  # 6. Definir grupos basados en 'Medida_original'
  group_cfi_tli      <- c("cfi.scaled", "tli.scaled")             # Rango: 0 - 1
  group_rm_srmr_crmr <- c("rmsea.scaled", "srmr", "crmr")         # Rango: 0 - 0.20
  group_factors      <- paste0("F", 1:num_factors)                # Rango: 0 - 1

  data_grp1 <- resultados_summary %>% filter(Medida_original %in% group_cfi_tli)
  data_grp2 <- resultados_summary %>% filter(Medida_original %in% group_rm_srmr_crmr)
  data_grp3 <- resultados_summary %>% filter(Medida_original %in% group_factors)

  # 7. Función auxiliar para generar un plot
  create_group_plot <- function(data, y_min, y_max, legend_label = "Index") {
    if (nrow(data) == 0) return(NULL)

    ggplot(data, aes(x = Porcentaje, y = mid, color = Medida, fill = Medida)) +
      geom_line() +
      geom_point() +
      geom_ribbon(aes(ymin = low, ymax = high), alpha = alpha_ribbon, color = NA) +
      labs(x = "", y = "", color = legend_label, fill = legend_label) +
      scale_y_continuous(limits = c(y_min, y_max)) +
      scale_x_reverse(breaks = c(90, 80, 70, 60, 50, 40, 30)) +
      theme_minimal() +
      theme(legend.position = "bottom")
  }

  # 8. Crear cada panel:
  # - Panel 1 (cfi.scaled, tli.scaled) -> leyenda "Index"
  # - Panel 2 (rmsea.scaled, srmr, crmr) -> leyenda "Index"
  # - Panel 3 (Factores) -> leyenda "Reliability"
  p1 <- create_group_plot(data_grp1, 0, 1,     legend_label = "Index")
  p2 <- create_group_plot(data_grp2, 0, 0.20,  legend_label = "Index")
  p3 <- create_group_plot(data_grp3, 0, 1,     legend_label = "Reliability")

  # 9. Retornar los tres paneles en una lista
  list(
    cfi_tli      = p1,
    rm_srmr_crmr = p2,
    factors      = p3
  )
}
