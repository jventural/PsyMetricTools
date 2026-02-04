#' @title Plot CFA Stability with Grouped Intervals
#' @description Creates grouped interval plots for CFA fit indices.
#' @param resultados Data frame with bootstrap CFA results.
#' @param num_factors Number of factors (default 1).
#' @param alpha_ribbon Alpha for ribbons (default 0.2).
#' @param rename_factors Named vector to rename factors (default NULL).
#' @param ... Additional arguments.
#' @return A list with three ggplot panels for different index groups.
#' @export
plot_cfa_stability_intervals2 <- function(resultados,
                                          num_factors = 1,
                                          alpha_ribbon = 0.2,
                                          rename_factors = NULL,
                                          ...) {

  # Detectar columnas de omega automáticamente
  fit_cols <- c("chisq.scaled", "df.scaled", "srmr", "wrmr", "cfi.scaled", "crmr", "tli.scaled", "rmsea.scaled")
  meta_cols <- c("Porcentaje", "Replica", "Muestras")
  all_cols <- names(resultados)
  omega_cols <- setdiff(all_cols, c(fit_cols, meta_cols))

  # Renombrar columnas de omega a F1, F2, etc.
  if (length(omega_cols) > 0) {
    new_omega_names <- paste0("F", seq_along(omega_cols))
    names(resultados)[names(resultados) %in% omega_cols] <- new_omega_names
    actual_num_factors <- length(omega_cols)
  } else {
    actual_num_factors <- 0
  }

  # 1. Lista completa de índices disponibles
  available_indices <- c(
    "chisq.scaled", "srmr", "wrmr",
    "cfi.scaled", "crmr", "tli.scaled", "rmsea.scaled",
    if (actual_num_factors > 0) paste0("F", 1:actual_num_factors) else NULL
  )

  # Usar todos los índices disponibles
  final_indices <- intersect(available_indices, names(resultados))

  # 2. Asegurarse de que 'Porcentaje' sea numérico
  resultados <- resultados %>%
    dplyr::mutate(Porcentaje = as.numeric(Porcentaje))

  # 3. Convertir a formato largo sólo las columnas de interés
  resultados_long <- resultados %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(final_indices),
      names_to = "Medida",
      values_to = "Valor"
    )

  # Guardar el nombre original antes de renombrar (para agrupar)
  resultados_long <- resultados_long %>%
    dplyr::mutate(Medida_original = Medida)

  # 4. Renombrar factores si corresponde
  if (!is.null(rename_factors)) {
    resultados_long <- resultados_long %>%
      dplyr::mutate(Medida = ifelse(Medida %in% names(rename_factors),
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
    dplyr::mutate(Medida = factor(Medida, levels = new_levels))

  # 5. Calcular intervalos (percentiles 2.5%, 97.5%) y la media
  resultados_summary <- resultados_long %>%
    dplyr::group_by(Porcentaje, Medida, Medida_original) %>%
    dplyr::summarise(
      low  = quantile(Valor, probs = 0.025, na.rm = TRUE),
      mid  = mean(Valor, na.rm = TRUE),
      high = quantile(Valor, probs = 0.975, na.rm = TRUE),
      .groups = "drop"
    )

  # 6. Definir grupos basados en 'Medida_original'
  group_cfi_tli      <- c("cfi.scaled", "tli.scaled")             # Rango: 0 - 1
  group_rm_srmr_crmr <- c("rmsea.scaled", "srmr", "crmr")         # Rango: 0 - 0.20
  group_factors      <- if (actual_num_factors > 0) paste0("F", 1:actual_num_factors) else character(0)  # Rango: 0 - 1

  data_grp1 <- resultados_summary %>% dplyr::filter(Medida_original %in% group_cfi_tli)
  data_grp2 <- resultados_summary %>% dplyr::filter(Medida_original %in% group_rm_srmr_crmr)
  data_grp3 <- resultados_summary %>% dplyr::filter(Medida_original %in% group_factors)

  # 7. Función auxiliar para generar un plot
  create_group_plot <- function(data, y_min, y_max, legend_label = "Index") {
    if (nrow(data) == 0) return(NULL)

    ggplot2::ggplot(data, ggplot2::aes(x = Porcentaje, y = mid, color = Medida, fill = Medida)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = low, ymax = high), alpha = alpha_ribbon, color = NA) +
      ggplot2::labs(x = "", y = "", color = legend_label, fill = legend_label) +
      ggplot2::scale_y_continuous(limits = c(y_min, y_max)) +
      ggplot2::scale_x_reverse(breaks = c(90, 80, 70, 60, 50, 40, 30)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom")
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
