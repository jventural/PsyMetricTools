#' @title Plot CFA Stability with Confidence Intervals
#' @description Creates plots with confidence intervals for CFA fit indices.
#' @param resultados Data frame with bootstrap CFA results.
#' @param num_factors Number of factors (default 1).
#' @param indices_to_plot Indices to plot ("All" or vector of names).
#' @param indices_to_exclude Indices to exclude (default NULL).
#' @param fill_color Fill color for ribbons (default "skyblue").
#' @param alpha_ribbon Alpha for ribbons (default 0.2).
#' @param rename_factors Named vector to rename factors (default NULL).
#' @param ... Additional arguments.
#' @return A ggplot object with faceted confidence intervals.
#' @export
plot_cfa_stability_intervals <- function(resultados,
                                         num_factors = 1,
                                         indices_to_plot = "All",   # "All" o vector con nombres de índices
                                         indices_to_exclude = NULL, # Vector con nombres a excluir
                                         fill_color = "skyblue",
                                         alpha_ribbon = 0.2,
                                         rename_factors = NULL,     # Vector nombrado para renombrar, e.g. c("F1"="NuevoF1", "F2"="NuevoF2")
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
    "chisq.scaled", "df.scaled", "srmr", "wrmr",
    "cfi.scaled", "crmr", "tli.scaled", "rmsea.scaled",
    if (actual_num_factors > 0) paste0("F", 1:actual_num_factors) else NULL
  )

  # Filtrar solo los índices que existen en los datos
  available_indices <- intersect(available_indices, names(resultados))

  # 2. Si se indica "All", usamos todos los índices disponibles
  if (length(indices_to_plot) == 1 && indices_to_plot == "All") {
    indices_to_plot <- available_indices
  }
  # Preservamos el orden en que el usuario los ponga
  indices_to_plot <- unique(indices_to_plot)

  # 3. Excluir índices si se indica alguno
  if (!is.null(indices_to_exclude)) {
    indices_to_exclude <- unique(indices_to_exclude)
    final_indices <- setdiff(indices_to_plot, indices_to_exclude)
  } else {
    final_indices <- indices_to_plot
  }

  # 4. Asegurarnos de que 'Porcentaje' sea numérico
  resultados <- resultados %>%
    dplyr::mutate(Porcentaje = as.numeric(Porcentaje))

  # (Si tus porcentajes son 0.9, 0.8, ... y quieres 90, 80, ... descomenta la siguiente línea)
  # resultados <- resultados %>% dplyr::mutate(Porcentaje = Porcentaje * 100)

  # 5. Pasar a formato largo sólo las columnas de interés
  resultados_long <- resultados %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(final_indices),
      names_to = "Medida",
      values_to = "Valor"
    )

  # Renombrar los índices F según lo indicado por el usuario
  if (!is.null(rename_factors)) {
    # Convertir a character para poder renombrar
    resultados_long$Medida <- as.character(resultados_long$Medida)
    resultados_long$Medida <- ifelse(resultados_long$Medida %in% names(rename_factors),
                                     rename_factors[resultados_long$Medida],
                                     resultados_long$Medida)
    # Establecer los niveles del factor de acuerdo al orden de final_indices,
    # aplicando el renombrado a las variables F
    new_levels <- sapply(final_indices, function(x) {
      if (x %in% names(rename_factors)) {
        rename_factors[x]
      } else {
        x
      }
    })
    resultados_long$Medida <- factor(resultados_long$Medida, levels = new_levels)
  } else {
    resultados_long <- resultados_long %>%
      dplyr::mutate(Medida = factor(Medida, levels = final_indices))
  }

  # 6. Calcular intervalos (ej. percentiles 2.5% y 97.5%) y la media
  resultados_summary <- resultados_long %>%
    dplyr::group_by(Porcentaje, Medida) %>%
    dplyr::summarise(
      low  = quantile(Valor, probs = 0.025, na.rm = TRUE),
      mid  = mean(Valor, na.rm = TRUE),
      high = quantile(Valor, probs = 0.975, na.rm = TRUE),
      .groups = "drop"
    )

  # 7. Graficar
  p <- ggplot2::ggplot(resultados_summary, ggplot2::aes(x = Porcentaje, y = mid)) +
    ggplot2::geom_line(color = "grey28") +
    ggplot2::geom_point(color = "grey28") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = low, ymax = high),
                fill = fill_color,
                alpha = alpha_ribbon) +
    ggplot2::facet_wrap(~ Medida, scales = "free_y") +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "", y = "") +
    # Eje x descendente de 90 a 30 (si tus datos tienen esos valores)
    ggplot2::scale_x_reverse(breaks = c(90, 80, 70, 60, 50, 40, 30))

  return(p)
}
