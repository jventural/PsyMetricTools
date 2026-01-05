#' @title Grafico Pointrange de Estabilidad CFA
#' @description Crea graficos pointrange (punto + intervalo de confianza) para
#'   visualizar la estabilidad de indices de ajuste y fiabilidad segun tamano muestral.
#' @param resultados Data frame con resultados de boot_cfa_stability.
#' @param indices Vector de indices a graficar. Por defecto c("cfi.scaled", "tli.scaled",
#'   "rmsea.scaled", "srmr"). Usar "All" para todos los disponibles.
#' @param ci_level Nivel de confianza para los intervalos (por defecto 0.95).
#' @param point_size Tamano de los puntos (por defecto 2.5).
#' @param line_width Grosor de las lineas del intervalo (por defecto 0.8).
#' @param hline_values Lista nombrada con valores de referencia para lineas horizontales.
#' @param hline_color Color de las lineas de referencia (por defecto "red").
#' @param hline_linetype Tipo de linea de referencia (por defecto "dashed").
#' @param facet_scales Escalas para facetas: "free_y", "fixed", etc. (por defecto "free_y").
#' @param color_palette Paleta de colores. Por defecto usa escala de grises.
#' @param theme_style Estilo del tema: "minimal", "bw", "classic" (por defecto "minimal").
#' @return Un objeto ggplot.
#' @export
#' @examples
#' \dontrun{
#' plot_cfa_stability_pointrange(resultado_stability)
#' plot_cfa_stability_pointrange(resultado_stability, indices = c("cfi.scaled", "rmsea.scaled"))
#' }
plot_cfa_stability_pointrange <- function(resultados,
                                          indices = c("cfi.scaled", "tli.scaled", "rmsea.scaled", "srmr"),
                                          ci_level = 0.95,
                                          point_size = 2.5,
                                          line_width = 0.8,
                                          hline_values = list(
                                            cfi.scaled = 0.95,
                                            tli.scaled = 0.95,
                                            rmsea.scaled = 0.08,
                                            srmr = 0.08,
                                            crmr = 0.05
                                          ),
                                          hline_color = "red",
                                          hline_linetype = "dashed",
                                          facet_scales = "free_y",
                                          color_palette = NULL,
                                          theme_style = "minimal") {

  # Detectar columnas de omega automaticamente

  fit_cols <- c("chisq.scaled", "df.scaled", "srmr", "wrmr", "cfi.scaled", "crmr", "tli.scaled", "rmsea.scaled")
  meta_cols <- c("Porcentaje", "Replica", "Muestras")
  all_cols <- names(resultados)
  omega_cols <- setdiff(all_cols, c(fit_cols, meta_cols))

  # Renombrar columnas de omega a F1, F2, etc.
  if (length(omega_cols) > 0) {
    new_omega_names <- paste0("omega_F", seq_along(omega_cols))
    names(resultados)[names(resultados) %in% omega_cols] <- new_omega_names
  }

  # Determinar indices disponibles
  available_fit <- intersect(fit_cols, names(resultados))
  available_omega <- grep("^omega_F", names(resultados), value = TRUE)
  all_available <- c(available_fit, available_omega)

  # Si indices es "All", usar todos los disponibles

  if (length(indices) == 1 && indices == "All") {
    indices <- all_available
  }

  # Verificar que los indices existan

  indices <- intersect(indices, all_available)

  if (length(indices) == 0) {
    stop("No se encontraron los indices especificados en los resultados.")
  }

  # Calcular percentiles para IC
  lower_prob <- (1 - ci_level) / 2
  upper_prob <- 1 - lower_prob

  # Asegurar que Porcentaje sea numerico
  resultados <- resultados %>%
    dplyr::mutate(Porcentaje = as.numeric(Porcentaje))

  # Convertir a formato largo
  resultados_long <- resultados %>%
    dplyr::select(Porcentaje, dplyr::all_of(indices)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(indices),
      names_to = "Indice",
      values_to = "Valor"
    )

  # Calcular estadisticos por Porcentaje e Indice
  summary_stats <- resultados_long %>%
    dplyr::group_by(Porcentaje, Indice) %>%
    dplyr::summarise(
      media = mean(Valor, na.rm = TRUE),
      ci_lower = quantile(Valor, probs = lower_prob, na.rm = TRUE),
      ci_upper = quantile(Valor, probs = upper_prob, na.rm = TRUE),
      .groups = "drop"
    )

  # Crear etiquetas mas legibles para los indices
  label_map <- c(
    "cfi.scaled" = "CFI",
    "tli.scaled" = "TLI",
    "rmsea.scaled" = "RMSEA",
    "srmr" = "SRMR",
    "crmr" = "CRMR",
    "wrmr" = "WRMR",
    "chisq.scaled" = "chi^2"
  )

  # Agregar etiquetas para omega
  omega_labels <- setNames(
    paste0("omega", gsub("omega_F", "", available_omega)),
    available_omega
  )
  label_map <- c(label_map, omega_labels)

  summary_stats <- summary_stats %>%
    dplyr::mutate(Indice_label = ifelse(Indice %in% names(label_map),
                                  label_map[Indice],
                                  Indice))

  # Ordenar factores
  summary_stats$Indice_label <- factor(summary_stats$Indice_label,
                                        levels = unique(summary_stats$Indice_label))

  # Crear data frame para lineas de referencia
  hline_df <- data.frame(
    Indice = names(hline_values),
    yintercept = unlist(hline_values)
  ) %>%
    dplyr::filter(Indice %in% indices) %>%
    dplyr::mutate(Indice_label = ifelse(Indice %in% names(label_map),
                                  label_map[Indice],
                                  Indice))

  # Crear el grafico
  p <- ggplot2::ggplot(summary_stats, ggplot2::aes(x = factor(Porcentaje), y = media)) +
    ggplot2::geom_pointrange(
      ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
      size = point_size / 3,
      linewidth = line_width,
      color = "grey30"
    ) +
    ggplot2::facet_wrap(~ Indice_label, scales = facet_scales, ncol = 2) +
    ggplot2::scale_x_discrete(limits = rev(c("90", "80", "70", "60", "50", "40", "30"))) +
    ggplot2::labs(
      x = "Porcentaje de la muestra",
      y = "Valor estimado",
      caption = paste0("Nota. Los puntos representan la media; las barras representan el IC ", ci_level * 100, "%.")
    )

  # Agregar lineas de referencia si existen

  if (nrow(hline_df) > 0) {
    p <- p + ggplot2::geom_hline(
      data = hline_df,
      ggplot2::aes(yintercept = yintercept),
      color = hline_color,
      linetype = hline_linetype,
      linewidth = 0.5
    )
  }

  # Aplicar tema

  if (theme_style == "minimal") {
    p <- p + ggplot2::theme_minimal(base_size = 11)
  } else if (theme_style == "bw") {
    p <- p + ggplot2::theme_bw(base_size = 11)
  } else if (theme_style == "classic") {
    p <- p + ggplot2::theme_classic(base_size = 11)
  }

  # Ajustes finales del tema
  p <- p + ggplot2::theme(
    strip.text = ggplot2::element_text(face = "bold", size = 11),
    strip.background = ggplot2::element_rect(fill = "grey95", color = NA),
    panel.grid.minor = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(size = 10),
    axis.title = ggplot2::element_text(size = 11),
    plot.caption = ggplot2::element_text(hjust = 0, size = 9, face = "italic")
  )

  return(p)
}
