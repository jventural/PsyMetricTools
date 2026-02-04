#' @title Plot CFA Stability Results (Boxplot)
#' @description Creates boxplots showing CFA fit indices distributions across sample sizes.
#' @param resultados Data frame with bootstrap CFA results.
#' @param y_min_cfi Minimum y-axis value for CFI plot.
#' @param y_max_cfi Maximum y-axis value for CFI plot.
#' @param y_min_tli Minimum y-axis value for TLI plot.
#' @param y_max_tli Maximum y-axis value for TLI plot.
#' @param y_min_rmsea Minimum y-axis value for RMSEA plot.
#' @param y_max_rmsea Maximum y-axis value for RMSEA plot.
#' @param y_min_srmr Minimum y-axis value for SRMR plot.
#' @param y_max_srmr Maximum y-axis value for SRMR plot.
#' @param y_min_crmr Minimum y-axis value for CRMR plot.
#' @param y_max_crmr Maximum y-axis value for CRMR plot.
#' @param y_min_reliability Minimum y-axis value for reliability plot.
#' @param y_max_reliability Maximum y-axis value for reliability plot.
#' @param y_breaks Y-axis breaks for fit indices.
#' @param y_breaks_reliability Y-axis breaks for reliability.
#' @param hline_color Color for reference lines (default "red").
#' @param xlab_size X-axis label size.
#' @param ylab_size Y-axis label size.
#' @param label_size Label size.
#' @return A combined ggplot figure with boxplots.
#' @export
plot_cfa_stability_box <- function(resultados,
                                y_min_cfi = 0.9, y_max_cfi = 1,
                                y_min_tli  = 0.9, y_max_tli = 1,
                                y_min_rmsea = 0, y_max_rmsea = 0.15,
                                y_min_srmr = 0, y_max_srmr = 0.15,
                                y_min_crmr = 0, y_max_crmr = 0.15,
                                y_min_reliability = 0.50, y_max_reliability = 1,
                                y_breaks = 0.01,
                                y_breaks_reliability = 0.05,
                                hline_color = "red",
                                xlab_size = 12,
                                ylab_size = 12,
                                label_size = 10) {

  # Verificar y convertir a data.frame si es necesario
  if (!is.data.frame(resultados)) {
    resultados <- as.data.frame(resultados)
  }

  # Crear el gráfico de boxplot CFI
  b1 <- ggplot2::ggplot(resultados, ggplot2::aes(x = factor(Porcentaje), y = cfi.scaled)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(x = "Percentages", y = "CFI", title = "") +
    ggplot2::theme_bw() +
    ggplot2::scale_x_discrete(limits = rev(levels(factor(resultados$Porcentaje)))) +
    ggplot2::geom_hline(yintercept = 0.95, linetype = "solid", color = hline_color, size = 0.5) +
    ggplot2::scale_y_continuous(limits = c(y_min_cfi, y_max_cfi), breaks = seq(y_min_cfi, y_max_cfi, y_breaks)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = xlab_size),
          axis.text.y = ggplot2::element_text(size = ylab_size))

  # Crear el gráfico de boxplot TLI
  b2 <- ggplot2::ggplot(resultados, ggplot2::aes(x = factor(Porcentaje), y = tli.scaled)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(x = "Percentages", y = "TLI", title = "") +
    ggplot2::theme_bw() +
    ggplot2::scale_x_discrete(limits = rev(levels(factor(resultados$Porcentaje)))) +
    ggplot2::geom_hline(yintercept = 0.95, linetype = "solid", color = hline_color, size = 0.5) +
    ggplot2::scale_y_continuous(limits = c(y_min_tli, y_max_tli), breaks = seq(y_min_tli, y_max_tli, y_breaks)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = xlab_size),
          axis.text.y = ggplot2::element_text(size = ylab_size))

  # Crear el gráfico de boxplot RMSEA
  b3 <- ggplot2::ggplot(resultados, ggplot2::aes(x = factor(Porcentaje), y = rmsea.scaled)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(x = "Percentages", y = "RMSEA", title = "") +
    ggplot2::theme_bw() +
    ggplot2::scale_x_discrete(limits = rev(levels(factor(resultados$Porcentaje)))) +
    ggplot2::geom_hline(yintercept = 0.08, linetype = "solid", color = hline_color, size = 0.5) +
    ggplot2::scale_y_continuous(limits = c(y_min_rmsea, y_max_rmsea), breaks = seq(y_min_rmsea, y_max_rmsea, y_breaks)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = xlab_size),
          axis.text.y = ggplot2::element_text(size = ylab_size))

  # Crear el gráfico de boxplot SRMR
  b4 <- ggplot2::ggplot(resultados, ggplot2::aes(x = factor(Porcentaje), y = srmr)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(x = "Percentages", y = "SRMR", title = "") +
    ggplot2::theme_bw() +
    ggplot2::scale_x_discrete(limits = rev(levels(factor(resultados$Porcentaje)))) +
    ggplot2::geom_hline(yintercept = 0.08, linetype = "solid", color = hline_color, size = 0.5) +
    ggplot2::scale_y_continuous(limits = c(y_min_srmr, y_max_srmr), breaks = seq(y_min_srmr, y_max_srmr, y_breaks)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = xlab_size),
          axis.text.y = ggplot2::element_text(size = ylab_size))

  # Crear el gráfico de boxplot CRMR
  b5 <- ggplot2::ggplot(resultados, ggplot2::aes(x = factor(Porcentaje), y = crmr)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(x = "Percentages", y = "CRMR", title = "") +
    ggplot2::theme_bw() +
    ggplot2::scale_x_discrete(limits = rev(levels(factor(resultados$Porcentaje)))) +
    ggplot2::geom_hline(yintercept = 0.05, linetype = "solid", color = hline_color, size = 0.5) +
    ggplot2::scale_y_continuous(limits = c(y_min_crmr, y_max_crmr), breaks = seq(y_min_crmr, y_max_crmr, y_breaks)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = xlab_size),
          axis.text.y = ggplot2::element_text(size = ylab_size))

  # Crear el gráfico de boxplot fiabilidad
  b6 <- ggplot2::ggplot(resultados, ggplot2::aes(x = factor(Porcentaje), y = F1)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(x = "Percentages", y = "Reliability", title = "") +
    ggplot2::theme_bw() +
    ggplot2::scale_x_discrete(limits = rev(levels(factor(resultados$Porcentaje)))) +
    ggplot2::geom_hline(yintercept = 0.70, linetype = "solid", color = hline_color, size = 0.5) +
    ggplot2::scale_y_continuous(limits = c(y_min_reliability, y_max_reliability), breaks = seq(y_min_reliability, y_max_reliability, y_breaks_reliability)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = xlab_size),
          axis.text.y = ggplot2::element_text(size = ylab_size))

  # Combinar todos los gráficos en una figura
  figure4 <- suppressMessages(suppressWarnings(ggpubr::ggarrange(b1, b2, b3, b4, b5, b6,
                                                         labels = c("A", "B", "C", "D", "E", "F"),
                                                         ncol = 3, nrow = 2)))

  return(figure4)

}
