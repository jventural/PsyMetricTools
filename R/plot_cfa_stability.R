#' @title Plot CFA Stability Results
#' @description Creates line plots showing CFA fit indices across sample sizes.
#' @param resultados Data frame with bootstrap CFA results.
#' @param num_factors Number of factors in the model (default 3).
#' @param y_min_omega Minimum y-axis value for omega plot.
#' @param y_max_omega Maximum y-axis value for omega plot.
#' @param y_breaks_omega Y-axis breaks for omega plot.
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
#' @param y_breaks_comparative Y-axis breaks for comparative indices.
#' @param y_breaks_absolutes Y-axis breaks for absolute indices.
#' @param hline_color Color for reference lines (default "red").
#' @param xlab_size X-axis label size.
#' @param ylab_size Y-axis label size.
#' @return A combined ggplot figure.
#' @examples
#' \dontrun{
#' # First run boot_cfa_stability to get stability results
#' set.seed(123)
#' n <- 500
#' data <- data.frame(
#'   Item1 = sample(1:5, n, replace = TRUE),
#'   Item2 = sample(1:5, n, replace = TRUE),
#'   Item3 = sample(1:5, n, replace = TRUE),
#'   Item4 = sample(1:5, n, replace = TRUE),
#'   Item5 = sample(1:5, n, replace = TRUE),
#'   Item6 = sample(1:5, n, replace = TRUE)
#' )
#'
#' model <- "
#'   F1 =~ Item1 + Item2 + Item3
#'   F2 =~ Item4 + Item5 + Item6
#' "
#'
#' stability_results <- boot_cfa_stability(
#'   modelo = model,
#'   data = data,
#'   num_replicas = 50,
#'   estimator = "WLSMV"
#' )
#'
#' # Create stability plots
#' figure <- plot_cfa_stability(
#'   resultados = stability_results,
#'   num_factors = 2,
#'   y_min_omega = 0.6,
#'   y_max_omega = 1,
#'   y_min_cfi = 0.85,
#'   y_max_cfi = 1,
#'   hline_color = "red"
#' )
#'
#' # Save the figure
#' ggplot2::ggsave("stability_analysis.jpg", figure,
#'                 width = 12, height = 8, dpi = 300)
#' }
#' @export
plot_cfa_stability <- function(resultados, num_factors = 3,
                                y_min_omega = 0.7, y_max_omega = 1, y_breaks_omega = 0.05,
                                y_min_cfi = 0.9, y_max_cfi = 1,
                                y_min_tli = 0.9, y_max_tli = 1, y_min_rmsea = 0, y_max_rmsea = 0.15,
                                y_min_srmr = 0, y_max_srmr = 0.15, y_min_crmr = 0, y_max_crmr = 0.15,
                                y_min_reliability = 0.5, y_max_reliability = 1,
                                y_breaks_comparative = 0.02, y_breaks_absolutes = 0.05,
                                hline_color = "red", xlab_size = 12, ylab_size = 12)
{

  # Grafico de CFI
  CFI_stability <- resultados %>%
    dplyr::group_by(Porcentaje) %>%
    dplyr::summarise(CFI = mean(cfi.scaled)) %>%
    dplyr::mutate(Porcentaje = as.numeric(Porcentaje))

  p4 <- ggplot2::ggplot(CFI_stability, ggplot2::aes(x = Porcentaje, y = CFI, group = 1)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Percentage", y = "CFI") +
    ggplot2::scale_y_continuous(limits = c(y_min_cfi, y_max_cfi), breaks = seq(y_min_cfi, y_max_cfi, y_breaks_comparative)) +
    ggplot2::geom_hline(yintercept = 0.95, linetype = "solid", color = hline_color, linewidth = 0.5) +
    ggplot2::scale_x_reverse(breaks = seq(90, 30, -10)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = xlab_size), axis.text.y = ggplot2::element_text(size = ylab_size))

  # Grafico de TLI
  TLI_stability <- resultados %>%
    dplyr::group_by(Porcentaje) %>%
    dplyr::summarise(TLI = mean(tli.scaled)) %>%
    dplyr::mutate(Porcentaje = as.numeric(Porcentaje))

  p5 <- ggplot2::ggplot(TLI_stability, ggplot2::aes(x = Porcentaje, y = TLI, group = 1)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Percentage", y = "TLI") +
    ggplot2::scale_y_continuous(limits = c(y_min_tli, y_max_tli), breaks = seq(y_min_tli, y_max_tli, y_breaks_comparative)) +
    ggplot2::geom_hline(yintercept = 0.95, linetype = "solid", color = hline_color, linewidth = 0.5) +
    ggplot2::scale_x_reverse(breaks = seq(90, 30, -10)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = xlab_size), axis.text.y = ggplot2::element_text(size = ylab_size))

  # Grafico de RMSEA
  RMSEA_stability <- resultados %>%
    dplyr::group_by(Porcentaje) %>%
    dplyr::summarise(RMSEA = mean(rmsea.scaled)) %>%
    dplyr::mutate(Porcentaje = as.numeric(Porcentaje))

  p6 <- ggplot2::ggplot(RMSEA_stability, ggplot2::aes(x = Porcentaje, y = RMSEA, group = 1)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Percentage", y = "RMSEA") +
    ggplot2::scale_y_continuous(limits = c(y_min_rmsea, y_max_rmsea), breaks = seq(y_min_rmsea, y_max_rmsea, y_breaks_absolutes)) +
    ggplot2::geom_hline(yintercept = 0.08, linetype = "solid", color = hline_color, linewidth = 0.5) +
    ggplot2::scale_x_reverse(breaks = seq(90, 30, -10)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = xlab_size), axis.text.y = ggplot2::element_text(size = ylab_size))

  # Grafico de SRMR
  SRMR_stability <- resultados %>%
    dplyr::group_by(Porcentaje) %>%
    dplyr::summarise(SRMR = mean(srmr)) %>%
    dplyr::mutate(Porcentaje = as.numeric(Porcentaje))

  p7 <- ggplot2::ggplot(SRMR_stability, ggplot2::aes(x = Porcentaje, y = SRMR, group = 1)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Percentage", y = "SRMR") +
    ggplot2::scale_y_continuous(limits = c(y_min_srmr, y_max_srmr), breaks = seq(y_min_srmr, y_max_srmr, y_breaks_absolutes)) +
    ggplot2::geom_hline(yintercept = 0.06, linetype = "solid", color = hline_color, linewidth = 0.5) +
    ggplot2::scale_x_reverse(breaks = seq(90, 30, -10)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = xlab_size), axis.text.y = ggplot2::element_text(size = ylab_size))

  # Grafico de CRMR
  CRMR_stability <- resultados %>%
    dplyr::group_by(Porcentaje) %>%
    dplyr::summarise(CRMR = mean(crmr)) %>%
    dplyr::mutate(Porcentaje = as.numeric(Porcentaje))

  p8 <- ggplot2::ggplot(CRMR_stability, ggplot2::aes(x = Porcentaje, y = CRMR, group = 1)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Percentage", y = "CRMR") +
    ggplot2::scale_y_continuous(limits = c(y_min_crmr, y_max_crmr), breaks = seq(y_min_crmr, y_max_crmr, y_breaks_absolutes)) +
    ggplot2::geom_hline(yintercept = 0.05, linetype = "solid", color = hline_color, linewidth = 0.5) +
    ggplot2::scale_x_reverse(breaks = seq(90, 30, -10)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = xlab_size), axis.text.y = ggplot2::element_text(size = ylab_size))

  # Grafico de medias para Fiabilidad Multidimensional (omega)
  # Detectar columnas de omega automaticamente (no son indices de ajuste ni metadatos)
  fit_cols <- c("chisq.scaled", "df.scaled", "srmr", "wrmr", "cfi.scaled", "crmr", "tli.scaled", "rmsea.scaled")
  meta_cols <- c("Porcentaje", "Replica", "Muestras")
  all_cols <- names(resultados)
  omega_cols <- setdiff(all_cols, c(fit_cols, meta_cols))

  # Si no hay columnas de omega, crear grafico vacio

  if (length(omega_cols) == 0) {
    p9 <- ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "No reliability data found")
  } else {
    # Renombrar columnas de omega a F1, F2, etc. para consistencia
    resultados_omega <- resultados %>% dplyr::select(dplyr::all_of(c(omega_cols, "Porcentaje")))
    names(resultados_omega)[names(resultados_omega) %in% omega_cols] <- paste0("F", seq_along(omega_cols))
    omega_cols_renamed <- paste0("F", seq_along(omega_cols))

    summary_long <- resultados_omega %>%
      dplyr::group_by(Porcentaje) %>%
      dplyr::summarise(dplyr::across(dplyr::all_of(omega_cols_renamed),
                       list(mean = ~mean(.x, na.rm = TRUE)),
                       .names = "{.col}_{.fn}")) %>%
      dplyr::rename_with(~gsub("_mean", "_omega", .)) %>%
      dplyr::mutate(Porcentaje = as.numeric(as.character(Porcentaje)))

    # Crear el grafico para la cantidad de factores detectada
    actual_num_factors <- min(num_factors, length(omega_cols))

    p9 <- ggplot2::ggplot(summary_long, ggplot2::aes(x = Porcentaje, group = 1)) +
      ggplot2::geom_hline(yintercept = 0.70, linetype = "solid", color = hline_color, linewidth = 0.5) +
      ggplot2::scale_x_reverse(breaks = seq(90, 30, -10)) +
      ggplot2::labs(x = "Percentage", y = "omega values", shape = "Variables", linetype = "Variables") +
      ggplot2::scale_y_continuous(limits = c(y_min_omega, y_max_omega), breaks = seq(y_min_omega, y_max_omega, y_breaks_omega)) +
      ggplot2::theme_minimal()

    for (i in 1:actual_num_factors) {
      col_name <- paste0("F", i, "_omega")
      p9 <- p9 +
        ggplot2::geom_line(ggplot2::aes(y = .data[[col_name]], linetype = col_name), linewidth = 0.5) +
        ggplot2::geom_point(ggplot2::aes(y = .data[[col_name]], shape = col_name), size = 2)
    }
  }

  # Combinando los graficos en una figura
  figure3 <- suppressWarnings(ggpubr::ggarrange(p4, p5, p6, p7, p8, p9,
                                        labels = c("A", "B", "C", "D", "E", "F"),
                                        ncol = 3, nrow = 2))

  return(figure3)
}
