#' Forest Plot for Bootstrap EFA Results
#'
#' Creates a forest plot showing factor loadings with 95% confidence intervals
#' from bootstrap EFA results. Style inspired by Solomon Kurz.
#'
#' @param boot_efa_results Results from boot_efa function.
#' @param save Logical, whether to save the plot (default: TRUE).
#' @param path File path to save the plot (default: "Forest_plot_efa.jpg").
#' @param dpi Resolution in dots per inch (default: 600).
#' @param title Plot title.
#' @param threshold_low Low loading threshold (default: 0.4).
#' @param threshold_mid Mid loading threshold (default: 0.7).
#' @param ... Additional arguments passed to ggsave.
#'
#' @return Invisibly returns a list with the plot and summary data.
#'
#' @export
#' @examples
#' \dontrun{
#' boot_efa_forest_plot(results_boot_efa,
#'                      path = "Forest_PBA.jpg",
#'                      title = "Factor Loadings PBA - Bootstrap EFA")
#' }
boot_efa_forest_plot <- function(boot_efa_results,
                                 save = TRUE,
                                 path = "Forest_plot_efa.jpg",
                                 dpi = 600,
                                 title = "Factor Loadings with 95% CI (Bootstrap)",
                                 threshold_low = 0.4,
                                 threshold_mid = 0.7,
                                 ...) {

  # Extraer cargas de todas las replicaciones convergidas
  loadings_list <- boot_efa_results$Replicaciones %>%
    dplyr::filter(converged == TRUE) %>%
    dplyr::pull(loadings)

  loadings_list <- loadings_list[!sapply(loadings_list, is.null)]

  if (length(loadings_list) == 0) {
    stop("No hay cargas factoriales disponibles en los resultados del bootstrap.")
  }

  # Combinar todas las cargas
  all_loadings <- dplyr::bind_rows(loadings_list, .id = "rep")

  # Obtener nombres de factores
  factor_cols <- setdiff(names(all_loadings), c("rep", "item"))

  # Calcular estadisticos por item y factor
  loadings_summary <- all_loadings %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(factor_cols),
      names_to = "Factor",
      values_to = "Loading"
    ) %>%
    dplyr::group_by(item, Factor) %>%
    dplyr::summarise(
      est.std = mean(Loading, na.rm = TRUE),
      sd = stats::sd(Loading, na.rm = TRUE),
      ci.lower = stats::quantile(Loading, 0.025, na.rm = TRUE),
      ci.upper = stats::quantile(Loading, 0.975, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Limitar los valores al rango [-0.99, 0.99] para evitar warnings de escala
    dplyr::mutate(
      ci.lower = pmax(ci.lower, -0.99),
      ci.upper = pmin(ci.upper, 0.99),
      est.std = pmax(pmin(est.std, 0.99), -0.99)
    )

  # Extraer numero del item para ordenar
  loadings_summary <- loadings_summary %>%
    dplyr::mutate(
      item_num = as.numeric(stringr::str_extract(item, "\\d+"))
    )

  # Crear variable para transparencia (cargas bajas)
  loadings_summary <- loadings_summary %>%
    dplyr::mutate(
      low_loading = abs(est.std) < threshold_low
    )

  # Crear el forest plot estilo Solomon Kurz
  p <- ggplot2::ggplot(loadings_summary, ggplot2::aes(x = est.std, xmin = ci.lower, xmax = ci.upper, y = reorder(item, -item_num))) +
    # Rectangulos de fondo para magnitud de cargas
    ggplot2::annotate(geom = "rect",
             xmin = -1, xmax = 1,
             ymin = -Inf, ymax = Inf,
             fill = "grey90") +
    ggplot2::annotate(geom = "rect",
             xmin = -threshold_mid, xmax = threshold_mid,
             ymin = -Inf, ymax = Inf,
             fill = "grey93") +
    ggplot2::annotate(geom = "rect",
             xmin = -threshold_low, xmax = threshold_low,
             ymin = -Inf, ymax = Inf,
             fill = "grey96") +

    # Linea vertical en 0
    ggplot2::geom_vline(xintercept = 0, color = "white", linewidth = 0.8) +

    # Lineas de intervalos de confianza
    ggplot2::geom_linerange(ggplot2::aes(alpha = low_loading),
                   linewidth = 0.6) +

    # Puntos (circulos grandes)
    ggplot2::geom_point(ggplot2::aes(alpha = low_loading),
               size = 5) +

    # Etiquetas de los items dentro de los puntos
    ggplot2::geom_text(ggplot2::aes(label = stringr::str_extract(item, "\\d+"),
                  color = low_loading),
              size = 2.2,
              fontface = "bold") +

    # Escalas de color y alpha
    ggplot2::scale_color_manual(values = c("white", "transparent"), guide = "none") +
    ggplot2::scale_alpha_manual(values = c(1, 1/3), guide = "none") +

    # Escala del eje X
    ggplot2::scale_x_continuous(
      expression(lambda[standardized]),
      limits = c(-1, 1),
      breaks = seq(-1, 1, 0.2),
      expand = c(0, 0)
    ) +

    # Facetas por factor
    ggplot2::facet_wrap(~ Factor, ncol = length(factor_cols)) +

    # Etiquetas
    ggplot2::labs(
      title = title,
      subtitle = paste0("Based on ", length(loadings_list), " bootstrap replications | ",
                        "Shaded regions: |lambda| < ", threshold_low, " (light), ",
                        threshold_low, "-", threshold_mid, " (medium), ",
                        "> ", threshold_mid, " (dark)"),
      y = "Item"
    ) +

    # Tema
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 14),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 9, color = "gray40"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "white", linewidth = 0.3),
      strip.text = ggplot2::element_text(face = "bold", size = 11),
      strip.background = ggplot2::element_rect(fill = "gray80", color = NA),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.text.y = ggplot2::element_text(size = 9),
      plot.caption = ggplot2::element_text(color = "gray50", hjust = 1),
      panel.spacing = ggplot2::unit(1, "lines")
    )

  # Mostrar el grafico
  print(p)

  # Guardar si se solicita
  if (isTRUE(save)) {
    n_items <- length(unique(loadings_summary$item))
    n_factors <- length(factor_cols)

    ggplot2::ggsave(
      filename = path,
      plot = p,
      height = max(15, n_items * 0.5),
      width = 8 + (n_factors * 5),
      dpi = dpi,
      units = "cm",
      ...
    )
    message("Grafico guardado en: ", path)
  }

  # Retornar invisiblemente el plot y los datos
  invisible(list(plot = p, data = loadings_summary))
}
