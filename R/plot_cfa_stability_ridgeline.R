#' @title Grafico Ridgeline de Estabilidad CFA
#' @description Crea graficos ridgeline (joy plots) para visualizar la distribucion
#'   de los indices de ajuste y fiabilidad segun tamano muestral.
#' @param resultados Data frame con resultados de boot_cfa_stability.
#' @param indices Vector de indices a graficar. Por defecto c("cfi.scaled", "tli.scaled",
#'   "rmsea.scaled", "srmr"). Usar "All" para todos los disponibles.
#' @param scale_ridges Escala de superposicion de las crestas (por defecto 1.2).
#' @param alpha_fill Transparencia del relleno (por defecto 0.7).
#' @param fill_color Color de relleno. Si es NULL, usa gradiente por porcentaje.
#' @param gradient_colors Vector de 2 colores para el gradiente (por defecto c("steelblue", "darkblue")).
#' @param show_quantiles Mostrar lineas de cuantiles (por defecto TRUE).
#' @param quantile_lines Vector de cuantiles a mostrar (por defecto c(0.025, 0.5, 0.975)).
#' @param vline_values Lista nombrada con valores de referencia para lineas verticales.
#' @param vline_color Color de las lineas de referencia (por defecto "red").
#' @param vline_linetype Tipo de linea de referencia (por defecto "dashed").
#' @param theme_style Estilo del tema: "minimal", "bw", "classic" (por defecto "minimal").
#' @param rel_min_height Altura minima relativa para recortar colas (por defecto 0.01).
#' @return Un objeto ggplot.
#' @export
#' @examples
#' \dontrun{
#' plot_cfa_stability_ridgeline(resultado_stability)
#' plot_cfa_stability_ridgeline(resultado_stability, indices = c("cfi.scaled", "rmsea.scaled"))
#' }
plot_cfa_stability_ridgeline <- function(resultados,
                                         indices = c("cfi.scaled", "tli.scaled", "rmsea.scaled", "srmr"),
                                         scale_ridges = 1.2,
                                         alpha_fill = 0.7,
                                         fill_color = NULL,
                                         gradient_colors = c("steelblue", "darkblue"),
                                         show_quantiles = TRUE,
                                         quantile_lines = c(0.025, 0.5, 0.975),
                                         vline_values = list(
                                           cfi.scaled = 0.95,
                                           tli.scaled = 0.95,
                                           rmsea.scaled = 0.08,
                                           srmr = 0.08,
                                           crmr = 0.05
                                         ),
                                         vline_color = "red",
                                         vline_linetype = "dashed",
                                         theme_style = "minimal",
                                         rel_min_height = 0.01) {

  # Verificar que ggridges este instalado
  if (!requireNamespace("ggridges", quietly = TRUE)) {
    stop("El paquete 'ggridges' es necesario. Instalalo con: install.packages('ggridges')")
  }

 # Detectar columnas de omega automaticamente
  fit_cols <- c("chisq.scaled", "df.scaled", "srmr", "wrmr", "cfi.scaled", "crmr", "tli.scaled", "rmsea.scaled")
  meta_cols <- c("Porcentaje", "Replica", "Muestras")
  all_cols <- names(resultados)
  omega_cols <- setdiff(all_cols, c(fit_cols, meta_cols))

  # Renombrar columnas de omega a omega_F1, omega_F2, etc.
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

  # Asegurar que Porcentaje sea numerico y convertir a factor ordenado
  resultados <- resultados %>%
    dplyr::mutate(
      Porcentaje = as.numeric(Porcentaje),
      Porcentaje_factor = factor(Porcentaje, levels = sort(unique(Porcentaje), decreasing = FALSE))
    )

  # Convertir a formato largo
  resultados_long <- resultados %>%
    dplyr::select(Porcentaje, Porcentaje_factor, dplyr::all_of(indices)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(indices),
      names_to = "Indice",
      values_to = "Valor"
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
  available_omega <- grep("^omega_F", names(resultados), value = TRUE)
  omega_labels <- setNames(
    paste0("omega", gsub("omega_F", "", available_omega)),
    available_omega
  )
  label_map <- c(label_map, omega_labels)

  resultados_long <- resultados_long %>%
    dplyr::mutate(Indice_label = ifelse(Indice %in% names(label_map),
                                  label_map[Indice],
                                  Indice))

  # Ordenar factores
  resultados_long$Indice_label <- factor(resultados_long$Indice_label,
                                          levels = unique(resultados_long$Indice_label))

  # Crear data frame para lineas de referencia
  vline_df <- data.frame(
    Indice = names(vline_values),
    xintercept = unlist(vline_values)
  ) %>%
    dplyr::filter(Indice %in% indices) %>%
    dplyr::mutate(Indice_label = ifelse(Indice %in% names(label_map),
                                  label_map[Indice],
                                  Indice))

  # Crear el grafico base
  if (is.null(fill_color)) {
    # Usar gradiente por porcentaje
    p <- ggplot2::ggplot(resultados_long, ggplot2::aes(x = Valor, y = Porcentaje_factor, fill = Porcentaje)) +
      ggplot2::scale_fill_gradient(low = gradient_colors[1], high = gradient_colors[2],
                          name = "% Muestra")
  } else {
    # Usar color fijo
    p <- ggplot2::ggplot(resultados_long, ggplot2::aes(x = Valor, y = Porcentaje_factor)) +
      ggridges::geom_density_ridges(
        fill = fill_color,
        alpha = alpha_fill
      )
  }

  # Agregar ridgelines con o sin cuantiles
  if (show_quantiles && is.null(fill_color)) {
    p <- p + ggridges::geom_density_ridges(
      ggplot2::aes(fill = Porcentaje),
      scale = scale_ridges,
      alpha = alpha_fill,
      rel_min_height = rel_min_height,
      quantile_lines = TRUE,
      quantiles = quantile_lines,
      quantile_fun = quantile
    )
  } else if (is.null(fill_color)) {
    p <- p + ggridges::geom_density_ridges(
      ggplot2::aes(fill = Porcentaje),
      scale = scale_ridges,
      alpha = alpha_fill,
      rel_min_height = rel_min_height
    )
  }

  # Agregar facetas
  p <- p + ggplot2::facet_wrap(~ Indice_label, scales = "free_x", ncol = 2)

  # Agregar lineas de referencia si existen
  if (nrow(vline_df) > 0) {
    p <- p + ggplot2::geom_vline(
      data = vline_df,
      ggplot2::aes(xintercept = xintercept),
      color = vline_color,
      linetype = vline_linetype,
      linewidth = 0.7
    )
  }

  # Etiquetas
  p <- p + ggplot2::labs(
    x = "Valor estimado",
    y = "Porcentaje de la muestra",
    caption = if(show_quantiles) {
      "Nota. Las lineas verticales dentro de las distribuciones representan los percentiles 2.5%, 50% y 97.5%."
    } else {
      NULL
    }
  )

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
    plot.caption = ggplot2::element_text(hjust = 0, size = 9, face = "italic"),
    legend.position = "right"
  )

  return(p)
}
