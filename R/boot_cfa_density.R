#' @title Density Plot para Bootstrap CFA
#' @description Visualiza la dispersion de resultados bootstrap mediante graficos
#'   de densidad con intervalos de confianza y valores de referencia.
#' @param df Data frame con resultados de boot_cfa().
#' @param save Logical. Guardar el grafico (default TRUE).
#' @param path Ruta para guardar (default "Plot_boot_density.jpg").
#' @param dpi Resolucion (default 600).
#' @param exclude_indices Vector de indices a excluir (e.g., c("RMSEA")).
#' @param show_ci Mostrar intervalo de confianza sombreado (default TRUE).
#' @param ci_level Nivel de confianza (default 0.95).
#' @param show_reference Mostrar lineas de referencia (default TRUE).
#' @param fill_color Color de relleno de densidad (default "#3498db").
#' @param ci_color Color del area de IC (default "#e74c3c").
#' @param ... Argumentos adicionales para ggsave.
#' @return Un objeto ggplot.
#' @export
boot_cfa_density <- function(df,
                              save = TRUE,
                              path = "Plot_boot_density.jpg",
                              dpi = 600,
                              exclude_indices = NULL,
                              show_ci = TRUE,
                              ci_level = 0.95,
                              show_reference = TRUE,
                              fill_color = "#3498db",
                              ci_color = "#e74c3c",
                              ...) {

  suppressWarnings({

    # =========================================================================
    # 1. Preparar datos
    # =========================================================================

    # Extraer omega
    if ("fit_measures1" %in% names(df)) {
      idx <- which(names(df) == "fit_measures1")
      omega_data <- df[, -(1:idx)]
    } else {
      omega_data <- df[, sapply(df, is.numeric)]
    }

    omega_long <- tidyr::pivot_longer(omega_data, tidyselect::everything(),
                                       names_to = "Index", values_to = "Value")
    omega_long$Index <- paste0("omega (", substr(omega_long$Index, 1, 3), ")")
    omega_long$Type <- "Reliability"

    # Extraer fit measures
    dfm <- purrr::map_dfr(df$fit_measures1, tibble::as_tibble)

    # Indices comparativos
    comp_indices <- setdiff(c("CFI", "TLI"), toupper(exclude_indices))
    comp_long <- NULL
    if (length(comp_indices) > 0 && all(comp_indices %in% names(dfm))) {
      comp_long <- tidyr::pivot_longer(dfm[, comp_indices, drop = FALSE],
                                        tidyselect::everything(),
                                        names_to = "Index", values_to = "Value")
      comp_long$Type <- "Comparative"
    }

    # Indices absolutos
    abs_indices <- setdiff(c("RMSEA", "SRMR", "CRMR"), toupper(exclude_indices))
    abs_long <- NULL
    if (length(abs_indices) > 0) {
      available_abs <- intersect(abs_indices, names(dfm))
      if (length(available_abs) > 0) {
        abs_long <- tidyr::pivot_longer(dfm[, available_abs, drop = FALSE],
                                         tidyselect::everything(),
                                         names_to = "Index", values_to = "Value")
        abs_long$Type <- "Absolute"
      }
    }

    # Convertir Value a numeric para evitar error con lavaan.vector
    omega_long$Value <- as.numeric(omega_long$Value)
    if (!is.null(comp_long)) comp_long$Value <- as.numeric(comp_long$Value)
    if (!is.null(abs_long)) abs_long$Value <- as.numeric(abs_long$Value)

    # Combinar todos los datos
    all_data <- dplyr::bind_rows(omega_long, comp_long, abs_long)

    # =========================================================================
    # 2. Calcular estadisticos
    # =========================================================================
    alpha_level <- (1 - ci_level) / 2

    stats_df <- all_data %>%
      dplyr::group_by(Index, Type) %>%
      dplyr::summarise(
        Mean = mean(Value, na.rm = TRUE),
        Median = stats::median(Value, na.rm = TRUE),
        SD = stats::sd(Value, na.rm = TRUE),
        CI_low = stats::quantile(Value, alpha_level, na.rm = TRUE),
        CI_high = stats::quantile(Value, 1 - alpha_level, na.rm = TRUE),
        Min = min(Value, na.rm = TRUE),
        Max = max(Value, na.rm = TRUE),
        .groups = "drop"
      )

    # =========================================================================
    # 3. Valores de referencia
    # =========================================================================
    ref_values <- data.frame(
      Index = c("CFI", "TLI", "RMSEA", "SRMR", "CRMR"),
      Reference = c(0.95, 0.95, 0.08, 0.08, 0.05),
      Direction = c("above", "above", "below", "below", "below")
    )

    ref_df <- ref_values[ref_values$Index %in% unique(all_data$Index), ]

    # =========================================================================
    # 4. Crear grafico
    # =========================================================================

    p <- ggplot2::ggplot(all_data, ggplot2::aes(x = Value)) +
      # Densidad
      ggplot2::geom_density(
        fill = fill_color,
        color = "white",
        alpha = 0.7,
        linewidth = 0.8
      ) +
      # Rug plot para mostrar datos individuales
      ggplot2::geom_rug(
        alpha = 0.15,
        color = fill_color,
        length = ggplot2::unit(0.03, "npc")
      )

    # Anadir IC sombreado
    if (show_ci) {
      p <- p + ggplot2::geom_rect(
        data = stats_df,
        ggplot2::aes(xmin = CI_low, xmax = CI_high, ymin = -Inf, ymax = Inf),
        fill = ci_color,
        alpha = 0.15,
        inherit.aes = FALSE
      )
    }

    # Linea de media
    p <- p + ggplot2::geom_vline(
      data = stats_df,
      ggplot2::aes(xintercept = Mean),
      color = "#2c3e50",
      linewidth = 1,
      linetype = "solid"
    )

    # Lineas de IC
    if (show_ci) {
      p <- p + ggplot2::geom_vline(
        data = stats_df,
        ggplot2::aes(xintercept = CI_low),
        color = ci_color,
        linewidth = 0.7,
        linetype = "dashed"
      ) +
        ggplot2::geom_vline(
          data = stats_df,
          ggplot2::aes(xintercept = CI_high),
          color = ci_color,
          linewidth = 0.7,
          linetype = "dashed"
        )
    }

    # Lineas de referencia
    if (show_reference && nrow(ref_df) > 0) {
      p <- p + ggplot2::geom_vline(
        data = ref_df,
        ggplot2::aes(xintercept = Reference),
        color = "#27ae60",
        linewidth = 0.9,
        linetype = "dotted"
      )
    }

    # Facetas
    p <- p + ggplot2::facet_wrap(~ Index, scales = "free", ncol = 2)

    # Tema
    p <- p +
      ggplot2::labs(
        x = "Value",
        y = "Density",
        caption = paste0(
          "Note: Solid line = Mean; Dashed lines = ", ci_level * 100, "% CI; ",
          if(show_reference) "Dotted line = Reference cutoff" else ""
        )
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        strip.text = ggplot2::element_text(face = "bold", size = 11, color = "#2c3e50"),
        strip.background = ggplot2::element_rect(fill = "#ecf0f1", color = NA),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(color = "#ecf0f1", linewidth = 0.3),
        panel.grid.major.x = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(size = 9, color = "#7f8c8d"),
        axis.title = ggplot2::element_text(size = 10, color = "#2c3e50"),
        plot.caption = ggplot2::element_text(size = 8, color = "#95a5a6", hjust = 0,
                                     margin = ggplot2::margin(t = 15)),
        plot.background = ggplot2::element_rect(fill = "white", color = NA),
        panel.background = ggplot2::element_rect(fill = "#fafafa", color = NA),
        plot.margin = ggplot2::margin(20, 20, 15, 15)
      )

    # Mostrar
    print(p)

    # Guardar
    if (isTRUE(save)) {
      n_facets <- length(unique(all_data$Index))
      n_rows <- ceiling(n_facets / 2)

      ggplot2::ggsave(
        filename = path,
        plot = p,
        height = 5 + (n_rows * 4),
        width = 18,
        dpi = dpi,
        units = "cm",
        bg = "white",
        ...
      )
      message("Grafico guardado en: ", path)
    }

    # Imprimir tabla de estadisticos
    cat("\n=== Bootstrap Statistics ===\n\n")
    stats_print <- stats_df
    stats_print[sapply(stats_print, is.numeric)] <- round(stats_print[sapply(stats_print, is.numeric)], 3)
    print(as.data.frame(stats_print), row.names = FALSE)

    invisible(p)
  })
}
