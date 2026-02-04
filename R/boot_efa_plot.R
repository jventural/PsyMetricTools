#' Plot Bootstrap EFA Results
#'
#' Creates a three-panel plot showing omega reliability, CFI/TLI, and RMSEA/SRMR/CRMR
#' from bootstrap EFA results.
#'
#' @param boot_efa_results Results from boot_efa function.
#' @param save Logical, whether to save the plot (default: TRUE).
#' @param path File path to save the plot (default: "Plot_boot_efa.jpg").
#' @param dpi Resolution in dots per inch (default: 600).
#' @param omega_ymin_annot Y-axis minimum for omega table annotation.
#' @param omega_ymax_annot Y-axis maximum for omega table annotation.
#' @param comp_ymin_annot Y-axis minimum for CFI/TLI table annotation.
#' @param comp_ymax_annot Y-axis maximum for CFI/TLI table annotation.
#' @param abs_ymin_annot Y-axis minimum for RMSEA/SRMR/CRMR table annotation.
#' @param abs_ymax_annot Y-axis maximum for RMSEA/SRMR/CRMR table annotation.
#' @param palette Color palette ("grey" or wesanderson palette name).
#' @param ... Additional arguments passed to ggsave.
#'
#' @return Invisibly returns the plot.
#'
#' @export
#' @examples
#' \dontrun{
#' boot_efa_plot(results_boot_efa,
#'               path = "mi_grafico_efa.jpg",
#'               omega_ymin_annot = 0.70,
#'               omega_ymax_annot = 0.80)
#' }
boot_efa_plot <- function(boot_efa_results,
                          save = TRUE,
                          path = "Plot_boot_efa.jpg",
                          dpi = 600,
                          omega_ymin_annot = NULL,
                          omega_ymax_annot = NULL,
                          comp_ymin_annot = NULL,
                          comp_ymax_annot = NULL,
                          abs_ymin_annot = NULL,
                          abs_ymax_annot = NULL,
                          palette = "grey",
                          ...) {
  suppressWarnings({
    # 1. Funcion auxiliar para paleta de colores
    get_palette <- function(pal, n) {
      if (requireNamespace("wesanderson", quietly = TRUE) &&
          pal %in% names(wesanderson::wes_palettes)) {
        wesanderson::wes_palette(pal, n, type = "discrete")
      } else if (identical(pal, "grey")) {
        grDevices::gray.colors(n, start = 0.5, end = 0.9)
      } else {
        rep(pal, n)
      }
    }

    # 2. Color de encabezado de tablas
    get_header_color <- function(pal) {
      if (identical(pal, "grey")) {
        "grey85"
      } else if (requireNamespace("wesanderson", quietly = TRUE) &&
                 pal %in% names(wesanderson::wes_palettes)) {
        wesanderson::wes_palette(pal, 1, type = "discrete")
      } else {
        pal
      }
    }

    # 3. Bordes horizontales en tablas
    add_horizontal_borders <- function(tbl) {
      tbl <- gtable::gtable_add_grob(tbl,
                                     grobs = grid::segmentsGrob(
                                       x0 = grid::unit(0, "npc"), x1 = grid::unit(1, "npc"),
                                       y0 = grid::unit(1, "npc"), y1 = grid::unit(1, "npc"),
                                       gp = grid::gpar(lwd = 2)
                                     ),
                                     t = 1, l = 1, r = ncol(tbl))
      tbl <- gtable::gtable_add_grob(tbl,
                                     grobs = grid::segmentsGrob(
                                       x0 = grid::unit(0, "npc"), x1 = grid::unit(1, "npc"),
                                       y0 = grid::unit(0, "npc"), y1 = grid::unit(0, "npc"),
                                       gp = grid::gpar(lwd = 2)
                                     ),
                                     t = nrow(tbl), l = 1, r = ncol(tbl))
      if (nrow(tbl) > 1) {
        tbl <- gtable::gtable_add_grob(tbl,
                                       grobs = grid::segmentsGrob(
                                         x0 = grid::unit(0, "npc"), x1 = grid::unit(1, "npc"),
                                         y0 = grid::unit(1, "npc") - grid::unit(1, "pt"),
                                         y1 = grid::unit(1, "npc") - grid::unit(1, "pt"),
                                         gp = grid::gpar(lwd = 1)
                                       ),
                                       t = 2, l = 1, r = ncol(tbl))
      }
      tbl
    }

    # 4. Tema de tabla para grid.arrange
    make_table_theme <- function(pal) {
      gridExtra::ttheme_default(
        core = list(bg_params = list(fill = "white", col = NA),
                    fg_params = list(fontface = 1)),
        colhead = list(bg_params = list(fill = get_header_color(pal), col = NA),
                       fg_params = list(col = "black", fontface = c(1,1,3,1,1))),
        rowhead = list(fg_params = list(col = "black", fontface = 1)),
        base_size = 8
      )
    }

    # 5. Panel A: Omega (fiabilidad McDonald)
    plot_and_table_omega <- function(boot_results, ymin_ann, ymax_ann, pal) {
      # Extraer omega de todas las replicaciones convergidas
      omega_list <- boot_results$Replicaciones %>%
        dplyr::filter(converged == TRUE) %>%
        dplyr::pull("omega")

      omega_list <- omega_list[!sapply(omega_list, is.null)]

      if (length(omega_list) == 0) {
        return(list(table = NULL, plot = ggplot2::ggplot() + ggplot2::theme_void() +
                      ggplot2::labs(title = "No hay valores de omega disponibles")))
      }

      # Convertir lista de omegas a data frame
      omega_df <- purrr::map_dfr(omega_list, ~ as.data.frame(t(unlist(.x))))

      # Crear resumen por factor
      res_tbl <- omega_df %>%
        tidyr::pivot_longer(dplyr::everything(), names_to = "Variable", values_to = "Value") %>%
        dplyr::mutate(Variable = substr(Variable, 1, 3)) %>%
        dplyr::group_by(Variable) %>%
        dplyr::summarise(
          mean = round(mean(Value, na.rm = TRUE), 2),
          sd   = round(stats::sd(Value, na.rm = TRUE), 2),
          min  = round(min(Value, na.rm = TRUE), 2),
          max  = round(max(Value, na.rm = TRUE), 2)
        ) %>%
        dplyr::ungroup()

      ymin <- if (is.null(ymin_ann)) max(res_tbl$mean) else ymin_ann
      ymax <- if (is.null(ymax_ann)) 0.92 else ymax_ann

      dat_long <- omega_df %>%
        tidyr::pivot_longer(dplyr::everything(), names_to = "Variable", values_to = "Value") %>%
        dplyr::mutate(Variable = substr(Variable, 1, 3))

      tbl_grob <- gridExtra::tableGrob(res_tbl, rows = NULL,
                                       theme = make_table_theme(pal)) %>%
        add_horizontal_borders()

      p <- ggplot2::ggplot(dat_long, ggplot2::aes(x = Variable, y = Value, fill = Variable)) +
        ggplot2::geom_boxplot(outlier.shape = 16) +
        ggplot2::theme_bw() +
        ggplot2::scale_fill_manual(values = get_palette(pal, length(unique(dat_long$Variable)))) +
        ggplot2::coord_cartesian(ylim = c(min(res_tbl$min) - 0.1, 1)) +
        ggplot2::labs(y = "\u03C9 values") +
        ggplot2::theme(legend.position = "none") +
        ggplot2::annotation_custom(tbl_grob,
                          xmin = 1, xmax = length(res_tbl$Variable),
                          ymin = ymin, ymax = ymax)

      list(table = res_tbl, plot = p)
    }

    # 6. Panel B: CFI / TLI
    plot_and_table_comp <- function(boot_results, ymin_ann, ymax_ann, pal) {
      dfm <- boot_results$Replicaciones %>%
        dplyr::filter(converged == TRUE) %>%
        dplyr::select(fit_measures) %>%
        tidyr::unnest(fit_measures)

      # Buscar columnas CFI y TLI
      cfi_col <- names(dfm)[grepl("^cfi$|^CFI$", names(dfm), ignore.case = TRUE)][1]
      tli_col <- names(dfm)[grepl("^tli$|^TLI$", names(dfm), ignore.case = TRUE)][1]

      if (is.na(cfi_col) || is.na(tli_col)) {
        return(list(table = NULL, plot = ggplot2::ggplot() + ggplot2::theme_void() +
                      ggplot2::labs(title = "CFI/TLI no disponibles")))
      }

      res_tbl <- dfm %>%
        dplyr::select(CFI = !!rlang::sym(cfi_col), TLI = !!rlang::sym(tli_col)) %>%
        tidyr::pivot_longer(dplyr::everything(), names_to = "Fit", values_to = "Value") %>%
        dplyr::mutate(Value = round(Value, 3)) %>%
        dplyr::group_by(Fit) %>%
        dplyr::summarise(
          mean = round(mean(Value, na.rm = TRUE), 2),
          sd   = round(stats::sd(Value, na.rm = TRUE), 2),
          min  = round(min(Value, na.rm = TRUE), 2),
          max  = round(max(Value, na.rm = TRUE), 2)
        ) %>%
        dplyr::ungroup()

      ymin <- if (is.null(ymin_ann)) min(res_tbl$min) - 0.05 else ymin_ann
      ymax <- if (is.null(ymax_ann)) ymin + 0.05 else ymax_ann
      y0   <- if (min(res_tbl$min) > 0.95) 0.90 else min(res_tbl$min)

      dfm_long <- dfm %>%
        dplyr::select(CFI = !!rlang::sym(cfi_col), TLI = !!rlang::sym(tli_col)) %>%
        tidyr::pivot_longer(dplyr::everything(), names_to = "Fit", values_to = "Value") %>%
        dplyr::mutate(Value = round(Value, 3))

      tbl_grob <- gridExtra::tableGrob(res_tbl, rows = NULL,
                                       theme = make_table_theme(pal)) %>%
        add_horizontal_borders()

      p <- ggplot2::ggplot(dfm_long, ggplot2::aes(x = Fit, y = Value, fill = Fit)) +
        ggplot2::geom_boxplot(outlier.shape = 16, outlier.size = 1) +
        ggplot2::theme_bw() +
        ggplot2::scale_fill_manual(values = get_palette(pal, length(unique(dfm_long$Fit)))) +
        ggplot2::coord_cartesian(ylim = c(y0, 1)) +
        ggplot2::labs(y = "values") +
        ggplot2::theme(legend.position = "none") +
        ggplot2::annotation_custom(tbl_grob,
                          xmin = 0, xmax = 3,
                          ymin = ymin, ymax = ymax)

      list(table = res_tbl, plot = p)
    }

    # 7. Panel C: RMSEA / SRMR / CRMR
    plot_and_table_abs <- function(boot_results, ymin_ann, ymax_ann, pal) {
      dfm <- boot_results$Replicaciones %>%
        dplyr::filter(converged == TRUE) %>%
        dplyr::select(fit_measures) %>%
        tidyr::unnest(fit_measures)

      # Buscar columnas
      rmsea_col <- names(dfm)[grepl("^rmsea$|^RMSEA$", names(dfm), ignore.case = TRUE)][1]
      srmr_col <- names(dfm)[grepl("^srmr$|^SRMR$", names(dfm), ignore.case = TRUE)][1]
      crmr_col <- names(dfm)[grepl("^crmr$|^CRMR$", names(dfm), ignore.case = TRUE)][1]

      if (is.na(rmsea_col) || is.na(srmr_col) || is.na(crmr_col)) {
        return(list(table = NULL, plot = ggplot2::ggplot() + ggplot2::theme_void() +
                      ggplot2::labs(title = "RMSEA/SRMR/CRMR no disponibles")))
      }

      res_tbl <- dfm %>%
        dplyr::select(RMSEA = !!rlang::sym(rmsea_col), SRMR = !!rlang::sym(srmr_col), CRMR = !!rlang::sym(crmr_col)) %>%
        tidyr::pivot_longer(dplyr::everything(), names_to = "Fit", values_to = "Value") %>%
        dplyr::mutate(Value = round(Value, 3)) %>%
        dplyr::group_by(Fit) %>%
        dplyr::summarise(
          mean = round(mean(Value, na.rm = TRUE), 2),
          sd   = round(stats::sd(Value, na.rm = TRUE), 2),
          min  = round(min(Value, na.rm = TRUE), 2),
          max  = round(max(Value, na.rm = TRUE), 2)
        ) %>%
        dplyr::ungroup()

      ymin <- if (is.null(ymin_ann)) 0 else ymin_ann
      ymax <- if (is.null(ymax_ann)) max(res_tbl$max) else ymax_ann

      dfm_long <- dfm %>%
        dplyr::select(RMSEA = !!rlang::sym(rmsea_col), SRMR = !!rlang::sym(srmr_col), CRMR = !!rlang::sym(crmr_col)) %>%
        tidyr::pivot_longer(dplyr::everything(), names_to = "Fit", values_to = "Value") %>%
        dplyr::mutate(Value = round(Value, 3))

      tbl_grob <- gridExtra::tableGrob(res_tbl, rows = NULL,
                                       theme = make_table_theme(pal)) %>%
        add_horizontal_borders()

      p <- ggplot2::ggplot(dfm_long, ggplot2::aes(x = Fit, y = Value, fill = Fit)) +
        ggplot2::geom_boxplot(outlier.shape = 16, outlier.size = 1) +
        ggplot2::theme_bw() +
        ggplot2::scale_fill_manual(values = get_palette(pal, length(unique(dfm_long$Fit)))) +
        ggplot2::coord_cartesian(ylim = c(0, max(res_tbl$max))) +
        ggplot2::labs(y = "values") +
        ggplot2::theme(legend.position = "none") +
        ggplot2::annotation_custom(tbl_grob,
                          xmin = 0, xmax = 4,
                          ymin = ymin, ymax = ymax)

      list(table = res_tbl, plot = p)
    }

    # 8. Ensamblar y dibujar con grid.arrange
    o <- plot_and_table_omega(boot_efa_results, omega_ymin_annot, omega_ymax_annot, palette)
    c <- plot_and_table_comp(boot_efa_results, comp_ymin_annot, comp_ymax_annot, palette)
    a <- plot_and_table_abs(boot_efa_results, abs_ymin_annot, abs_ymax_annot, palette)

    # Dibujar
    gridExtra::grid.arrange(
      o$plot, c$plot, a$plot,
      ncol = 3,
      top = grid::textGrob("",
                           gp = grid::gpar(fontsize = 16, fontface = "bold"))
    )

    # Guardar si se solicita
    if (isTRUE(save)) {
      ggplot2::ggsave(
        filename = path,
        plot     = gridExtra::arrangeGrob(o$plot, c$plot, a$plot, ncol = 3),
        height   = 16,
        width    = 22,
        dpi      = dpi,
        units    = "cm",
        ...
      )
    }
  })
}
