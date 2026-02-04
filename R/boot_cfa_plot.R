#' @title Bootstrap CFA Plot
#' @description Creates boxplot visualizations for bootstrap CFA results.
#' @param df Data frame with results from boot_cfa().
#' @param save Logical. Save the plot (default TRUE).
#' @param path Path to save the plot (default "Plot_boot_cfa.jpg").
#' @param dpi Resolution in DPI (default 600).
#' @param omega_ymin_annot Y-axis minimum for omega annotation.
#' @param omega_ymax_annot Y-axis maximum for omega annotation.
#' @param comp_ymin_annot Y-axis minimum for comparative indices annotation.
#' @param comp_ymax_annot Y-axis maximum for comparative indices annotation.
#' @param abs_ymin_annot Y-axis minimum for absolute indices annotation.
#' @param abs_ymax_annot Y-axis maximum for absolute indices annotation.
#' @param palette Color palette (default "grey").
#' @param exclude_indices Indices to exclude from plots.
#' @param show_tables Show summary tables in plots (default TRUE).
#' @param ... Additional arguments passed to ggsave.
#' @return A combined ggplot object (invisibly).
#' @examples
#' \dontrun{
#' # First run boot_cfa to get bootstrap results
#' set.seed(123)
#' n <- 300
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
#' boot_results <- boot_cfa(
#'   new_df = data,
#'   model_string = model,
#'   item_prefix = "Item",
#'   n_replications = 100
#' )
#'
#' # Create boxplot visualization
#' boot_cfa_plot(boot_results,
#'               save = TRUE,
#'               path = "bootstrap_cfa_results.jpg",
#'               palette = "grey",
#'               show_tables = TRUE)
#'
#' # Exclude certain indices
#' boot_cfa_plot(boot_results,
#'               save = FALSE,
#'               exclude_indices = c("CRMR"),
#'               palette = "grey")
#' }
#' @export
boot_cfa_plot <- function(df,
                         save = TRUE,
                         path = "Plot_boot_cfa.jpg",
                         dpi = 600,
                         omega_ymin_annot = NULL,
                         omega_ymax_annot = NULL,
                         comp_ymin_annot = NULL,
                         comp_ymax_annot = NULL,
                         abs_ymin_annot = NULL,
                         abs_ymax_annot = NULL,
                         palette = "grey",
                         exclude_indices = NULL,
                         show_tables = TRUE,
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

    # 5. Panel A: Omega
    plot_and_table_omega <- function(df_repli, ymin_ann, ymax_ann, pal, show_tbl = TRUE) {
      if ("fit_measures1" %in% names(df_repli)) {
        idx <- which(names(df_repli) == "fit_measures1")
        dat <- df_repli[, -(1:idx)]
      } else {
        dat <- df_repli[, sapply(df_repli, is.numeric)]
      }

      dat_long <- tidyr::pivot_longer(dat, tidyselect::everything(),
                                       names_to = "Variable", values_to = "Value")

      res_tbl <- dat_long %>%
        dplyr::group_by(Variable) %>%
        dplyr::summarise(
          mean = round(mean(Value, na.rm = TRUE), 2),
          sd   = round(stats::sd(Value, na.rm = TRUE), 2),
          min  = round(min(Value, na.rm = TRUE), 2),
          max  = round(max(Value, na.rm = TRUE), 2),
          .groups = "drop"
        )

      ymin <- if (is.null(ymin_ann)) max(res_tbl$mean) else ymin_ann
      ymax <- if (is.null(ymax_ann)) 0.92 else ymax_ann

      p <- ggplot2::ggplot(dat_long, ggplot2::aes(x = Variable, y = Value, fill = Variable)) +
        ggplot2::geom_boxplot(outlier.shape = 16) +
        ggplot2::theme_bw() +
        ggplot2::scale_fill_manual(values = get_palette(pal, length(unique(dat_long$Variable)))) +
        ggplot2::coord_cartesian(ylim = c(min(res_tbl$min) - 0.1, 1)) +
        ggplot2::labs(y = "\u03C9 values") +
        ggplot2::theme(legend.position = "none")

      if (show_tbl) {
        tbl_grob <- gridExtra::tableGrob(res_tbl, rows = NULL,
                                         theme = make_table_theme(pal))
        tbl_grob <- add_horizontal_borders(tbl_grob)
        p <- p + ggplot2::annotation_custom(tbl_grob,
                                   xmin = 1, xmax = length(res_tbl$Variable),
                                   ymin = ymin, ymax = ymax)
      }

      list(table = res_tbl, plot = p)
    }

    # 6. Panel B: CFI / TLI
    plot_and_table_comp <- function(df_repli, ymin_ann, ymax_ann, pal, exclude = NULL, show_tbl = TRUE) {
      dfm <- purrr::map_dfr(df_repli$fit_measures1, tibble::as_tibble)

      # Indices comparativos disponibles
      comp_indices <- c("CFI", "TLI")
      if (!is.null(exclude)) {
        comp_indices <- setdiff(comp_indices, toupper(exclude))
      }

      if (length(comp_indices) == 0) return(NULL)

      dfm_long <- tidyr::pivot_longer(dfm[, comp_indices, drop = FALSE],
                                       tidyselect::everything(),
                                       names_to = "Fit", values_to = "Value")
      dfm_long$Value <- round(dfm_long$Value, 3)

      res_tbl <- dfm_long %>%
        dplyr::group_by(Fit) %>%
        dplyr::summarise(
          mean = round(mean(Value, na.rm = TRUE), 2),
          sd   = round(stats::sd(Value, na.rm = TRUE), 2),
          min  = round(min(Value, na.rm = TRUE), 2),
          max  = round(max(Value, na.rm = TRUE), 2),
          .groups = "drop"
        )

      ymin <- if (is.null(ymin_ann)) min(res_tbl$min) - 0.05 else ymin_ann
      ymax <- if (is.null(ymax_ann)) ymin + 0.05 else ymax_ann
      y0   <- if (min(res_tbl$min) > 0.95) 0.90 else min(res_tbl$min)

      p <- ggplot2::ggplot(dfm_long, ggplot2::aes(x = Fit, y = Value, fill = Fit)) +
        ggplot2::geom_boxplot(outlier.shape = 16, outlier.size = 1) +
        ggplot2::theme_bw() +
        ggplot2::scale_fill_manual(values = get_palette(pal, length(unique(dfm_long$Fit)))) +
        ggplot2::coord_cartesian(ylim = c(y0, 1)) +
        ggplot2::labs(y = "values") +
        ggplot2::theme(legend.position = "none")

      if (show_tbl) {
        tbl_grob <- gridExtra::tableGrob(res_tbl, rows = NULL,
                                         theme = make_table_theme(pal))
        tbl_grob <- add_horizontal_borders(tbl_grob)
        p <- p + ggplot2::annotation_custom(tbl_grob,
                                   xmin = 0, xmax = length(comp_indices) + 1,
                                   ymin = ymin, ymax = ymax)
      }

      list(table = res_tbl, plot = p)
    }

    # 7. Panel C: RMSEA / SRMR / CRMR
    plot_and_table_abs <- function(df_repli, ymin_ann, ymax_ann, pal, exclude = NULL, show_tbl = TRUE) {
      dfm <- purrr::map_dfr(df_repli$fit_measures1, tibble::as_tibble)

      # Indices absolutos disponibles
      abs_indices <- c("RMSEA", "SRMR", "CRMR")
      if (!is.null(exclude)) {
        abs_indices <- setdiff(abs_indices, toupper(exclude))
      }

      if (length(abs_indices) == 0) return(NULL)

      dfm_long <- tidyr::pivot_longer(dfm[, abs_indices, drop = FALSE],
                                       tidyselect::everything(),
                                       names_to = "Fit", values_to = "Value")
      dfm_long$Value <- round(dfm_long$Value, 3)

      res_tbl <- dfm_long %>%
        dplyr::group_by(Fit) %>%
        dplyr::summarise(
          mean = round(mean(Value, na.rm = TRUE), 2),
          sd   = round(stats::sd(Value, na.rm = TRUE), 2),
          min  = round(min(Value, na.rm = TRUE), 2),
          max  = round(max(Value, na.rm = TRUE), 2),
          .groups = "drop"
        )

      ymin <- if (is.null(ymin_ann)) 0 else ymin_ann
      ymax <- if (is.null(ymax_ann)) max(res_tbl$max) else ymax_ann

      p <- ggplot2::ggplot(dfm_long, ggplot2::aes(x = Fit, y = Value, fill = Fit)) +
        ggplot2::geom_boxplot(outlier.shape = 16, outlier.size = 1) +
        ggplot2::theme_bw() +
        ggplot2::scale_fill_manual(values = get_palette(pal, length(unique(dfm_long$Fit)))) +
        ggplot2::coord_cartesian(ylim = c(0, max(res_tbl$max))) +
        ggplot2::labs(y = "values") +
        ggplot2::theme(legend.position = "none")

      if (show_tbl) {
        tbl_grob <- gridExtra::tableGrob(res_tbl, rows = NULL,
                                         theme = make_table_theme(pal))
        tbl_grob <- add_horizontal_borders(tbl_grob)
        p <- p + ggplot2::annotation_custom(tbl_grob,
                                   xmin = 0, xmax = length(abs_indices) + 1,
                                   ymin = ymin, ymax = ymax)
      }

      list(table = res_tbl, plot = p)
    }

    # 8. Ensamblar y dibujar con grid.arrange (sin patchwork)
    o <- plot_and_table_omega(df, omega_ymin_annot, omega_ymax_annot, palette, show_tables)
    c <- plot_and_table_comp(df, comp_ymin_annot, comp_ymax_annot, palette, exclude_indices, show_tables)
    a <- plot_and_table_abs(df, abs_ymin_annot, abs_ymax_annot, palette, exclude_indices, show_tables)

    # Crear lista de plots no nulos
    plot_list <- list()
    if (!is.null(o)) plot_list <- c(plot_list, list(o$plot))
    if (!is.null(c)) plot_list <- c(plot_list, list(c$plot))
    if (!is.null(a)) plot_list <- c(plot_list, list(a$plot))

    ncols <- length(plot_list)

    # Dibujar en el dispositivo
    if (ncols > 0) {
      args <- c(plot_list, list(ncol = ncols))
      do.call(gridExtra::grid.arrange, args)
    }

    # Si se pide guardar en disco, volver a ensamblar con arrangeGrob
    if (isTRUE(save) && ncols > 0) {
      args <- c(plot_list, list(ncol = ncols))
      combined_plot <- do.call(gridExtra::arrangeGrob, args)
      ggplot2::ggsave(
        filename = path,
        plot     = combined_plot,
        height   = 16,
        width    = 7 * ncols,
        dpi      = dpi,
        units    = "cm",
        ...
      )
    }

    # Retornar el plot combinado con clase personalizada para auto-print
    if (ncols > 0) {
      args <- c(plot_list, list(ncol = ncols))
      result <- do.call(gridExtra::arrangeGrob, args)
      class(result) <- c("boot_cfa_plot", class(result))
      return(invisible(result))
    }
  })
}

#' @export
#' @method print boot_cfa_plot
print.boot_cfa_plot <- function(x, ...) {
  grid::grid.newpage()
  grid::grid.draw(x)
  invisible(x)
}
