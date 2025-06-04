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
                          ...) {
  suppressWarnings({
    # Cargamos solo lo imprescindible
    library(ggplot2)
    library(tidyr)
    library(dplyr)
    library(purrr)
    library(reshape2)
    library(gridExtra)
    library(gtable)

    # 1. Funcion auxiliar para paleta de colores
    get_palette <- function(pal, n) {
      if (requireNamespace("wesanderson", quietly = TRUE) &&
          pal %in% names(wesanderson::wes_palettes)) {
        wesanderson::wes_palette(pal, n, type = "discrete")
      } else if (identical(pal, "grey")) {
        gray.colors(n, start = 0.5, end = 0.9)
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
                                       x0 = unit(0, "npc"), x1 = unit(1, "npc"),
                                       y0 = unit(1, "npc"), y1 = unit(1, "npc"),
                                       gp = grid::gpar(lwd = 2)
                                     ),
                                     t = 1, l = 1, r = ncol(tbl))
      tbl <- gtable::gtable_add_grob(tbl,
                                     grobs = grid::segmentsGrob(
                                       x0 = unit(0, "npc"), x1 = unit(1, "npc"),
                                       y0 = unit(0, "npc"), y1 = unit(0, "npc"),
                                       gp = grid::gpar(lwd = 2)
                                     ),
                                     t = nrow(tbl), l = 1, r = ncol(tbl))
      if (nrow(tbl) > 1) {
        tbl <- gtable::gtable_add_grob(tbl,
                                       grobs = grid::segmentsGrob(
                                         x0 = unit(0, "npc"), x1 = unit(1, "npc"),
                                         y0 = unit(1, "npc") - unit(1, "pt"),
                                         y1 = unit(1, "npc") - unit(1, "pt"),
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
    plot_and_table_omega <- function(df_repli, ymin_ann, ymax_ann, pal) {
      if ("fit_measures1" %in% names(df_repli)) {
        idx <- which(names(df_repli) == "fit_measures1")
        dat <- df_repli %>% select(-(1:idx))
      } else {
        dat <- df_repli %>% select(where(is.numeric))
      }

      res_tbl <- dat %>%
        pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
        mutate(Variable = substr(Variable, 1, 3)) %>%
        group_by(Variable) %>%
        summarise(
          mean = round(mean(Value, na.rm = TRUE), 2),
          sd   = round(sd(Value,   na.rm = TRUE), 2),
          min  = round(min(Value,  na.rm = TRUE), 2),
          max  = round(max(Value,  na.rm = TRUE), 2)
        ) %>%
        ungroup()

      ymin <- if (is.null(ymin_ann)) max(res_tbl$mean) else ymin_ann
      ymax <- if (is.null(ymax_ann)) 0.92 else ymax_ann

      dat_long <- dat %>%
        pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
        mutate(Variable = substr(Variable, 1, 3))

      tbl_grob <- gridExtra::tableGrob(res_tbl, rows = NULL,
                                       theme = make_table_theme(pal)) %>%
        add_horizontal_borders()

      p <- ggplot(dat_long, aes(x = Variable, y = Value, fill = Variable)) +
        geom_boxplot(outlier.shape = 16) +
        theme_bw() +
        scale_fill_manual(values = get_palette(pal, length(unique(dat_long$Variable)))) +
        coord_cartesian(ylim = c(min(res_tbl$min) - 0.1, 1)) +
        labs(y = "\u03C9 values") +
        theme(legend.position = "none") +
        annotation_custom(tbl_grob,
                          xmin = 1, xmax = length(res_tbl$Variable),
                          ymin = ymin, ymax = ymax)

      list(table = res_tbl, plot = p)
    }

    # 6. Panel B: CFI / TLI
    plot_and_table_comp <- function(df_repli, ymin_ann, ymax_ann, pal) {
      dfm <- map_dfr(df_repli$fit_measures1, as_tibble)

      res_tbl <- dfm %>%
        select(CFI, TLI) %>%
        pivot_longer(everything(), names_to = "Fit", values_to = "Value") %>%
        mutate(Value = round(Value, 3)) %>%
        group_by(Fit) %>%
        summarise(
          mean = round(mean(Value, na.rm = TRUE), 2),
          sd   = round(sd(Value,   na.rm = TRUE), 2),
          min  = round(min(Value,  na.rm = TRUE), 2),
          max  = round(max(Value,  na.rm = TRUE), 2)
        ) %>%
        ungroup()

      ymin <- if (is.null(ymin_ann)) min(res_tbl$min) - 0.05 else ymin_ann
      ymax <- if (is.null(ymax_ann)) ymin + 0.05 else ymax_ann
      y0   <- if (min(res_tbl$min) > 0.95) 0.90 else min(res_tbl$min)

      dfm_long <- dfm %>%
        select(CFI, TLI) %>%
        pivot_longer(everything(), names_to = "Fit", values_to = "Value") %>%
        mutate(Value = round(Value, 3))

      tbl_grob <- gridExtra::tableGrob(res_tbl, rows = NULL,
                                       theme = make_table_theme(pal)) %>%
        add_horizontal_borders()

      p <- ggplot(dfm_long, aes(x = Fit, y = Value, fill = Fit)) +
        geom_boxplot(outlier.shape = 16, outlier.size = 1) +
        theme_bw() +
        scale_fill_manual(values = get_palette(pal, length(unique(dfm_long$Fit)))) +
        coord_cartesian(ylim = c(y0, 1)) +
        labs(y = "values") +
        theme(legend.position = "none") +
        annotation_custom(tbl_grob,
                          xmin = 0, xmax = 3,
                          ymin = ymin, ymax = ymax)

      list(table = res_tbl, plot = p)
    }

    # 7. Panel C: RMSEA / SRMR / CRMR
    plot_and_table_abs <- function(df_repli, ymin_ann, ymax_ann, pal) {
      dfm <- map_dfr(df_repli$fit_measures1, as_tibble)

      res_tbl <- dfm %>%
        select(RMSEA, SRMR, CRMR) %>%
        pivot_longer(everything(), names_to = "Fit", values_to = "Value") %>%
        mutate(Value = round(Value, 3)) %>%
        group_by(Fit) %>%
        summarise(
          mean = round(mean(Value, na.rm = TRUE), 2),
          sd   = round(sd(Value,   na.rm = TRUE), 2),
          min  = round(min(Value,  na.rm = TRUE), 2),
          max  = round(max(Value,  na.rm = TRUE), 2)
        ) %>%
        ungroup()

      ymin <- if (is.null(ymin_ann)) 0 else ymin_ann
      ymax <- if (is.null(ymax_ann)) max(res_tbl$max) else ymax_ann

      dfm_long <- dfm %>%
        select(RMSEA, SRMR, CRMR) %>%
        pivot_longer(everything(), names_to = "Fit", values_to = "Value") %>%
        mutate(Value = round(Value, 3))

      tbl_grob <- gridExtra::tableGrob(res_tbl, rows = NULL,
                                       theme = make_table_theme(pal)) %>%
        add_horizontal_borders()

      p <- ggplot(dfm_long, aes(x = Fit, y = Value, fill = Fit)) +
        geom_boxplot(outlier.shape = 16, outlier.size = 1) +
        theme_bw() +
        scale_fill_manual(values = get_palette(pal, length(unique(dfm_long$Fit)))) +
        coord_cartesian(ylim = c(0, max(res_tbl$max))) +
        labs(y = "values") +
        theme(legend.position = "none") +
        annotation_custom(tbl_grob,
                          xmin = 0, xmax = 4,
                          ymin = ymin, ymax = ymax)

      list(table = res_tbl, plot = p)
    }

    # 8. Ensamblar y dibujar con grid.arrange (sin patchwork)
    o <- plot_and_table_omega(df, omega_ymin_annot, omega_ymax_annot, palette)
    c <- plot_and_table_comp(df, comp_ymin_annot, comp_ymax_annot, palette)
    a <- plot_and_table_abs(df, abs_ymin_annot, abs_ymax_annot, palette)

    # Dibujar en el dispositivo Shiny
    gridExtra::grid.arrange(
      o$plot, c$plot, a$plot,
      ncol = 3,
      top = grid::textGrob("",
                           gp = grid::gpar(fontsize = 16, fontface = "bold"))
    )

    # Si se pide guardar en disco, volver a ensamblar con arrangeGrob
    if (isTRUE(save)) {
      ggsave(
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

