boot_cfa_plot <- function(df, save = TRUE, dpi = 600,
                           omega_ymin_annot = NULL, omega_ymax_annot = NULL,
                           comp_ymin_annot = NULL, comp_ymax_annot = NULL,
                           abs_ymin_annot = NULL, abs_ymax_annot = NULL,
                           palette = "grey") {
  suppressWarnings({
    #---------------------------------------------------------------------------
    # Cargar librerías necesarias
    #---------------------------------------------------------------------------
    library(ggplot2)
    library(tidyr)
    library(dplyr)
    library(gridExtra)
    library(gtable)
    library(ggpubr)
    library(wesanderson)   # Para paletas de Wes Anderson
    library(purrr)
    library(reshape2)

    #---------------------------------------------------------------------------
    # 1. Función que define la paleta para los boxplots
    #---------------------------------------------------------------------------
    get_palette <- function(palette, n) {
      if (palette %in% names(wesanderson::wes_palettes)) {
        return(wes_palette(palette, n, type = "discrete"))
      } else if (palette == "grey") {
        return(gray.colors(n, start = 0.5, end = 0.9))
      } else {
        return(rep(palette, n))
      }
    }

    #---------------------------------------------------------------------------
    # 2. Función que define el color del encabezado de la tabla
    #---------------------------------------------------------------------------
    get_header_color <- function(palette) {
      if (palette == "grey") {
        return("grey85")
      } else if (palette %in% names(wesanderson::wes_palettes)) {
        return(wes_palette(palette, 1, type = "discrete"))
      } else {
        return(palette)
      }
    }

    #---------------------------------------------------------------------------
    # 3. Función auxiliar para añadir líneas horizontales en la tabla:
    #---------------------------------------------------------------------------
    add_horizontal_borders <- function(tbl) {
      tbl <- gtable::gtable_add_grob(
        tbl,
        grobs = grid::segmentsGrob(
          x0 = unit(0, "npc"), x1 = unit(1, "npc"),
          y0 = unit(1, "npc"), y1 = unit(1, "npc"),
          gp = grid::gpar(lwd = 2)
        ),
        t = 1, l = 1, r = ncol(tbl)
      )
      if (nrow(tbl) > 1) {
        tbl <- gtable::gtable_add_grob(
          tbl,
          grobs = grid::segmentsGrob(
            x0 = unit(0, "npc"), x1 = unit(1, "npc"),
            y0 = unit(1, "npc"), y1 = unit(1, "npc"),
            gp = grid::gpar(lwd = 2)
          ),
          t = 2, l = 1, r = ncol(tbl)
        )
      }
      tbl <- gtable::gtable_add_grob(
        tbl,
        grobs = grid::segmentsGrob(
          x0 = unit(0, "npc"), x1 = unit(1, "npc"),
          y0 = unit(0, "npc"), y1 = unit(0, "npc"),
          gp = grid::gpar(lwd = 2)
        ),
        t = nrow(tbl), l = 1, r = ncol(tbl)
      )
      return(tbl)
    }

    #---------------------------------------------------------------------------
    # 4. Función que crea el "theme" de la tabla:
    #---------------------------------------------------------------------------
    make_table_theme <- function(palette) {
      header_col <- get_header_color(palette)
      gridExtra::ttheme_default(
        core = list(bg_params = list(fill = "white", col = NA),
                    fg_params = list(fontface = 1)),
        colhead = list(bg_params = list(fill = header_col, col = NA),
                       fg_params = list(col = "black", fontface = c(1,1,3,1,1))),
        rowhead = list(fg_params = list(col = "black", fontface = 1)),
        base_size = 8
      )
    }

    #---------------------------------------------------------------------------
    # 5. Función interna: plot_and_table_omega
    #---------------------------------------------------------------------------
    plot_and_table_omega <- function(df_repli,
                                     omega_ymin_annot = NULL, omega_ymax_annot = NULL,
                                     palette = "grey") {
      # Generar la tabla de estadísticas de fiabilidad utilizando las columnas
      # que están después de "fit_measures1" y acortando los nombres a 3 caracteres
      res_omega_table <- df_repli %>%
        select(-(1:which(names(.) == "fit_measures1"))) %>%
        pivot_longer(
          cols = everything(),
          names_to = "Variable",
          values_to = "Value"
        ) %>%
        mutate(Variable = substr(Variable, 1, 3)) %>%
        group_by(Variable) %>%
        summarise(
          mean = round(mean(Value, na.rm = TRUE), 2),
          sd   = round(sd(Value, na.rm = TRUE), 2),
          min  = round(min(Value, na.rm = TRUE), 2),
          max  = round(max(Value, na.rm = TRUE), 2)
        ) %>%
        ungroup()

      if (is.null(omega_ymin_annot)) {
        omega_ymin_annot <- max(res_omega_table$mean)
      }
      if (is.null(omega_ymax_annot)) {
        omega_ymax_annot <- 0.92
      }

      # Preparar los datos para el gráfico de forma similar:
      data_long <- df_repli %>%
        select(-(1:which(names(.) == "fit_measures1"))) %>%
        pivot_longer(
          cols = everything(),
          names_to = "Reliability",
          values_to = "value"
        ) %>%
        mutate(Reliability = substr(Reliability, 1, 3))

      # Crear la tabla con el tema
      table_theme <- make_table_theme(palette)
      table_omega <- gridExtra::tableGrob(res_omega_table, rows = NULL, theme = table_theme)
      table_omega <- add_horizontal_borders(table_omega)

      categories <- unique(data_long$Reliability)

      # Crear el gráfico de fiabilidad
      plot <- ggplot(data_long, aes(x = Reliability, y = value, fill = Reliability)) +
        geom_boxplot(outlier.shape = 16) +
        theme_bw() +
        scale_y_continuous(
          limits = c(min(res_omega_table$min) - 0.1, 1),
          breaks = seq(min(res_omega_table$min) - 0.1, 1, by = 0.05)
        ) +
        scale_fill_manual(values = get_palette(palette, length(categories))) +
        labs(y = "\u03C9 values") +
        theme(legend.position = "none") +
        annotation_custom(
          table_omega,
          xmin = 1, xmax = length(res_omega_table$Variable),
          ymin = omega_ymin_annot, ymax = omega_ymax_annot
        )

      return(list(table = res_omega_table, plot = plot))
    }


    #---------------------------------------------------------------------------
    # 6. Función interna: Plot_and_Table_comparative (CFI, TLI)
    #---------------------------------------------------------------------------
    Plot_and_Table_comparative <- function(df_repli,
                                           comp_ymin_annot = NULL, comp_ymax_annot = NULL,
                                           palette = "grey") {
      table <- map_dfr(df_repli$fit_measures1, ~as_tibble(.)) %>%
        select(CFI, TLI) %>%
        pivot_longer(cols = everything(), names_to = "Fit", values_to = "Value") %>%
        mutate(Value = round(Value, 3)) %>%
        group_by(Fit) %>%
        summarise(
          mean = round(mean(Value, na.rm = TRUE), 2),
          sd = round(sd(Value, na.rm = TRUE), 2),
          min = round(min(Value, na.rm = TRUE), 2),
          max = round(max(Value, na.rm = TRUE), 2)
        ) %>%
        ungroup()

      if (is.null(comp_ymin_annot) || is.null(comp_ymax_annot)) {
        min_adjusted <- min(table$min) - 0.05
        comp_ymin_annot <- min_adjusted
        comp_ymax_annot <- comp_ymin_annot + 0.05
      }

      ymin_limit <- min(table$min)
      if (ymin_limit > 0.95) {
        ymin_limit <- 0.90
      }

      table_theme <- make_table_theme(palette)
      table_comp <- gridExtra::tableGrob(table, rows = NULL, theme = table_theme)
      table_comp <- add_horizontal_borders(table_comp)

      data_plot <- map_dfr(df_repli$fit_measures1, ~as_tibble(.)) %>%
        select(CFI, TLI) %>%
        pivot_longer(cols = everything(), names_to = "Fit", values_to = "Value") %>%
        mutate(Value = round(Value, 3))

      categories <- unique(data_plot$Fit)

      plot <- ggplot(data_plot, aes(x = Fit, y = Value, fill = Fit)) +
        geom_boxplot(outlier.shape = 16, outlier.size = 1) +
        theme_bw() +
        coord_cartesian(ylim = c(ymin_limit, 1.00)) +
        scale_fill_manual(values = get_palette(palette, length(categories))) +
        theme(legend.position = "none") +
        annotation_custom(
          table_comp,
          xmin = 0, xmax = 3,
          ymin = comp_ymin_annot, ymax = comp_ymax_annot
        )

      return(list(table = table, plot = plot))
    }

    #---------------------------------------------------------------------------
    # 7. Función interna: Plot_and_Table_absolute (RMSEA, SRMR, CRMR)
    #---------------------------------------------------------------------------
    Plot_and_Table_absolute <- function(df_repli,
                                        abs_ymin_annot = NULL, abs_ymax_annot = NULL,
                                        palette = "grey") {
      table <- map_dfr(df_repli$fit_measures1, ~as_tibble(.)) %>%
        select(RMSEA, SRMR, CRMR) %>%
        pivot_longer(cols = everything(), names_to = "Fit", values_to = "Value") %>%
        mutate(Value = round(Value, 3)) %>%
        group_by(Fit) %>%
        summarise(
          mean = round(mean(Value, na.rm = TRUE), 2),
          sd = round(sd(Value, na.rm = TRUE), 2),
          min = round(min(Value, na.rm = TRUE), 2),
          max = round(max(Value, na.rm = TRUE), 2)
        ) %>%
        ungroup()

      if (is.null(abs_ymin_annot)) {
        abs_ymin_annot <- 0
      }
      if (is.null(abs_ymax_annot)) {
        abs_ymax_annot <- max(table$max)
      }

      table_theme <- make_table_theme(palette)
      table_abs <- gridExtra::tableGrob(table, rows = NULL, theme = table_theme)
      table_abs <- add_horizontal_borders(table_abs)

      data_plot <- map_dfr(df_repli$fit_measures1, ~as_tibble(.)) %>%
        select(RMSEA, SRMR, CRMR) %>%
        pivot_longer(cols = everything(), names_to = "Fit", values_to = "Value") %>%
        mutate(Value = round(Value, 3))

      categories <- unique(data_plot$Fit)

      plot <- ggplot(data_plot, aes(x = Fit, y = Value, fill = Fit)) +
        geom_boxplot(outlier.shape = 16, outlier.size = 1) +
        theme_bw() +
        scale_y_continuous(
          limits = c(0, max(table$max)),
          breaks = seq(0, max(table$max), by = 0.01)
        ) +
        scale_fill_manual(values = get_palette(palette, length(categories))) +
        theme(legend.position = "none") +
        annotation_custom(
          table_abs,
          xmin = 0, xmax = 4,
          ymin = abs_ymin_annot, ymax = abs_ymax_annot
        )

      return(list(table = table, plot = plot))
    }

    #---------------------------------------------------------------------------
    # 8. Configuración final y creación de la figura combinada
    #---------------------------------------------------------------------------
    filename <- "Plot_boot_cfa.jpg"
    height   <- 16
    width    <- 22
    units    <- "cm"

    p1 <- plot_and_table_omega(df, omega_ymin_annot, omega_ymax_annot, palette)
    a1 <- p1$plot
    p2 <- Plot_and_Table_comparative(df, comp_ymin_annot, comp_ymax_annot, palette)
    a2 <- p2$plot
    p3 <- Plot_and_Table_absolute(df, abs_ymin_annot, abs_ymax_annot, palette)
    a3 <- p3$plot

    figure <- ggarrange(a1, a2, a3,
                        labels = c("A", "B", "C"),
                        ncol = 3, nrow = 1)

    if (save) {
      ggsave(filename = filename, plot = figure, height = height, width = width, dpi = dpi, units = units)
    }

    return(figure)
  })
}
