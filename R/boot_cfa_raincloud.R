#' @title Raincloud Plot para Bootstrap CFA
#' @description Crea visualizaciones raincloud elegantes para resultados de bootstrap CFA.
#' @param df Data frame con resultados de boot_cfa().
#' @param save Logical. Guardar el grafico (default TRUE).
#' @param path Ruta para guardar el grafico (default "Plot_boot_raincloud.jpg").
#' @param dpi Resolucion en DPI (default 600).
#' @param exclude_indices Vector de indices a excluir (e.g., c("RMSEA")).
#' @param color_scheme Esquema de color: "ocean", "sunset", "forest", "lavender", "monochrome", "elegant".
#' @param show_stats Mostrar estadisticos en el grafico (default TRUE).
#' @param theme_style Estilo: "modern", "minimal", "dark" (default "modern").
#' @param ... Argumentos adicionales para ggsave.
#' @return Un objeto ggplot.
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
#' # Create raincloud plot with ocean color scheme
#' boot_cfa_raincloud(boot_results,
#'                    save = TRUE,
#'                    path = "bootstrap_raincloud.jpg",
#'                    color_scheme = "ocean",
#'                    show_stats = TRUE,
#'                    theme_style = "modern")
#'
#' # Dark theme with sunset colors
#' boot_cfa_raincloud(boot_results,
#'                    save = FALSE,
#'                    color_scheme = "sunset",
#'                    theme_style = "dark")
#'
#' # Available color schemes: "ocean", "sunset", "forest", "lavender", "monochrome", "elegant"
#' }
#' @export
boot_cfa_raincloud <- function(df,
                                save = TRUE,
                                path = "Plot_boot_raincloud.jpg",
                                dpi = 600,
                                exclude_indices = NULL,
                                color_scheme = "ocean",
                                show_stats = TRUE,
                                theme_style = "modern",
                                ...) {

  suppressWarnings({

    # =========================================================================
    # 1. Esquemas de colores elegantes
    # =========================================================================
    get_colors <- function(scheme, n) {
      schemes <- list(
        ocean = c("#0077b6", "#00b4d8", "#90e0ef", "#caf0f8"),
        sunset = c("#d00000", "#e85d04", "#faa307", "#ffba08"),
        forest = c("#2d6a4f", "#40916c", "#52b788", "#95d5b2"),
        lavender = c("#7b2cbf", "#9d4edd", "#c77dff", "#e0aaff"),
        monochrome = c("#212529", "#495057", "#adb5bd", "#dee2e6"),
        elegant = c("#264653", "#2a9d8f", "#e9c46a", "#f4a261")
      )

      colors <- if (scheme %in% names(schemes)) schemes[[scheme]] else schemes$ocean
      grDevices::colorRampPalette(colors)(n)
    }

    # =========================================================================
    # 2. Funcion para crear half-violin (geom personalizado)
    # =========================================================================
    geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                                  position = "dodge", trim = TRUE, scale = "area",
                                  show.legend = NA, inherit.aes = TRUE, ...) {
      ggplot2::layer(
        data = data, mapping = mapping, stat = stat, geom = GeomFlatViolin,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(trim = trim, scale = scale, ...)
      )
    }

    GeomFlatViolin <- ggplot2::ggproto("GeomFlatViolin", ggplot2::Geom,
      setup_data = function(data, params) {
        data$width <- data$width %||% params$width %||% (ggplot2::resolution(data$x, FALSE) * 0.9)
        data <- dplyr::group_by(data, group)
        data <- dplyr::mutate(data, ymin = min(y), ymax = max(y), xmin = x, xmax = x + width / 2)
        dplyr::ungroup(data)
      },
      draw_group = function(data, panel_params, coord, ...) {
        data <- transform(data,
                          xminv = x,
                          xmaxv = x + violinwidth * (xmax - x))
        newdata <- rbind(
          transform(data, x = xminv)[order(data$y), ],
          transform(data, x = xmaxv)[order(data$y, decreasing = TRUE), ]
        )
        newdata <- rbind(newdata, newdata[1, ])
        grid::gTree(children = grid::gList(
                         ggplot2::GeomPolygon$draw_panel(newdata, panel_params, coord, ...)),
                         name = "geom_flat_violin")
      },
      draw_key = ggplot2::draw_key_polygon,
      default_aes = ggplot2::aes(weight = 1, colour = "grey20", fill = "white",
                        linewidth = 0.5, alpha = NA, linetype = "solid"),
      required_aes = c("x", "y")
    )

    # =========================================================================
    # 3. Preparar datos
    # =========================================================================
    prepare_omega_data <- function(df_repli) {
      if ("fit_measures1" %in% names(df_repli)) {
        idx <- which(names(df_repli) == "fit_measures1")
        dat <- df_repli[, -(1:idx)]
      } else {
        dat <- df_repli[, sapply(df_repli, is.numeric)]
      }

      dat_long <- tidyr::pivot_longer(dat, tidyselect::everything(),
                                       names_to = "Index", values_to = "Value")
      dat_long$Index <- substr(dat_long$Index, 1, 3)
      dat_long$Type <- "Reliability"
      dat_long$Label <- paste0("omega (", dat_long$Index, ")")
      dat_long
    }

    prepare_comp_data <- function(df_repli, exclude = NULL) {
      dfm <- purrr::map_dfr(df_repli$fit_measures1, tibble::as_tibble)
      indices <- setdiff(c("CFI", "TLI"), toupper(exclude))
      if (length(indices) == 0) return(NULL)

      dat_long <- tidyr::pivot_longer(dfm[, indices, drop = FALSE],
                                       tidyselect::everything(),
                                       names_to = "Index", values_to = "Value")
      dat_long$Type <- "Comparative"
      dat_long$Label <- dat_long$Index
      dat_long
    }

    prepare_abs_data <- function(df_repli, exclude = NULL) {
      dfm <- purrr::map_dfr(df_repli$fit_measures1, tibble::as_tibble)
      indices <- setdiff(c("RMSEA", "SRMR", "CRMR"), toupper(exclude))
      if (length(indices) == 0) return(NULL)

      dat_long <- tidyr::pivot_longer(dfm[, indices, drop = FALSE],
                                       tidyselect::everything(),
                                       names_to = "Index", values_to = "Value")
      dat_long$Type <- "Absolute"
      dat_long$Label <- dat_long$Index
      dat_long
    }

    # =========================================================================
    # 4. Crear un panel de raincloud elegante
    # =========================================================================
    create_panel <- function(data, title, colors, show_stats, theme_style) {
      if (is.null(data) || nrow(data) == 0) return(NULL)

      n_indices <- length(unique(data$Label))
      panel_colors <- colors[1:n_indices]

      # Calcular estadisticos
      stats <- data %>%
        dplyr::group_by(Label) %>%
        dplyr::summarise(
          Mean = mean(Value, na.rm = TRUE),
          SD = stats::sd(Value, na.rm = TRUE),
          Median = stats::median(Value, na.rm = TRUE),
          Q1 = stats::quantile(Value, 0.25, na.rm = TRUE),
          Q3 = stats::quantile(Value, 0.75, na.rm = TRUE),
          Min = min(Value, na.rm = TRUE),
          Max = max(Value, na.rm = TRUE),
          .groups = "drop"
        )

      # Anadir jitter a los datos
      set.seed(123)
      data <- dplyr::group_by(data, Label)
      data <- dplyr::mutate(data,
          x_base = as.numeric(factor(Label, levels = unique(data$Label))),
          x_jitter = x_base - 0.15 + stats::runif(dplyr::n(), -0.03, 0.03)
        )
      data <- dplyr::ungroup(data)

      # Determinar colores de fondo segun tema
      if (theme_style == "dark") {
        bg_color <- "#1a1a2e"
        text_color <- "#eaeaea"
        grid_color <- "#2d2d44"
        point_color <- "white"
      } else {
        bg_color <- "#fafafa"
        text_color <- "#2d3436"
        grid_color <- "#ecf0f1"
        point_color <- "#2d3436"
      }

      # Crear plot base
      p <- ggplot2::ggplot(data, ggplot2::aes(x = Label, y = Value))

      # Half violin
      p <- p + geom_flat_violin(
        ggplot2::aes(fill = Label),
        position = ggplot2::position_nudge(x = 0.2),
        alpha = 0.85,
        color = NA,
        trim = FALSE,
        scale = "width"
      )

      # Puntos individuales
      p <- p + ggplot2::geom_point(
        ggplot2::aes(x = x_jitter, color = Label),
        size = 1.2,
        alpha = 0.4
      )

      # Boxplot delgado
      p <- p + ggplot2::geom_boxplot(
        ggplot2::aes(fill = Label),
        width = 0.12,
        alpha = 0.9,
        outlier.shape = NA,
        color = "white",
        linewidth = 0.4
      )

      # Punto de media
      p <- p + ggplot2::geom_point(
        data = stats,
        ggplot2::aes(x = Label, y = Mean),
        size = 3,
        shape = 23,
        fill = "white",
        color = point_color,
        stroke = 1.2
      )

      # Colores
      p <- p + ggplot2::scale_fill_manual(values = panel_colors) +
        ggplot2::scale_color_manual(values = panel_colors)

      # Voltear coordenadas
      p <- p + ggplot2::coord_flip()

      # Colores secundarios para texto
      text_color_light <- if (theme_style == "dark") "#b0b0b0" else "#636e72"

      # Tema base
      p <- p + ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          legend.position = "none",
          plot.title = ggplot2::element_text(
            face = "bold",
            size = 13,
            color = text_color,
            hjust = 0,
            margin = ggplot2::margin(b = 15)
          ),
          plot.subtitle = ggplot2::element_text(
            size = 10,
            color = text_color_light,
            margin = ggplot2::margin(b = 10)
          ),
          axis.title = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text(
            size = 11,
            face = "bold",
            color = text_color
          ),
          axis.text.x = ggplot2::element_text(
            size = 10,
            color = text_color_light
          ),
          panel.grid.major.x = ggplot2::element_line(color = grid_color, linewidth = 0.3),
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          plot.background = ggplot2::element_rect(fill = bg_color, color = NA),
          panel.background = ggplot2::element_rect(fill = bg_color, color = NA),
          plot.margin = ggplot2::margin(20, 25, 15, 15)
        )

      # Titulo
      p <- p + ggplot2::labs(title = title)

      # Anadir estadisticos como texto
      if (show_stats) {
        stats_labels <- stats
        stats_labels$stat_text <- sprintf("M = %.2f, SD = %.2f", stats_labels$Mean, stats_labels$SD)
        stats_labels$x_pos <- as.numeric(factor(stats_labels$Label, levels = unique(data$Label)))

        # Determinar posicion x para las anotaciones
        x_range <- range(data$Value, na.rm = TRUE)
        x_annotation <- x_range[2] + (x_range[2] - x_range[1]) * 0.02

        p <- p + ggplot2::geom_text(
          data = stats_labels,
          ggplot2::aes(x = x_pos + 0.35, y = x_annotation, label = stat_text),
          hjust = 1,
          size = 3.2,
          color = text_color_light,
          fontface = "italic"
        )
      }

      return(p)
    }

    # =========================================================================
    # 5. Generar plots
    # =========================================================================

    # Preparar datos
    omega_data <- prepare_omega_data(df)
    comp_data <- prepare_comp_data(df, exclude_indices)
    abs_data <- prepare_abs_data(df, exclude_indices)

    # Contar total de indices para paleta
    total_indices <- sum(
      length(unique(omega_data$Label)),
      if (!is.null(comp_data)) length(unique(comp_data$Label)) else 0,
      if (!is.null(abs_data)) length(unique(abs_data$Label)) else 0
    )

    all_colors <- get_colors(color_scheme, total_indices + 2)

    # Crear paneles
    plots <- list()
    color_idx <- 1

    # Panel Omega
    if (!is.null(omega_data) && nrow(omega_data) > 0) {
      n <- length(unique(omega_data$Label))
      p_omega <- create_panel(
        omega_data,
        "Reliability Coefficients",
        all_colors[color_idx:(color_idx + n - 1)],
        show_stats,
        theme_style
      )
      plots <- c(plots, list(p_omega))
      color_idx <- color_idx + n
    }

    # Panel Comparativos
    if (!is.null(comp_data) && nrow(comp_data) > 0) {
      n <- length(unique(comp_data$Label))
      p_comp <- create_panel(
        comp_data,
        "Comparative Fit Indices",
        all_colors[color_idx:(color_idx + n - 1)],
        show_stats,
        theme_style
      )
      plots <- c(plots, list(p_comp))
      color_idx <- color_idx + n
    }

    # Panel Absolutos
    if (!is.null(abs_data) && nrow(abs_data) > 0) {
      n <- length(unique(abs_data$Label))
      p_abs <- create_panel(
        abs_data,
        "Absolute Fit Indices",
        all_colors[color_idx:(color_idx + n - 1)],
        show_stats,
        theme_style
      )
      plots <- c(plots, list(p_abs))
    }

    # Combinar
    n_plots <- length(plots)
    if (n_plots == 0) stop("No hay datos para graficar.")

    # Dibujar
    args <- c(plots, list(ncol = n_plots))
    do.call(gridExtra::grid.arrange, args)

    # Guardar
    if (isTRUE(save)) {
      combined <- do.call(gridExtra::arrangeGrob, args)
      ggplot2::ggsave(
        filename = path,
        plot = combined,
        height = 14,
        width = 8 * n_plots,
        dpi = dpi,
        units = "cm",
        bg = if(theme_style == "dark") "#1a1a2e" else "white",
        ...
      )
      message("Grafico guardado en: ", path)
    }

    invisible(do.call(gridExtra::arrangeGrob, args))
  })
}
