#' @title Path-mediation diagram for SEM models with multiple distal predictors
#' @description Genera un diagrama de senderos en tres columnas (predictores
#'   distales \eqn{\to} mediadores \eqn{\to} outcome) a partir de un objeto
#'   \code{lavaan} ajustado. Las aristas tienen grosor proporcional a \eqn{|\beta|}
#'   estandarizado, color por bloque de origen, y se atenúan cuando \eqn{p \ge .05}.
#'   Los nodos se rellenan con una paleta pastel agrupada por bloque conceptual.
#'   Diseñado para modelos con muchos predictores distales (10+) en los que
#'   \code{semPlot}/\code{lavaanPlot} producen layouts ilegibles.
#'
#' @param fit Objeto \code{lavaan} ajustado con \code{lavaan::sem()} o
#'   \code{lavaan::cfa()}.
#' @param nodes Data frame con columnas obligatorias \code{id} (nombre de la
#'   variable como aparece en \code{fit}), \code{label} (etiqueta a mostrar; usar
#'   "\\n" para saltos de línea), \code{block} (categoría conceptual para el
#'   color del nodo), \code{layer} (1 = distal, 2 = mediador, 3 = outcome) y
#'   \code{y} (posición vertical dentro de la capa). Si \code{NULL}, la función
#'   intenta deducir el layout automáticamente a partir del modelo lavaan.
#' @param edges_extra Lista opcional de pares \code{c(from, to)} adicionales que
#'   se quieran trazar más allá de las regresiones detectadas. Default \code{NULL}.
#' @param x_positions Vector numérico de longitud 3 con las coordenadas X de las
#'   tres capas. Default \code{c(0, 6, 12)}.
#' @param palette Vector con nombres iguales a las categorías de \code{block}
#'   del data frame \code{nodes}. Si \code{NULL} se usa una paleta pastel por
#'   defecto basada en \code{RColorBrewer::Pastel1}.
#' @param sig_alpha Número entre 0 y 1 para la transparencia de las aristas no
#'   significativas (default \code{0.18}).
#' @param edge_width_range Vector de longitud 2 con el rango de grosor de las
#'   aristas (default \code{c(0.3, 2.4)}).
#' @param sig_threshold Umbral de significancia para diferenciar aristas
#'   (default \code{0.05}).
#' @param show_n_s Lógico. Si \code{FALSE}, las aristas no significativas se
#'   omiten por completo (default \code{TRUE}).
#' @param show_edge_labels Lógico. Mostrar el valor de \eqn{\beta} sobre cada
#'   arista significativa (default \code{TRUE}).
#' @param edge_label_decimals Número de decimales para los labels de aristas
#'   (default 2).
#' @param shrink_dx,shrink_dy Distancias en X e Y por las que se acortan las
#'   aristas para que no se solapen con los nodos (default 1.2 y 0.7).
#' @param node_size,node_label_size,edge_label_size Tamaños de fuente para
#'   los textos.
#' @param fit_indices Vector con nombres de \code{lavaan::fitMeasures()} a
#'   incluir en el subtítulo (default \code{c("cfi.robust","tli.robust",
#'   "rmsea.robust","srmr")}).
#' @param show_r2 Lógico. Anexar los R² de las variables endógenas al subtítulo
#'   (default \code{TRUE}).
#' @param title,subtitle,caption Cadenas de texto para los componentes
#'   correspondientes. Si \code{subtitle = NULL}, se construye automáticamente
#'   con los \code{fit_indices} y los R² (cuando \code{show_r2 = TRUE}).
#' @param layer_labels Vector de longitud 3 con las etiquetas inferiores de
#'   las tres capas (default \code{c("Predictores distales","Mediadores",
#'   "Variable dependiente")}).
#' @param show_layer_labels Lógico. Mostrar las etiquetas de capa al pie del
#'   gráfico (default \code{TRUE}).
#'
#' @return Un objeto \code{ggplot} listo para imprimir, guardar con
#'   \code{ggsave()} o componer con \code{patchwork}.
#'
#' @details
#' La función toma los coeficientes estandarizados de
#' \code{lavaan::standardizedSolution()} y construye el data frame de aristas
#' filtrando las regresiones (\code{op == "~"}) que conectan nodos del data
#' frame \code{nodes}. Los efectos indirectos definidos con \code{:=} en la
#' sintaxis lavaan no se grafican aquí; para visualizarlos en otra capa, use
#' \code{\link{plot_mediation_forest}}.
#'
#' El layout en tres columnas se inspira en Li et al. (2026, BMC Public Health,
#' figura del modelo de mediación dual de presión académica) y en Bakioglu,
#' Korkmaz y Ercan (2020, IJMHA, modelo de mediación del miedo a la COVID-19).
#'
#' @references
#' Bakioglu, F., Korkmaz, O., & Ercan, H. (2020). Fear of COVID-19 and
#' positivity: Mediating role of intolerance of uncertainty, depression, anxiety
#' and stress. \emph{International Journal of Mental Health and Addiction}.
#' \doi{10.1007/s11469-020-00331-y}
#'
#' Li, Y., Wang, J., Luo, W., Zhou, H., Zhang, J., Xu, J., et al. (2026).
#' Academic pressure and suicide risk among adolescents: A dual mediation model
#' of depression and school cohesion. \emph{BMC Public Health, 26}, 335.
#' \doi{10.1186/s12889-025-25805-3}
#'
#' @examples
#' \dontrun{
#' library(lavaan)
#' library(PsyMetricTools)
#'
#' # Modelo de senderos: 3 predictores distales -> 2 mediadores -> 1 outcome
#' mod <- '
#'   M1 ~ X1 + X2 + X3
#'   M2 ~ X1 + X2 + X3
#'   Y  ~ M1 + M2 + X1 + X2 + X3
#' '
#' fit <- sem(mod, data = your_data, estimator = "MLR", missing = "fiml")
#'
#' nodes <- data.frame(
#'   id    = c("X1","X2","X3","M1","M2","Y"),
#'   label = c("Estilo 1","Estilo 2","Demogr","Mediador 1","Mediador 2","Outcome"),
#'   block = c("Habito","Habito","Demogr","Afecto","Afecto","Outcome"),
#'   layer = c(1,1,1,2,2,3),
#'   y     = c(2,0,-2,1,-1,0)
#' )
#'
#' plot_path_mediation(fit, nodes,
#'                     title = "Modelo de mediación afectiva",
#'                     palette = c(Habito = "#FFE082", Demogr = "#B0BEC5",
#'                                 Afecto = "#F4A8A8", Outcome = "#90CAF9"))
#' }
#' @importFrom ggplot2 ggplot aes geom_segment geom_label arrow unit
#'   scale_color_manual scale_fill_manual scale_linewidth_continuous
#'   scale_alpha_manual coord_cartesian theme_void theme element_text
#'   margin annotate labs guides guide_legend
#' @importFrom dplyr filter mutate select left_join bind_rows
#' @importFrom scales alpha
#' @importFrom lavaan standardizedSolution fitMeasures lavInspect
#' @export
plot_path_mediation <- function(fit,
                                nodes,
                                edges_extra = NULL,
                                x_positions = c(0, 6, 12),
                                palette = NULL,
                                sig_alpha = 0.18,
                                edge_width_range = c(0.3, 2.4),
                                sig_threshold = 0.05,
                                show_n_s = TRUE,
                                show_edge_labels = TRUE,
                                edge_label_decimals = 2,
                                shrink_dx = 1.2,
                                shrink_dy = 0.7,
                                node_size = 3.4,
                                node_label_size = 3.4,
                                edge_label_size = 2.7,
                                fit_indices = c("cfi.robust","tli.robust",
                                                "rmsea.robust","srmr"),
                                show_r2 = TRUE,
                                title = NULL,
                                subtitle = NULL,
                                caption = NULL,
                                layer_labels = c("Predictores distales",
                                                 "Mediadores",
                                                 "Variable dependiente"),
                                show_layer_labels = TRUE) {

  # ---- Validaciones --------------------------------------------------------
  if (!inherits(fit, "lavaan"))
    stop("`fit` debe ser un objeto lavaan ajustado.", call. = FALSE)
  if (missing(nodes) || !is.data.frame(nodes))
    stop("`nodes` debe ser un data.frame con las columnas id, label, block, layer, y.",
         call. = FALSE)
  required_cols <- c("id","label","block","layer","y")
  miss <- setdiff(required_cols, names(nodes))
  if (length(miss) > 0)
    stop("Faltan columnas en `nodes`: ", paste(miss, collapse = ", "), call. = FALSE)

  # ---- Coordenadas X por capa ---------------------------------------------
  nodes$x <- x_positions[nodes$layer]

  # ---- Paleta por defecto --------------------------------------------------
  if (is.null(palette)) {
    blocks <- unique(nodes$block)
    default_pal <- c("#FFE082","#FFAB91","#B0BEC5","#F4A8A8",
                     "#A5D6A7","#90CAF9","#CE93D8","#FFCC80","#80CBC4")
    palette <- setNames(default_pal[seq_along(blocks)], blocks)
  }

  # ---- Coeficientes estandarizados ----------------------------------------
  est <- lavaan::standardizedSolution(fit)

  get_b <- function(dv, iv) {
    f <- est[est$op == "~" & est$lhs == dv & est$rhs == iv, ]
    if (nrow(f) == 0) return(c(NA_real_, NA_real_))
    c(f$est.std[1], f$pvalue[1])
  }

  # ---- Construcción de aristas -------------------------------------------
  pairs <- expand.grid(from = nodes$id, to = nodes$id, stringsAsFactors = FALSE)
  pairs <- pairs[pairs$from != pairs$to, ]
  if (!is.null(edges_extra)) {
    extra_df <- do.call(rbind, lapply(edges_extra,
                                       function(z) data.frame(from = z[1], to = z[2])))
    pairs <- rbind(pairs, extra_df)
  }
  beta_p <- t(mapply(get_b, pairs$to, pairs$from))
  edges <- data.frame(from = pairs$from, to = pairs$to,
                      beta = beta_p[, 1], p = beta_p[, 2],
                      stringsAsFactors = FALSE)
  edges <- edges[!is.na(edges$beta), ]

  # ---- Decoración de aristas ----------------------------------------------
  edges <- merge(edges,
                 data.frame(from = nodes$id, x_from = nodes$x,
                            y_from = nodes$y, block_from = nodes$block,
                            stringsAsFactors = FALSE),
                 by = "from", all.x = TRUE)
  edges <- merge(edges,
                 data.frame(to = nodes$id, x_to = nodes$x, y_to = nodes$y,
                            stringsAsFactors = FALSE),
                 by = "to", all.x = TRUE)
  edges$abs_b <- abs(edges$beta)
  edges$sig   <- ifelse(edges$p < sig_threshold,
                        sprintf("Significativo (p<%s)", sub("0", "", sig_threshold)),
                        "n.s.")
  fmt <- paste0("%.", edge_label_decimals, "f")
  edges$label <- sprintf(fmt, edges$beta)

  # ---- Acortar flechas ----------------------------------------------------
  shrink_one <- function(x1, y1, x2, y2) {
    vx <- x2 - x1; vy <- y2 - y1; L <- sqrt(vx^2 + vy^2)
    if (is.na(L) || L == 0) return(c(x1, y1, x2, y2))
    ux <- vx / L; uy <- vy / L
    c(x1 + ux*shrink_dx, y1 + uy*shrink_dy,
      x2 - ux*shrink_dx, y2 - uy*shrink_dy)
  }
  exy <- t(mapply(shrink_one, edges$x_from, edges$y_from,
                              edges$x_to,   edges$y_to))
  edges$x_from2 <- exy[, 1]; edges$y_from2 <- exy[, 2]
  edges$x_to2   <- exy[, 3]; edges$y_to2   <- exy[, 4]

  # ---- Posición de etiquetas ----------------------------------------------
  edges$mx <- (edges$x_from + edges$x_to) / 2
  edges$my <- (edges$y_from + edges$y_to) / 2
  edges$dx <- edges$x_to - edges$x_from
  edges$dy <- edges$y_to - edges$y_from
  edges$L  <- sqrt(edges$dx^2 + edges$dy^2)
  edges$lab_x <- edges$mx + (-edges$dy / edges$L) * 0.15
  edges$lab_y <- edges$my + ( edges$dx / edges$L) * 0.15

  # ---- Subtítulo automático ----------------------------------------------
  if (is.null(subtitle)) {
    fi <- tryCatch(lavaan::fitMeasures(fit, fit_indices), error = function(e) NULL)
    sub_pieces <- character(0)
    if (!is.null(fi) && length(fi) > 0) {
      labs <- toupper(sub("\\.robust$", "", names(fi)))
      sub_pieces <- c(sub_pieces,
        paste(sprintf("%s = %.3f", labs, as.numeric(fi)), collapse = " • "))
    }
    if (show_r2) {
      r2 <- tryCatch(lavaan::lavInspect(fit, "r2"), error = function(e) NULL)
      if (!is.null(r2)) {
        sub_pieces <- c(sub_pieces,
          paste0("R² — ",
                 paste(sprintf("%s = %.2f", names(r2), r2), collapse = ", ")))
      }
    }
    n_total <- tryCatch(lavaan::lavInspect(fit, "ntotal"), error = function(e) NA_integer_)
    if (!is.na(n_total))
      sub_pieces <- c(sub_pieces, sprintf("N = %d", n_total))
    subtitle <- paste(sub_pieces, collapse = "\n")
  }

  # ---- Construcción del ggplot --------------------------------------------
  p <- ggplot2::ggplot()

  if (show_n_s) {
    p <- p +
      ggplot2::geom_segment(
        data = edges[edges$p >= sig_threshold, , drop = FALSE],
        ggplot2::aes(x = .data$x_from2, xend = .data$x_to2,
                     y = .data$y_from2, yend = .data$y_to2,
                     linewidth = .data$abs_b, color = .data$block_from),
        arrow = ggplot2::arrow(length = ggplot2::unit(0.18, "cm"),
                                type = "closed"),
        lineend = "round", alpha = sig_alpha)
  }

  p <- p +
    ggplot2::geom_segment(
      data = edges[edges$p < sig_threshold, , drop = FALSE],
      ggplot2::aes(x = .data$x_from2, xend = .data$x_to2,
                   y = .data$y_from2, yend = .data$y_to2,
                   linewidth = .data$abs_b, color = .data$block_from),
      arrow = ggplot2::arrow(length = ggplot2::unit(0.18, "cm"), type = "closed"),
      lineend = "round", alpha = 1)

  if (show_edge_labels) {
    p <- p +
      ggplot2::geom_label(
        data = edges[edges$p < sig_threshold, , drop = FALSE],
        ggplot2::aes(x = .data$lab_x, y = .data$lab_y, label = .data$label),
        color = "grey20", size = edge_label_size, fontface = "bold",
        label.padding = ggplot2::unit(0.10, "lines"),
        label.size = 0,
        fill = scales::alpha("white", 0.95))
  }

  p <- p +
    ggplot2::geom_label(
      data = nodes,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label,
                   fill = .data$block),
      color = "grey15", size = node_label_size, fontface = "bold",
      label.padding = ggplot2::unit(0.40, "lines"),
      label.r = ggplot2::unit(0.30, "lines"),
      label.size = 0, lineheight = 0.95) +
    ggplot2::scale_color_manual(values = palette, name = "Bloque origen") +
    ggplot2::scale_fill_manual(values = palette, name = "Bloque") +
    ggplot2::scale_linewidth_continuous(
      range = edge_width_range,
      name = expression("|" * beta * "|"),
      breaks = c(0.05, 0.15, 0.30, 0.50)) +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption) +
    ggplot2::guides(color = ggplot2::guide_legend(order = 1,
                     override.aes = list(linewidth = 1.8)),
                    linewidth = ggplot2::guide_legend(order = 2),
                    fill = "none") +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14,
                                          hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5,
                                             color = "grey25",
                                             lineheight = 1.15,
                                             margin = ggplot2::margin(b = 12)),
      plot.caption = ggplot2::element_text(size = 8.5, hjust = 0.5,
                                            color = "grey35",
                                            lineheight = 1.2,
                                            margin = ggplot2::margin(t = 10)),
      legend.position = "bottom", legend.box = "horizontal",
      legend.spacing.x = ggplot2::unit(0.5, "cm"),
      plot.margin = ggplot2::margin(15, 30, 15, 30))

  # ---- Etiquetas inferiores por capa --------------------------------------
  if (show_layer_labels && length(layer_labels) == 3) {
    y_min <- min(nodes$y) - 2
    p <- p +
      ggplot2::annotate("text", x = x_positions[1], y = y_min,
                         label = layer_labels[1], size = 3.0,
                         fontface = "italic", color = "grey30") +
      ggplot2::annotate("text", x = x_positions[2], y = y_min,
                         label = layer_labels[2], size = 3.0,
                         fontface = "italic", color = "grey30") +
      ggplot2::annotate("text", x = x_positions[3], y = y_min,
                         label = layer_labels[3], size = 3.0,
                         fontface = "italic", color = "grey30") +
      ggplot2::coord_cartesian(
        xlim = c(min(x_positions) - 2.5, max(x_positions) + 2),
        ylim = c(y_min - 1, max(nodes$y) + 2),
        clip = "off")
  } else {
    p <- p +
      ggplot2::coord_cartesian(
        xlim = c(min(x_positions) - 2.5, max(x_positions) + 2),
        ylim = c(min(nodes$y) - 1, max(nodes$y) + 2),
        clip = "off")
  }

  p
}
