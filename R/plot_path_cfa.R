#' @title Path diagram for a CFA model (psychometric convention, ggplot2)
#' @description Genera un diagrama de senderos para un modelo CFA ajustado
#'   con \code{lavaan::cfa()} siguiendo la convencion psicometrica estandar:
#'   \strong{factores latentes como elipses (circulos)}, \strong{items
#'   observados como rectangulos}, y \strong{cargas dibujadas en abanico}
#'   desde el borde del circulo del factor hacia cada item. Las
#'   correlaciones latentes se representan como curvas dobles entre
#'   factores. Por defecto el dibujo es lineal (blanco y negro), tipo
#'   diagrama de Brown (2015) o Kline; el usuario puede pedir paleta de
#'   color con \code{bw = FALSE} o pasando \code{palette}.
#'
#' @param fit Objeto \code{lavaan} ajustado con \code{lavaan::cfa()} (o
#'   \code{lavaan::sem()} con bloque de medida).
#' @param loading_threshold Numeric. Cargas con \eqn{|\lambda|} bajo el
#'   umbral se omiten (default 0.30).
#' @param show_loading_labels Logical. Imprimir el valor de \eqn{\lambda}
#'   sobre cada flecha (default \code{TRUE}).
#' @param loading_label_decimals Integer. Decimales para etiquetas
#'   (default 2).
#' @param loading_label_position Numeric en (0, 1). Posicion de la
#'   etiqueta a lo largo de la flecha medida desde el factor; default 0.55
#'   (mas cerca del item).
#' @param correlate_factors Logical. Dibujar correlaciones latentes
#'   \eqn{\phi} entre factores (default \code{TRUE}).
#' @param show_correlation_labels Logical. Mostrar el valor de \eqn{\phi}
#'   junto a cada curva (default \code{TRUE}).
#' @param correlation_curvature Numeric. Curvatura de las correlaciones
#'   (default 0.45).
#' @param bw Logical. Si \code{TRUE} (default), dibujo lineal en blanco y
#'   negro al estilo libro de texto. Si \code{FALSE} usa color: ya sea la
#'   \code{palette} provista o una paleta pastel automatica.
#' @param palette Vector con nombres iguales a los factores. Si \code{NULL}
#'   se respeta \code{bw}: blanco y negro o pastel auto. Pasar
#'   \code{palette} fuerza color (override \code{bw}).
#' @param factor_radius Numeric. Radio (en unidades del eje) de los
#'   circulos de factor (default 0.85).
#' @param item_half_width,item_half_height Numeric. Medio ancho y media
#'   altura de los rectangulos de item (defaults 0.65 y 0.28).
#' @param edge_width_range Vector de longitud 2 con el rango de grosor
#'   de las flechas (default \code{c(0.4, 1.6)}).
#' @param node_label_size,edge_label_size Tamanos de fuente para los
#'   textos (defaults 3.0 y 2.7).
#' @param item_x Numeric. Coordenada X de la columna de items (default 0).
#' @param factor_x Numeric. Coordenada X de los centros de los circulos
#'   de factor (default 8).
#' @param item_spacing Numeric. Distancia vertical entre items
#'   consecutivos del mismo factor (default 1).
#' @param group_gap Numeric. Espacio adicional entre grupos de items de
#'   distintos factores (default 1.2).
#' @param fit_indices Vector de nombres de \code{lavaan::fitMeasures()} a
#'   incluir en el subtitulo.
#' @param show_n Logical. Anexar \eqn{N} al subtitulo (default \code{TRUE}).
#' @param title,subtitle,caption Cadenas para los componentes del plot.
#' @param factor_label_map Lista nombrada para renombrar factores (e.g.
#'   \code{list(F1 = "Cognitive", F2 = "Affective")}).
#'
#' @return Un objeto \code{ggplot} listo para imprimir, guardar con
#'   \code{ggsave()} o componer con \code{patchwork}.
#'
#' @details
#' Toma \code{lavaan::standardizedSolution()} y filtra:
#' \itemize{
#'   \item \code{op == "=~"} para construir las cargas factor \eqn{\to} item.
#'   \item \code{op == "~~"} con \code{lhs != rhs} entre factores latentes
#'         para las correlaciones \eqn{\phi}.
#' }
#'
#' Cada flecha sale del \strong{borde} del circulo del factor (no del
#' centro) gracias a un calculo trigonometrico que las distribuye en
#' abanico, evitando la convergencia visual al mismo punto.
#'
#' Las varianzas residuales se omiten para mantener la lectura limpia. Si
#' las necesitas explicitas, usa \code{semPlot::semPaths(..., what = "std")}.
#'
#' @references
#' Brown, T. A. (2015). \emph{Confirmatory factor analysis for applied
#' research} (2nd ed.). Guilford Press.
#'
#' Kline, R. B. (2023). \emph{Principles and practice of structural
#' equation modeling} (5th ed.). Guilford Press.
#'
#' @examples
#' \dontrun{
#' library(lavaan); library(PsyMetricTools)
#'
#' mod <- '
#'   F1 =~ ITEM1 + ITEM2 + ITEM3 + ITEM4 + ITEM5
#'   F2 =~ ITEM6 + ITEM7 + ITEM8 + ITEM9 + ITEM10
#'   F3 =~ ITEM11 + ITEM12 + ITEM13 + ITEM14 + ITEM15
#' '
#' fit <- cfa(mod, data = my_data, ordered = paste0("ITEM", 1:15),
#'            estimator = "WLSMV", std.lv = TRUE)
#'
#' # Default: black & white (textbook style)
#' plot_path_cfa(fit, title = "DERS — final structure")
#'
#' # With colour palette
#' plot_path_cfa(fit, bw = FALSE,
#'   palette = c(F1 = "#90CAF9", F2 = "#F4A8A8", F3 = "#A5D6A7"))
#' }
#' @importFrom ggplot2 ggplot aes geom_segment geom_curve geom_polygon
#'   geom_rect geom_label geom_text arrow unit scale_color_manual
#'   scale_fill_manual scale_linewidth_continuous coord_fixed theme_void
#'   theme element_text margin labs guides guide_legend annotate
#' @importFrom scales alpha
#' @importFrom lavaan standardizedSolution fitMeasures lavInspect
#' @importFrom stats setNames
#' @export
plot_path_cfa <- function(fit,
                          loading_threshold       = 0.30,
                          show_loading_labels     = TRUE,
                          loading_label_decimals  = 2,
                          loading_label_position  = 0.55,
                          correlate_factors       = TRUE,
                          show_correlation_labels = TRUE,
                          correlation_curvature   = 0.45,
                          bw                      = TRUE,
                          palette                 = NULL,
                          factor_radius           = 0.85,
                          item_half_width         = 0.65,
                          item_half_height        = 0.28,
                          edge_width_range        = c(0.4, 1.6),
                          node_label_size         = 3.0,
                          edge_label_size         = 2.7,
                          item_x                  = 0,
                          factor_x                = 8,
                          item_spacing            = 1,
                          group_gap               = 1.2,
                          fit_indices             = c("cfi.scaled","tli.scaled",
                                                       "rmsea.scaled","srmr"),
                          show_n                  = TRUE,
                          title                   = NULL,
                          subtitle                = NULL,
                          caption                 = NULL,
                          factor_label_map        = NULL) {

  # ---- Validation ----------------------------------------------------------
  if (!inherits(fit, "lavaan"))
    stop("`fit` must be a fitted lavaan object.", call. = FALSE)

  est <- lavaan::standardizedSolution(fit)
  loadings <- est[est$op == "=~",
                  c("lhs","rhs","est.std","pvalue"), drop = FALSE]
  if (nrow(loadings) == 0L)
    stop("No factor loadings (op == '=~') found in this model.",
         call. = FALSE)
  loadings <- loadings[abs(loadings$est.std) >= loading_threshold, ]
  if (nrow(loadings) == 0L)
    stop(sprintf("No loadings with |lambda| >= %.2f.", loading_threshold),
         call. = FALSE)

  factors <- unique(loadings$lhs)

  # ---- Item nodes (rectangles) --------------------------------------------
  loadings_sorted <- loadings[order(loadings$lhs,
                                      -abs(loadings$est.std)), ]
  item_nodes_list <- list()
  current_y <- 0
  for (f in factors) {
    f_items <- loadings_sorted[loadings_sorted$lhs == f, ]
    if (nrow(f_items) == 0) next
    ys <- seq(from = current_y, by = -item_spacing,
              length.out = nrow(f_items))
    item_nodes_list[[f]] <- data.frame(
      id    = f_items$rhs,
      label = f_items$rhs,
      block = f,
      x     = item_x,
      y     = ys,
      stringsAsFactors = FALSE)
    current_y <- min(ys) - group_gap
  }
  item_nodes <- do.call(rbind, item_nodes_list)
  item_nodes$xmin <- item_nodes$x - item_half_width
  item_nodes$xmax <- item_nodes$x + item_half_width
  item_nodes$ymin <- item_nodes$y - item_half_height
  item_nodes$ymax <- item_nodes$y + item_half_height

  # ---- Factor nodes (circles centered on the centroid of their items) ----
  factor_label_for <- function(f) {
    if (is.null(factor_label_map) || is.null(factor_label_map[[f]])) f
    else as.character(factor_label_map[[f]])
  }
  factor_nodes <- do.call(rbind, lapply(factors, function(f) {
    its <- item_nodes[item_nodes$block == f, , drop = FALSE]
    data.frame(
      id    = f,
      label = factor_label_for(f),
      block = f,
      x     = factor_x,
      y     = mean(c(min(its$y), max(its$y))),
      r     = factor_radius,
      stringsAsFactors = FALSE)
  }))

  # Build polygons (60-gon) approximating each factor circle, used by
  # geom_polygon. Coord_fixed keeps them visually round.
  .circle_xy <- function(cx, cy, r, n = 60L) {
    th <- seq(0, 2 * pi, length.out = n + 1L)
    data.frame(x = cx + r * cos(th), y = cy + r * sin(th))
  }
  factor_polys <- do.call(rbind, lapply(seq_len(nrow(factor_nodes)),
    function(i) {
      cf <- factor_nodes[i, ]
      d  <- .circle_xy(cf$x, cf$y, cf$r)
      d$id <- cf$id; d$block <- cf$block
      d
    }))

  # ---- Resolve color scheme -----------------------------------------------
  use_color <- !isTRUE(bw) || !is.null(palette)
  if (is.null(palette)) {
    if (use_color) {
      pal_colors <- c("#FFE082","#FFAB91","#B0BEC5","#F4A8A8",
                      "#A5D6A7","#90CAF9","#CE93D8","#FFCC80","#80CBC4")
      palette <- stats::setNames(pal_colors[seq_along(factors)], factors)
    } else {
      palette <- stats::setNames(rep("white", length(factors)), factors)
    }
  }
  arrow_color  <- if (use_color) NA else "grey15"   # NA -> use aes(color)
  border_color <- if (use_color) "grey15" else "grey15"

  # ---- Edges (factor -> item, fan-out from circle border) ----------------
  # Each arrow originates at the point on the factor circle that lies on
  # the line from the factor centre to the item, and ends at the right
  # edge of the item rectangle. This gives the classic CFA "fan" look.
  edges <- merge(loadings,
    data.frame(lhs = factor_nodes$id,
               x_factor = factor_nodes$x,
               y_factor = factor_nodes$y,
               r_factor = factor_nodes$r,
               stringsAsFactors = FALSE),
    by = "lhs", all.x = TRUE)
  edges <- merge(edges,
    data.frame(rhs = item_nodes$id,
               x_item = item_nodes$x,
               y_item = item_nodes$y,
               xmax_item = item_nodes$xmax,
               stringsAsFactors = FALSE),
    by = "rhs", all.x = TRUE)
  edges$abs_b      <- abs(edges$est.std)
  fmt              <- paste0("%.", loading_label_decimals, "f")
  edges$label      <- sprintf(fmt, edges$est.std)
  edges$block_from <- edges$lhs

  # Direction unit vector from factor centre to item centre
  edges$vx <- edges$x_item - edges$x_factor
  edges$vy <- edges$y_item - edges$y_factor
  edges$L  <- sqrt(edges$vx^2 + edges$vy^2)
  edges$ux <- edges$vx / pmax(edges$L, 1e-9)
  edges$uy <- edges$vy / pmax(edges$L, 1e-9)
  # Origin: point on factor circle along that direction
  edges$x_from <- edges$x_factor + edges$ux * edges$r_factor
  edges$y_from <- edges$y_factor + edges$uy * edges$r_factor
  # Arrowhead lands at the right edge of the item box (on the same y)
  edges$x_to <- edges$xmax_item
  edges$y_to <- edges$y_item

  # Label at fraction t along the arrow (counted from factor side)
  t_lab <- max(0.05, min(0.95, loading_label_position))
  edges$lab_x <- edges$x_from + (edges$x_to - edges$x_from) * t_lab
  edges$lab_y <- edges$y_from + (edges$y_to - edges$y_from) * t_lab

  # ---- Latent correlations (curves on the right side of the factors) -----
  cor_segments <- NULL
  if (correlate_factors) {
    cors <- est[est$op == "~~" & est$lhs != est$rhs &
                est$lhs %in% factors & est$rhs %in% factors, ]
    if (nrow(cors) > 0L) {
      cors <- merge(cors,
        data.frame(lhs = factor_nodes$id,
                   x_from = factor_nodes$x + factor_nodes$r,
                   y_from = factor_nodes$y, stringsAsFactors = FALSE),
        by = "lhs", all.x = TRUE)
      cors <- merge(cors,
        data.frame(rhs = factor_nodes$id,
                   x_to = factor_nodes$x + factor_nodes$r,
                   y_to = factor_nodes$y, stringsAsFactors = FALSE),
        by = "rhs", all.x = TRUE)
      cors$cor_label <- sprintf(fmt, cors$est.std)
      ord <- factor_nodes$id
      cors$lvl_dist <- abs(match(cors$lhs, ord) - match(cors$rhs, ord))
      cors$curv <- correlation_curvature *
        (1 + 0.45 * (cors$lvl_dist - 1)) *
        ifelse(seq_len(nrow(cors)) %% 2L == 0L, -1, 1)
      cor_segments <- cors
    }
  }

  # ---- Subtitle (auto fit + N) -------------------------------------------
  if (is.null(subtitle)) {
    fi <- tryCatch(lavaan::fitMeasures(fit, fit_indices),
                   error = function(e) NULL)
    sub_pieces <- character(0)
    if (!is.null(fi) && length(fi) > 0L) {
      labs <- toupper(sub("\\.[a-z]+$", "", names(fi)))
      sub_pieces <- c(sub_pieces,
        paste(sprintf("%s = %.3f", labs, as.numeric(fi)),
              collapse = " | "))
    }
    if (show_n) {
      ntot <- tryCatch(lavaan::lavInspect(fit, "ntotal"),
                       error = function(e) NA_integer_)
      if (!is.na(ntot)) sub_pieces <- c(sub_pieces, sprintf("N = %d", ntot))
    }
    subtitle <- paste(sub_pieces, collapse = "\n")
  }

  # ---- Build ggplot ------------------------------------------------------
  p <- ggplot2::ggplot()

  # Latent correlations: draw each as a separate curve (curvature is fixed
  # per geom_curve call, so we loop).
  if (!is.null(cor_segments) && nrow(cor_segments) > 0L) {
    for (i in seq_len(nrow(cor_segments))) {
      p <- p +
        ggplot2::geom_curve(
          data = cor_segments[i, , drop = FALSE],
          ggplot2::aes(x = .data$x_from, xend = .data$x_to,
                       y = .data$y_from, yend = .data$y_to),
          curvature = cor_segments$curv[i],
          linetype  = "dashed",
          color     = "grey25",
          alpha     = 0.7,
          linewidth = 0.4,
          inherit.aes = FALSE)
    }
    if (show_correlation_labels) {
      cor_segments$lab_x <- factor_x + factor_radius + 1.0 +
                            1.0 * (cor_segments$lvl_dist - 1)
      cor_segments$lab_y <- (cor_segments$y_from + cor_segments$y_to) / 2
      p <- p +
        ggplot2::geom_label(
          data = cor_segments,
          ggplot2::aes(x = .data$lab_x, y = .data$lab_y,
                       label = .data$cor_label),
          color = "grey15", size = edge_label_size, fontface = "italic",
          label.padding = ggplot2::unit(0.10, "lines"),
          label.size = 0,
          fill = scales::alpha("white", 0.95),
          inherit.aes = FALSE)
    }
  }

  # Loading arrows (fan-out from circle border)
  if (use_color) {
    p <- p +
      ggplot2::geom_segment(
        data = edges,
        ggplot2::aes(x = .data$x_from, xend = .data$x_to,
                     y = .data$y_from, yend = .data$y_to,
                     linewidth = .data$abs_b, color = .data$block_from),
        arrow = ggplot2::arrow(length = ggplot2::unit(0.20, "cm"),
                                type = "closed"),
        lineend = "round")
  } else {
    p <- p +
      ggplot2::geom_segment(
        data = edges,
        ggplot2::aes(x = .data$x_from, xend = .data$x_to,
                     y = .data$y_from, yend = .data$y_to,
                     linewidth = .data$abs_b),
        color = "grey15",
        arrow = ggplot2::arrow(length = ggplot2::unit(0.20, "cm"),
                                type = "closed"),
        lineend = "round")
  }

  # Loading labels
  if (show_loading_labels) {
    p <- p +
      ggplot2::geom_label(
        data = edges,
        ggplot2::aes(x = .data$lab_x, y = .data$lab_y, label = .data$label),
        color = "grey15", size = edge_label_size, fontface = "bold",
        label.padding = ggplot2::unit(0.12, "lines"),
        label.size = 0,
        fill = scales::alpha("white", 0.97))
  }

  # Item rectangles (sharp corners — observed-variable convention)
  if (use_color) {
    p <- p +
      ggplot2::geom_rect(
        data = item_nodes,
        ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                     ymin = .data$ymin, ymax = .data$ymax,
                     fill = .data$block),
        color = border_color, linewidth = 0.4)
  } else {
    p <- p +
      ggplot2::geom_rect(
        data = item_nodes,
        ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                     ymin = .data$ymin, ymax = .data$ymax),
        fill = "white", color = border_color, linewidth = 0.4)
  }
  p <- p + ggplot2::geom_text(
    data = item_nodes,
    ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
    color = "grey15", size = node_label_size)

  # Factor circles — latent-variable convention
  if (use_color) {
    p <- p +
      ggplot2::geom_polygon(
        data = factor_polys,
        ggplot2::aes(x = .data$x, y = .data$y,
                     group = .data$id, fill = .data$block),
        color = border_color, linewidth = 0.5)
  } else {
    p <- p +
      ggplot2::geom_polygon(
        data = factor_polys,
        ggplot2::aes(x = .data$x, y = .data$y, group = .data$id),
        fill = "white", color = border_color, linewidth = 0.5)
  }
  p <- p + ggplot2::geom_text(
    data = factor_nodes,
    ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
    color = "grey15", size = node_label_size + 0.8, fontface = "bold")

  # Scales (only when colour is in play)
  if (use_color) {
    p <- p +
      ggplot2::scale_color_manual(values = palette, name = "Factor") +
      ggplot2::scale_fill_manual(values  = palette, name = "Factor")
  }
  p <- p +
    ggplot2::scale_linewidth_continuous(
      range  = edge_width_range,
      name   = expression("|" * lambda * "|"),
      breaks = c(0.30, 0.50, 0.70, 0.90))

  if (use_color) {
    p <- p + ggplot2::guides(
      color     = ggplot2::guide_legend(order = 1,
                    override.aes = list(linewidth = 1.6)),
      linewidth = ggplot2::guide_legend(order = 2),
      fill      = "none")
  } else {
    p <- p + ggplot2::guides(linewidth = ggplot2::guide_legend(order = 1),
                              fill = "none", color = "none")
  }

  p <- p +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption) +
    # coord_fixed keeps the factor circles round.
    ggplot2::coord_fixed(
      ratio = 1,
      xlim  = c(item_x - item_half_width - 0.5,
                 factor_x + factor_radius + 4.5),
      ylim  = c(min(item_nodes$ymin, factor_nodes$y - factor_nodes$r) - 1,
                 max(item_nodes$ymax, factor_nodes$y + factor_nodes$r) + 1),
      clip  = "off") +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = 14,
                                              hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5,
                                              color = "grey25",
                                              lineheight = 1.15,
                                              margin = ggplot2::margin(b = 12)),
      plot.caption  = ggplot2::element_text(size = 8.5, hjust = 0.5,
                                              color = "grey35",
                                              lineheight = 1.2,
                                              margin = ggplot2::margin(t = 10)),
      legend.position  = if (use_color) "bottom" else "bottom",
      legend.box       = "horizontal",
      legend.spacing.x = ggplot2::unit(0.5, "cm"),
      plot.margin      = ggplot2::margin(15, 30, 15, 30))

  p
}
