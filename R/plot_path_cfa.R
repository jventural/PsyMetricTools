#' @title Path diagram for a CFA model (ggplot2)
#' @description Genera un diagrama de senderos para un modelo CFA ajustado
#'   con \code{lavaan::cfa()}. Los \strong{items} se colocan en la columna
#'   izquierda agrupados por factor (con un pequeño espacio vertical entre
#'   grupos), y los \strong{factores} en la columna derecha, alineados al
#'   centroide vertical de sus items. Las flechas factor \eqn{\to} item
#'   tienen grosor proporcional a \eqn{|\lambda|} (carga estandarizada) y
#'   color por factor de origen. Las correlaciones latentes \eqn{\phi}
#'   entre factores se dibujan como curvas punteadas con su valor.
#'
#'   El estilo es análogo a \code{\link{plot_path_mediation}} pero adaptado
#'   a la sintaxis CFA (\code{op == "=~"}). Diseñado como reemplazo legible
#'   de \code{semPlot} cuando hay 10+ items.
#'
#' @param fit Objeto \code{lavaan} ajustado con \code{lavaan::cfa()} (o
#'   \code{lavaan::sem()} con bloque de medida).
#' @param loading_threshold Numeric. Cargas con \eqn{|\lambda| <} threshold
#'   se omiten del diagrama (default 0.30).
#' @param show_loading_labels Logical. Imprimir el valor de \eqn{\lambda}
#'   sobre cada flecha (default \code{TRUE}).
#' @param loading_label_decimals Integer. Decimales para etiquetas
#'   (default 2).
#' @param correlate_factors Logical. Dibujar correlaciones latentes
#'   \eqn{\phi} entre factores como curvas punteadas (default \code{TRUE}).
#' @param show_correlation_labels Logical. Mostrar el valor de \eqn{\phi}
#'   junto a cada curva (default \code{TRUE}).
#' @param correlation_curvature Numeric. Curvatura de las correlaciones
#'   (default 0.45).
#' @param palette Vector con nombres iguales a los factores. Si \code{NULL}
#'   se usa una paleta pastel por defecto.
#' @param edge_width_range Vector de longitud 2 con el rango de grosor
#'   de las flechas (default \code{c(0.3, 2.4)}).
#' @param node_label_size,edge_label_size Tamaños de fuente para los
#'   textos (defaults 3.0 y 2.6).
#' @param item_x,factor_x Coordenadas X de las dos columnas (defaults 0
#'   y 6).
#' @param item_spacing Numeric. Distancia vertical entre items
#'   consecutivos del mismo factor (default 1).
#' @param group_gap Numeric. Espacio vertical adicional entre grupos de
#'   items de distintos factores (default 1.5).
#' @param fit_indices Vector de nombres de \code{lavaan::fitMeasures()} a
#'   incluir en el subtítulo (default \code{c("cfi.scaled","tli.scaled",
#'   "rmsea.scaled","srmr")}).
#' @param show_n Logical. Anexar \eqn{N} al subtítulo (default \code{TRUE}).
#' @param title,subtitle,caption Cadenas para los componentes
#'   correspondientes. Si \code{subtitle = NULL} se construye automática-
#'   mente con \code{fit_indices} y \eqn{N}.
#' @param factor_label_map Lista nombrada para renombrar factores en el
#'   diagrama (e.g. \code{list(F1 = "Cognitive", F2 = "Affective")}).
#'   Default \code{NULL}.
#'
#' @return Un objeto \code{ggplot} listo para imprimir, guardar con
#'   \code{ggsave()} o componer con \code{patchwork}.
#'
#' @details
#' Toma los coeficientes estandarizados de \code{lavaan::standardizedSolution()}
#' y filtra:
#' \itemize{
#'   \item \code{op == "=~"} para construir las flechas factor \eqn{\to} item.
#'   \item \code{op == "~~"} con \code{lhs != rhs} entre factores latentes
#'         para las correlaciones \eqn{\phi}.
#' }
#' Las varianzas residuales se omiten para evitar saturación visual; si las
#' necesitas, usa \code{semPlot::semPaths(..., what = "std")}.
#'
#' @references
#' Brown, T. A. (2015). \emph{Confirmatory factor analysis for applied
#' research} (2nd ed.). Guilford Press.
#'
#' @examples
#' \dontrun{
#' library(lavaan)
#' library(PsyMetricTools)
#'
#' # Modelo CFA de 3 factores
#' mod <- '
#'   F1 =~ ITEM1 + ITEM2 + ITEM3 + ITEM4 + ITEM5
#'   F2 =~ ITEM6 + ITEM7 + ITEM8 + ITEM9 + ITEM10
#'   F3 =~ ITEM11 + ITEM12 + ITEM13 + ITEM14 + ITEM15
#' '
#' fit <- cfa(mod, data = my_data, ordered = paste0("ITEM", 1:15),
#'            estimator = "WLSMV", std.lv = TRUE)
#'
#' plot_path_cfa(fit, title = "Estructura factorial final")
#'
#' # Renombrar factores y elegir paleta
#' plot_path_cfa(fit,
#'   factor_label_map = list(F1 = "Cognitive", F2 = "Affective", F3 = "Behavioral"),
#'   palette = c(F1 = "#90CAF9", F2 = "#F4A8A8", F3 = "#A5D6A7"))
#' }
#' @importFrom ggplot2 ggplot aes geom_segment geom_curve geom_label arrow
#'   unit scale_color_manual scale_fill_manual scale_linewidth_continuous
#'   coord_cartesian theme_void theme element_text margin labs guides
#'   guide_legend
#' @importFrom scales alpha
#' @importFrom lavaan standardizedSolution fitMeasures lavInspect
#' @importFrom stats setNames
#' @export
plot_path_cfa <- function(fit,
                          loading_threshold       = 0.30,
                          show_loading_labels     = TRUE,
                          loading_label_decimals  = 2,
                          correlate_factors       = TRUE,
                          show_correlation_labels = TRUE,
                          correlation_curvature   = 0.45,
                          palette                 = NULL,
                          edge_width_range        = c(0.3, 2.4),
                          node_label_size         = 3.0,
                          edge_label_size         = 2.6,
                          item_x                  = 0,
                          factor_x                = 6,
                          item_spacing            = 1,
                          group_gap               = 1.5,
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
    stop(sprintf("No loadings with |lambda| >= %.2f. Lower loading_threshold.",
                  loading_threshold), call. = FALSE)

  factors <- unique(loadings$lhs)

  # ---- Build node coordinates ---------------------------------------------
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
      layer = 1L,
      x     = item_x,
      y     = ys,
      stringsAsFactors = FALSE)
    current_y <- min(ys) - group_gap
  }
  item_nodes <- do.call(rbind, item_nodes_list)

  # Factor nodes (right column), centered on their items
  factor_label_for <- function(f) {
    if (is.null(factor_label_map) || is.null(factor_label_map[[f]])) f
    else as.character(factor_label_map[[f]])
  }
  factor_nodes <- do.call(rbind, lapply(factors, function(f) {
    its <- item_nodes[item_nodes$block == f, ]
    data.frame(
      id    = f,
      label = factor_label_for(f),
      block = f,
      layer = 2L,
      x     = factor_x,
      y     = mean(its$y),
      stringsAsFactors = FALSE)
  }))
  nodes <- rbind(item_nodes, factor_nodes)

  # ---- Default palette -----------------------------------------------------
  if (is.null(palette)) {
    pal_colors <- c("#FFE082","#FFAB91","#B0BEC5","#F4A8A8",
                    "#A5D6A7","#90CAF9","#CE93D8","#FFCC80","#80CBC4")
    palette <- stats::setNames(pal_colors[seq_along(factors)], factors)
  }

  # ---- Edges (factor -> item) ---------------------------------------------
  edges <- merge(loadings,
    data.frame(lhs = factor_nodes$id, x_from = factor_nodes$x,
               y_from = factor_nodes$y, stringsAsFactors = FALSE),
    by = "lhs", all.x = TRUE)
  edges <- merge(edges,
    data.frame(rhs = item_nodes$id, x_to = item_nodes$x,
               y_to = item_nodes$y, stringsAsFactors = FALSE),
    by = "rhs", all.x = TRUE)
  edges$abs_b <- abs(edges$est.std)
  fmt <- paste0("%.", loading_label_decimals, "f")
  edges$label      <- sprintf(fmt, edges$est.std)
  edges$block_from <- edges$lhs

  # ---- Shrink arrows so they don't collide with node labels ---------------
  shrink_dx <- 0.9
  shrink_dy <- 0.4
  shrink_one <- function(x1, y1, x2, y2) {
    vx <- x2 - x1; vy <- y2 - y1; L <- sqrt(vx^2 + vy^2)
    if (is.na(L) || L == 0) return(c(x1, y1, x2, y2))
    ux <- vx / L; uy <- vy / L
    c(x1 + ux * shrink_dx, y1 + uy * shrink_dy,
      x2 - ux * shrink_dx, y2 - uy * shrink_dy)
  }
  exy <- t(mapply(shrink_one, edges$x_from, edges$y_from,
                              edges$x_to,   edges$y_to))
  edges$x_from2 <- exy[, 1]; edges$y_from2 <- exy[, 2]
  edges$x_to2   <- exy[, 3]; edges$y_to2   <- exy[, 4]

  # Edge label position (slight perpendicular offset)
  edges$mx <- (edges$x_from + edges$x_to) / 2
  edges$my <- (edges$y_from + edges$y_to) / 2
  edges$dx <- edges$x_to - edges$x_from
  edges$dy <- edges$y_to - edges$y_from
  edges$L  <- sqrt(edges$dx^2 + edges$dy^2)
  edges$lab_x <- edges$mx + (-edges$dy / edges$L) * 0.18
  edges$lab_y <- edges$my + ( edges$dx / edges$L) * 0.18

  # ---- Latent correlations between factors --------------------------------
  cor_segments <- NULL
  if (correlate_factors) {
    cors <- est[est$op == "~~" & est$lhs != est$rhs &
                est$lhs %in% factors & est$rhs %in% factors, ]
    if (nrow(cors) > 0L) {
      cors <- merge(cors,
        data.frame(lhs = factor_nodes$id, x_from = factor_nodes$x,
                   y_from = factor_nodes$y, stringsAsFactors = FALSE),
        by = "lhs", all.x = TRUE)
      cors <- merge(cors,
        data.frame(rhs = factor_nodes$id, x_to = factor_nodes$x,
                   y_to = factor_nodes$y, stringsAsFactors = FALSE),
        by = "rhs", all.x = TRUE)
      cors$cor_label <- sprintf(fmt, cors$est.std)
      cor_segments <- cors
    }
  }

  # ---- Auto subtitle (fit indices + N) ------------------------------------
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

  # ---- Plot ----------------------------------------------------------------
  p <- ggplot2::ggplot()

  # Latent correlations as dashed curves
  if (!is.null(cor_segments) && nrow(cor_segments) > 0L) {
    p <- p +
      ggplot2::geom_curve(
        data = cor_segments,
        ggplot2::aes(x = .data$x_from, xend = .data$x_to,
                     y = .data$y_from, yend = .data$y_to),
        curvature = correlation_curvature,
        linetype  = "dashed",
        color     = "grey40",
        alpha     = 0.6,
        linewidth = 0.4)

    if (show_correlation_labels) {
      cor_segments$mid_x <- factor_x + 1.0
      cor_segments$mid_y <- (cor_segments$y_from + cor_segments$y_to) / 2
      p <- p +
        ggplot2::geom_label(
          data = cor_segments,
          ggplot2::aes(x = .data$mid_x, y = .data$mid_y,
                       label = .data$cor_label),
          color = "grey25", size = edge_label_size, fontface = "italic",
          label.padding = ggplot2::unit(0.10, "lines"),
          label.size = 0,
          fill = scales::alpha("white", 0.95))
    }
  }

  # Loadings (factor -> item)
  p <- p +
    ggplot2::geom_segment(
      data = edges,
      ggplot2::aes(x = .data$x_from2, xend = .data$x_to2,
                   y = .data$y_from2, yend = .data$y_to2,
                   linewidth = .data$abs_b, color = .data$block_from),
      arrow = ggplot2::arrow(length = ggplot2::unit(0.18, "cm"),
                             type = "closed"),
      lineend = "round", alpha = 0.85)

  if (show_loading_labels) {
    p <- p +
      ggplot2::geom_label(
        data = edges,
        ggplot2::aes(x = .data$lab_x, y = .data$lab_y, label = .data$label),
        color = "grey20", size = edge_label_size, fontface = "bold",
        label.padding = ggplot2::unit(0.10, "lines"),
        label.size = 0,
        fill = scales::alpha("white", 0.95))
  }

  # Item nodes (smaller, plain text)
  p <- p + ggplot2::geom_label(
    data = item_nodes,
    ggplot2::aes(x = .data$x, y = .data$y, label = .data$label,
                 fill = .data$block),
    color = "grey15", size = node_label_size, fontface = "plain",
    label.padding = ggplot2::unit(0.30, "lines"),
    label.r       = ggplot2::unit(0.12, "lines"),
    label.size    = 0)

  # Factor nodes (bigger, bold)
  p <- p + ggplot2::geom_label(
    data = factor_nodes,
    ggplot2::aes(x = .data$x, y = .data$y, label = .data$label,
                 fill = .data$block),
    color = "grey15", size = node_label_size + 0.6, fontface = "bold",
    label.padding = ggplot2::unit(0.45, "lines"),
    label.r       = ggplot2::unit(0.55, "lines"),
    label.size    = 0)

  p <- p +
    ggplot2::scale_color_manual(values = palette, name = "Factor") +
    ggplot2::scale_fill_manual(values  = palette, name = "Factor") +
    ggplot2::scale_linewidth_continuous(
      range  = edge_width_range,
      name   = expression("|" * lambda * "|"),
      breaks = c(0.30, 0.50, 0.70, 0.90)) +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption) +
    ggplot2::guides(
      color     = ggplot2::guide_legend(order = 1,
                    override.aes = list(linewidth = 1.8)),
      linewidth = ggplot2::guide_legend(order = 2),
      fill      = "none") +
    ggplot2::coord_cartesian(
      xlim = c(item_x - 1.5, factor_x + 2.5),
      ylim = c(min(nodes$y) - 1, max(nodes$y) + 1),
      clip = "off") +
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
      legend.position  = "bottom",
      legend.box       = "horizontal",
      legend.spacing.x = ggplot2::unit(0.5, "cm"),
      plot.margin      = ggplot2::margin(15, 30, 15, 30))

  p
}
