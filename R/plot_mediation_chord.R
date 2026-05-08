#' @title Chord diagram of distal-predictor by mediator associations
#' @description Visualización circular de cuerdas (chord diagram) que muestra
#'   las asociaciones bivariadas entre los predictores distales y los
#'   indicadores de los mediadores en un modelo de mediación múltiple paralela.
#'   Replica el estilo de la Figura 1 de Wang et al. (2025, BMC Medicine):
#'   cada arco perimetral representa una variable agrupada por bloque
#'   conceptual (color), y cada cuerda interna representa una asociación con
#'   grosor proporcional a |\eqn{\beta}| y color por signo.
#'
#' @param fit Objeto \code{lavaan} ajustado.
#' @param predictors Vector de IDs de los predictores distales (X) tal como
#'   aparecen en \code{rhs} de las regresiones \code{outcome ~ X}.
#' @param mediators Vector de IDs de los mediadores (M) tal como aparecen en
#'   \code{lhs} de las regresiones \code{M ~ X}.
#' @param outcome ID del outcome opcional (para incluir cuerdas X→Y).
#' @param node_groups Vector \emph{named} que mapea cada ID a su bloque
#'   conceptual (por ejemplo, \code{c(sueno_num = "Hábito", DASS_dep = "DASS")}).
#'   Si \code{NULL}, se usa "X" para predictores y "M" para mediadores.
#' @param node_labels Vector \emph{named} de etiquetas para mostrar en el arco.
#'   Si \code{NULL}, se usan los IDs.
#' @param palette Vector \emph{named} de colores por bloque conceptual.
#' @param sig_threshold Umbral de significancia para diferenciar cuerdas
#'   (default \code{0.05}).
#' @param show_n_s Lógico. Mostrar también las asociaciones no significativas
#'   con transparencia (default \code{TRUE}).
#' @param sig_alpha Transparencia para cuerdas n.s. (default \code{0.15}).
#' @param positive_color,negative_color Colores para asociaciones positivas y
#'   negativas. Si \code{NULL}, se hereda el color del bloque del predictor.
#' @param chord_width_range Rango de grosor de las cuerdas (default
#'   \code{c(0.5, 6)}).
#' @param gap_degree Espacio en grados entre arcos (default \code{2}).
#' @param big_gap Espacio en grados entre bloques conceptuales (default
#'   \code{8}).
#' @param label_cex Tamaño de las etiquetas perimetrales (default \code{0.85}).
#' @param title Título del gráfico.
#'
#' @return Invoca \code{circlize::chordDiagram()} y devuelve invisiblemente la
#'   matriz de asociaciones usada para el dibujo.
#'
#' @details
#' La función calcula los \eqn{\beta} estandarizados de
#' \code{lavaan::standardizedSolution()} para cada par (predictor, mediador) y
#' construye una matriz adyacencia que se pasa a
#' \code{circlize::chordDiagram()}. Requiere el paquete \code{circlize}.
#'
#' @references
#' Wang, X., Cao, Z., Yin, S., Duan, T., Sun, T., & Xu, C. (2025). Childhood
#' maltreatment and depression: Mediating role of lifestyle factors,
#' personality traits, adult traumas, and social connections among
#' middle-aged and elderly participants. \emph{BMC Medicine, 23}, 319.
#' \doi{10.1186/s12916-025-04147-2}
#'
#' Gu, Z., Gu, L., Eils, R., Schlesner, M., & Brors, B. (2014). circlize
#' implements and enhances circular visualization in R. \emph{Bioinformatics,
#' 30}(19), 2811–2812. \doi{10.1093/bioinformatics/btu393}
#'
#' @examples
#' \dontrun{
#' library(lavaan); library(PsyMetricTools); library(circlize)
#'
#' plot_mediation_chord(
#'   fit,
#'   predictors = c("X1","X2","X3"),
#'   mediators  = c("M1","M2"),
#'   node_groups = c(X1 = "Block A", X2 = "Block A", X3 = "Block B",
#'                   M1 = "Mediator", M2 = "Mediator"),
#'   palette = c("Block A" = "#FFE082", "Block B" = "#B0BEC5",
#'               "Mediator" = "#F4A8A8")
#' )
#' }
#' @importFrom lavaan standardizedSolution
#' @export
plot_mediation_chord <- function(fit,
                                 predictors,
                                 mediators,
                                 outcome = NULL,
                                 node_groups = NULL,
                                 node_labels = NULL,
                                 palette = NULL,
                                 sig_threshold = 0.05,
                                 show_n_s = TRUE,
                                 sig_alpha = 0.15,
                                 positive_color = NULL,
                                 negative_color = NULL,
                                 chord_width_range = c(0.5, 6),
                                 gap_degree = 2,
                                 big_gap = 8,
                                 label_cex = 0.85,
                                 title = NULL) {

  if (!requireNamespace("circlize", quietly = TRUE))
    stop("Requiere el paquete 'circlize'. Instalar con: install.packages('circlize')",
         call. = FALSE)
  if (!inherits(fit, "lavaan"))
    stop("`fit` debe ser un objeto lavaan ajustado.", call. = FALSE)

  est <- lavaan::standardizedSolution(fit)

  all_nodes <- c(predictors, mediators)
  if (!is.null(outcome)) all_nodes <- c(all_nodes, outcome)

  if (is.null(node_labels))
    node_labels <- setNames(all_nodes, all_nodes)
  if (is.null(node_groups))
    node_groups <- setNames(c(rep("Predictor", length(predictors)),
                              rep("Mediator", length(mediators)),
                              if (!is.null(outcome)) "Outcome"),
                            all_nodes)
  if (is.null(palette)) {
    blocks <- unique(node_groups)
    default_pal <- c("#FFE082","#FFAB91","#B0BEC5","#F4A8A8","#A5D6A7","#90CAF9")
    palette <- setNames(default_pal[seq_along(blocks)], blocks)
  }

  # ---- Construir matriz de asociaciones X-M y opcionalmente X-Y ----------
  pairs <- expand.grid(from = predictors, to = mediators, stringsAsFactors = FALSE)
  if (!is.null(outcome)) {
    extra <- expand.grid(from = predictors, to = outcome, stringsAsFactors = FALSE)
    pairs <- rbind(pairs, extra)
    extra2 <- expand.grid(from = mediators, to = outcome, stringsAsFactors = FALSE)
    pairs <- rbind(pairs, extra2)
  }
  get_b <- function(dv, iv) {
    f <- est[est$op == "~" & est$lhs == dv & est$rhs == iv, ]
    if (nrow(f) == 0) return(c(NA_real_, NA_real_))
    c(f$est.std[1], f$pvalue[1])
  }
  bp <- t(mapply(get_b, pairs$to, pairs$from))
  pairs$beta <- bp[, 1]; pairs$pvalue <- bp[, 2]
  pairs <- pairs[!is.na(pairs$beta), ]

  # ---- Filtro por significancia ------------------------------------------
  if (!show_n_s) pairs <- pairs[pairs$pvalue < sig_threshold, ]

  # ---- Orden circular: predictores agrupados, mediadores y outcome -------
  ordered_nodes <- c(predictors, mediators,
                     if (!is.null(outcome)) outcome else character(0))
  group_per_node <- node_groups[ordered_nodes]
  group_levels   <- unique(group_per_node)

  # ---- Color por bloque para los arcos ------------------------------------
  grid_col <- setNames(palette[node_groups[ordered_nodes]], ordered_nodes)

  # ---- Color de las cuerdas (positivo / negativo) ------------------------
  pos <- if (is.null(positive_color)) "#C0392B" else positive_color
  neg <- if (is.null(negative_color)) "#2980B9" else negative_color
  link_col <- ifelse(pairs$beta > 0, pos, neg)
  alpha_v  <- ifelse(pairs$pvalue < sig_threshold, "FF",
                     sprintf("%02X", as.integer(sig_alpha * 255)))
  link_col <- paste0(link_col, alpha_v)

  # ---- Grosor proporcional a |β| ----------------------------------------
  abs_b <- abs(pairs$beta)
  if (max(abs_b) > 0) {
    rng <- chord_width_range
    chord_w <- rng[1] + (abs_b - min(abs_b)) /
               (max(abs_b) - min(abs_b) + 1e-9) * (rng[2] - rng[1])
  } else chord_w <- rep(chord_width_range[1], nrow(pairs))

  # ---- Construir gaps entre bloques --------------------------------------
  big_gap <- big_gap; small_gap <- gap_degree
  gap_after <- numeric(length(ordered_nodes))
  for (i in seq_along(ordered_nodes)) {
    is_last_of_block <- i == length(ordered_nodes) ||
                        group_per_node[i] != group_per_node[i + 1]
    gap_after[i] <- ifelse(is_last_of_block, big_gap, small_gap)
  }

  # ---- Formato de etiquetas ----------------------------------------------
  display_labels <- node_labels[ordered_nodes]

  circlize::circos.clear()
  circlize::circos.par(start.degree = 90, gap.degree = gap_after,
                       points.overflow.warning = FALSE)

  # ---- Adyacencia para chordDiagramFromDataFrame -------------------------
  link_df <- data.frame(from = pairs$from, to = pairs$to,
                        value = chord_w,
                        stringsAsFactors = FALSE)

  circlize::chordDiagram(
    link_df,
    grid.col = grid_col,
    col = link_col,
    transparency = 0,
    annotationTrack = "grid",
    preAllocateTracks = list(track.height = 0.08),
    order = ordered_nodes,
    directional = 0
  )

  # ---- Etiquetas perimetrales --------------------------------------------
  for (s in ordered_nodes) {
    circlize::circos.text(
      circlize::CELL_META$xcenter, circlize::CELL_META$ylim[1] + 0.5,
      display_labels[[s]],
      facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5),
      cex = label_cex, sector.index = s,
      track.index = circlize::get.cell.meta.data("track.index", sector.index = s)
    )
  }

  if (!is.null(title))
    title(main = title, cex.main = 1.1)

  circlize::circos.clear()

  invisible(pairs)
}
