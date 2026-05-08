#' @title Donut panel of mediation proportions per parallel mediator
#' @description Genera un panel de \emph{donuts} (gráficos circulares con
#'   centro hueco) que cuantifica visualmente la proporción del efecto total
#'   mediado por cada uno de los mediadores paralelos para un predictor distal
#'   determinado. Cada donut muestra el porcentaje de la asociación X→Y
#'   atribuible al mediador en cuestión, replicando el estilo de la Figura 2
#'   superior de Wang et al. (2025, BMC Medicine).
#'
#' @param fit Objeto \code{lavaan} ajustado.
#' @param predictor Stem del predictor distal (tal como aparece tras los
#'   prefijos \code{ind_} y \code{tot_} en las definiciones \code{:=} del
#'   modelo). Debe coincidir con uno de los predictores incluidos en el
#'   modelo de senderos.
#' @param mediators Vector \emph{named} con los IDs de los mediadores donde el
#'   nombre es la etiqueta a mostrar y el valor es el ID en \code{lavaan}.
#'   Por ejemplo, \code{c("Depresión" = "DASS_dep", "Ansiedad" = "DASS_anx")}.
#' @param outcome ID del outcome (default \code{"suic"}).
#' @param predictor_regression Nombre del predictor en la regresión
#'   \code{outcome ~ X} (si difiere del stem). Por ejemplo, si el stem es
#'   \code{"sueno"} y la regresión usa \code{sueno_num}, especificar
#'   \code{"sueno_num"}.
#' @param indirect_prefix Prefijo de las definiciones de efectos indirectos
#'   por mediador (default \code{"ind_"}).
#' @param mediator_colors Vector \emph{named} de colores para cada mediador.
#'   Si \code{NULL}, se usa una paleta pastel por defecto.
#' @param remaining_color Color para la porción no mediada (default
#'   \code{"grey85"}).
#' @param show_total Lógico. Si \code{TRUE}, añade un panel adicional con la
#'   proporción mediada total (suma de todos los mediadores; default
#'   \code{TRUE}).
#' @param ncol Número de columnas en el panel (default es la longitud de
#'   \code{mediators} + opcionalmente uno para el total).
#' @param title Título del panel.
#' @param subtitle Subtítulo del panel.
#'
#' @return Un objeto \code{patchwork} (composición de \code{ggplot}s) listo
#'   para imprimir o guardar con \code{ggsave()}.
#'
#' @details
#' La función calcula la \emph{proporción mediada} por cada mediador como
#' \deqn{P_j = \frac{a_j \times b_j}{|a_1 b_1| + |a_2 b_2| + ... + |a_m b_m|}}
#' donde \eqn{a_j} es el coeficiente \code{M_j ~ X} y \eqn{b_j} es el
#' coeficiente \code{Y ~ M_j}, ambos estandarizados. Cada donut representa
#' el valor absoluto de \eqn{P_j} con la fracción restante en gris,
#' acompañado por el porcentaje en el centro y el signo del efecto en la
#' parte inferior.
#'
#' Requiere los paquetes \code{ggplot2} y \code{patchwork}.
#'
#' @references
#' Wang, X., Cao, Z., Yin, S., Duan, T., Sun, T., & Xu, C. (2025). Childhood
#' maltreatment and depression: Mediating role of lifestyle factors,
#' personality traits, adult traumas, and social connections among
#' middle-aged and elderly participants. \emph{BMC Medicine, 23}, 319.
#' \doi{10.1186/s12916-025-04147-2}
#'
#' @examples
#' \dontrun{
#' library(lavaan); library(PsyMetricTools); library(patchwork)
#'
#' plot_mediation_donuts(
#'   fit,
#'   predictor = "sueno",
#'   mediators = c("Depresión" = "DASS_dep", "Ansiedad" = "DASS_anx",
#'                 "Estrés" = "DASS_str", "Bienestar" = "bien"),
#'   outcome = "suic",
#'   predictor_regression = "sueno_num",
#'   title = "Mediación de la calidad del sueño sobre la conducta suicida"
#' )
#' }
#' @importFrom ggplot2 ggplot aes geom_rect coord_polar xlim
#'   theme_void scale_fill_manual annotate labs
#' @importFrom lavaan standardizedSolution
#' @export
plot_mediation_donuts <- function(fit,
                                  predictor,
                                  mediators,
                                  outcome = "suic",
                                  predictor_regression = NULL,
                                  indirect_prefix = "ind_",
                                  mediator_colors = NULL,
                                  remaining_color = "grey85",
                                  show_total = TRUE,
                                  ncol = NULL,
                                  title = NULL,
                                  subtitle = NULL) {

  if (!requireNamespace("patchwork", quietly = TRUE))
    stop("Requiere el paquete 'patchwork'. Instalar con: install.packages('patchwork')",
         call. = FALSE)
  if (!inherits(fit, "lavaan"))
    stop("`fit` debe ser un objeto lavaan ajustado.", call. = FALSE)

  est <- lavaan::standardizedSolution(fit)
  pred_reg <- if (is.null(predictor_regression)) predictor else predictor_regression

  # ---- Calcular a × b por mediador ---------------------------------------
  med_ids <- unname(mediators)
  med_lbl <- names(mediators)

  ab <- numeric(length(med_ids))
  for (i in seq_along(med_ids)) {
    a <- est[est$op == "~" & est$lhs == med_ids[i] & est$rhs == pred_reg, "est.std"]
    b <- est[est$op == "~" & est$lhs == outcome   & est$rhs == med_ids[i], "est.std"]
    ab[i] <- if (length(a) == 0 || length(b) == 0) NA_real_ else a[1] * b[1]
  }
  if (any(is.na(ab)))
    stop("No se encontraron coeficientes para los mediadores indicados.",
         call. = FALSE)

  # Indirecto total como base de comparación
  ind_key <- paste0(indirect_prefix, predictor)
  ind_tot_row <- est[est$op == ":=" & est$lhs == ind_key, ]
  ind_tot <- if (nrow(ind_tot_row) == 0) sum(ab) else ind_tot_row$est.std[1]

  # Proporción de cada mediador (puede ser negativa si supresor)
  prop <- ab / abs(ind_tot)
  abs_prop <- abs(prop)

  # ---- Paleta -------------------------------------------------------------
  if (is.null(mediator_colors)) {
    default_pal <- c("#F4A8A8","#FFAB91","#FFE082","#A5D6A7","#90CAF9","#CE93D8")
    mediator_colors <- setNames(default_pal[seq_along(med_lbl)], med_lbl)
  }

  # ---- Helper: un donut ---------------------------------------------------
  make_donut <- function(label, color, prop_val) {
    abs_p <- min(abs(prop_val), 1)
    sign_str <- if (prop_val < 0) "−" else "+"
    pct_text <- sprintf("%s%.1f %%", sign_str, 100 * abs_p)
    df <- data.frame(
      cat = factor(c("filled","empty"), levels = c("filled","empty")),
      ymin = c(0, abs_p),
      ymax = c(abs_p, 1)
    )
    ggplot2::ggplot(df) +
      ggplot2::geom_rect(
        ggplot2::aes(xmin = 3, xmax = 4, ymin = .data$ymin, ymax = .data$ymax,
                     fill = .data$cat)) +
      ggplot2::coord_polar(theta = "y") +
      ggplot2::xlim(c(0, 4.2)) +
      ggplot2::scale_fill_manual(values = c(filled = color,
                                             empty = remaining_color),
                                  guide = "none") +
      ggplot2::annotate("text", x = 0, y = 0,
                        label = pct_text, size = 5.5, fontface = "bold",
                        color = "grey15") +
      ggplot2::labs(title = label) +
      ggplot2::theme_void(base_size = 11) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 11,
                                            hjust = 0.5,
                                            color = "grey15"),
        plot.margin = ggplot2::margin(2, 2, 2, 2))
  }

  # ---- Construir donuts ---------------------------------------------------
  donuts <- mapply(make_donut,
                   label = med_lbl,
                   color = mediator_colors[med_lbl],
                   prop_val = prop,
                   SIMPLIFY = FALSE)

  if (show_total) {
    total_prop <- sum(abs_prop)
    sign_str <- if (ind_tot < 0) "−" else "+"
    df_tot <- data.frame(
      cat = factor(c("filled","empty"), levels = c("filled","empty")),
      ymin = c(0, min(total_prop, 1)),
      ymax = c(min(total_prop, 1), 1)
    )
    p_total <- ggplot2::ggplot(df_tot) +
      ggplot2::geom_rect(
        ggplot2::aes(xmin = 3, xmax = 4, ymin = .data$ymin, ymax = .data$ymax,
                     fill = .data$cat)) +
      ggplot2::coord_polar(theta = "y") +
      ggplot2::xlim(c(0, 4.2)) +
      ggplot2::scale_fill_manual(values = c(filled = "#3D405B",
                                             empty = remaining_color),
                                  guide = "none") +
      ggplot2::annotate("text", x = 0, y = 0,
                        label = sprintf("%s%.1f %%", sign_str,
                                         100 * min(total_prop, 1)),
                        size = 5.5, fontface = "bold", color = "grey15") +
      ggplot2::labs(title = "Indirecto total") +
      ggplot2::theme_void(base_size = 11) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 11,
                                            hjust = 0.5,
                                            color = "grey15"),
        plot.margin = ggplot2::margin(2, 2, 2, 2))
    donuts <- c(donuts, list(p_total))
  }

  # ---- Componer con patchwork --------------------------------------------
  if (is.null(ncol)) ncol <- length(donuts)
  combined <- patchwork::wrap_plots(donuts, ncol = ncol)
  combined <- combined +
    patchwork::plot_annotation(
      title = title, subtitle = subtitle,
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 13,
                                            hjust = 0),
        plot.subtitle = ggplot2::element_text(size = 10, color = "grey25",
                                               margin = ggplot2::margin(b = 6))))
  combined
}
