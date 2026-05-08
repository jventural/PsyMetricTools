#' @title Forest plot of direct, indirect, and total effects in a SEM model
#' @description Visualización alternativa al diagrama de senderos clásico cuando
#'   el modelo tiene muchos predictores distales. Genera un \emph{forest plot}
#'   horizontal que ordena cada predictor por la magnitud absoluta de su efecto
#'   \eqn{\beta} estandarizado y muestra simultáneamente los efectos directos,
#'   indirectos (\eqn{a \times b}) y totales con intervalos de confianza al 95 \%.
#'   Sigue la recomendación de Fife y Mendoza (2018) sobre presentar la
#'   descomposición de efectos en un panel separado en lugar de saturar el path
#'   diagram con los tres tipos de coeficientes.
#'
#' @param fit Objeto \code{lavaan} ajustado con efectos definidos vía
#'   \code{:=} en la sintaxis lavaan (por ejemplo, \code{ind_X := a*b} para el
#'   efecto indirecto y \code{tot_X := c + ind_X} para el total).
#' @param predictors Vector con los \emph{stems} de los predictores distales,
#'   tal como aparecen tras los prefijos \code{ind_} y \code{tot_} en las
#'   definiciones \code{:=} del modelo. Por ejemplo, si las definiciones son
#'   \code{ind_sueno := a*b} y \code{tot_sueno := c + ind_sueno}, el stem es
#'   \code{"sueno"}.
#' @param outcome ID del outcome (default \code{"suic"}; usar el nombre del
#'   \code{lhs} de la regresión final del modelo).
#' @param predictor_regression Vector \emph{named} que mapea cada \code{stem}
#'   en \code{predictors} con el nombre exacto de la variable como aparece en
#'   la regresión \code{outcome ~ X}. Útil cuando el modelo usa stems cortos en
#'   las definiciones \code{:=} pero nombres completos en las regresiones (por
#'   ejemplo, \code{ind_sueno} junto con \code{suic ~ sueno_num}). Si \code{NULL},
#'   se asume que coinciden.
#' @param indirect_prefix Prefijo de los parámetros definidos para los efectos
#'   indirectos (default \code{"ind_"}). Se busca \code{paste0(prefix, predictor)}
#'   dentro de \code{lavaan::standardizedSolution(fit)} con \code{op == ":="}.
#' @param total_prefix Prefijo de los parámetros definidos para los efectos
#'   totales (default \code{"tot_"}).
#' @param predictor_labels Vector \emph{named} para mostrar etiquetas legibles
#'   en el eje vertical. Sus nombres deben coincidir con los \code{stems} de
#'   \code{predictors}.
#' @param effect_levels Vector de longitud máximo 3 con las componentes a graficar
#'   (default \code{c("Directo","Indirecto","Total")}). Permite ocultar tipos.
#' @param effect_colors Vector con los colores asignados a las tres componentes.
#'   Default \code{c(Directo = "#5B8DBE", Indirecto = "#E07A5F", Total = "#3D405B")}.
#' @param layout Disposición visual: \code{"facet"} (default) coloca cada tipo
#'   de efecto en un panel separado lado a lado, evitando el solapamiento de
#'   etiquetas \eqn{p}; \code{"stacked"} reproduce la versión clásica con los
#'   tres efectos en un único panel y \emph{dodge} vertical (modo legado).
#' @param effect_dodge Distancia vertical entre las tres componentes en modo
#'   \code{"stacked"} (default \code{0.55}). Ignorado en modo \code{"facet"}.
#' @param sig_threshold Umbral de significancia para destacar coeficientes
#'   (default \code{0.05}).
#' @param sort_by Forma de ordenar el eje vertical: \code{"total"} (default,
#'   por |\eqn{\beta}| total descendente), \code{"indirect"} o \code{"direct"}.
#' @param show_zero Lógico. Dibujar la línea vertical de referencia en
#'   \eqn{\beta = 0} (default \code{TRUE}).
#' @param show_p_text Lógico. Anotar el valor de \eqn{p} junto a cada punto
#'   (default \code{TRUE}).
#' @param show_beta_text Lógico. Anotar el valor de \eqn{\beta} a la izquierda
#'   del IC (default \code{FALSE}; útil en modo \code{"facet"} cuando se quiere
#'   tabla de coeficientes integrada).
#' @param p_text_position Posición horizontal de la etiqueta \eqn{p}:
#'   \code{"right"} (default; al final del IC superior) o \code{"panel"} (al
#'   margen derecho de cada panel; recomendado en modo \code{"facet"}).
#' @param point_size Tamaño de los puntos (default 2.4).
#' @param error_width Grosor de las barras de IC (default 0.7).
#' @param title,subtitle,caption Cadenas de texto opcionales.
#' @param x_lim Vector numérico de longitud 2 con los límites del eje X (default
#'   \code{NULL} = automáticos).
#' @param facet_scales Pasado a \code{ggplot2::facet_wrap()} cuando
#'   \code{layout = "facet"} (default \code{"fixed"}; usar \code{"free_x"} si
#'   las escalas de los efectos difieren mucho).
#'
#' @return Objeto \code{ggplot} listo para imprimir o guardar con
#'   \code{ggsave()}.
#'
#' @details
#' La función toma los efectos definidos con \code{:=} en la sintaxis lavaan y
#' los reorganiza en un \emph{long format} apto para \code{ggplot2}. Cada
#' predictor genera un bloque vertical con los tres tipos de efecto (cuando
#' están disponibles); las barras representan IC 95 \% obtenidos directamente
#' de \code{lavaan::standardizedSolution()}.
#'
#' Comparado con el path diagram de \code{\link{plot_path_mediation}}, este
#' formato es preferible cuando \eqn{\ge 8} predictores distales saturan
#' visualmente el diagrama de tres columnas, o cuando se necesita comunicar la
#' descomposición \emph{directo + indirecto = total} con precisión cuantitativa.
#'
#' @references
#' Fife, D. A., & Mendoza, J. L. (2018). Beyond path diagrams: Enhancing applied
#' structural equation modeling research through data visualization.
#' \emph{Addictive Behaviors, 94}, 232--239.
#' \doi{10.1016/j.addbeh.2018.08.030}
#'
#' Preacher, K. J., & Hayes, A. F. (2008). Asymptotic and resampling strategies
#' for assessing and comparing indirect effects in multiple mediator models.
#' \emph{Behavior Research Methods, 40}(3), 879--891.
#' \doi{10.3758/BRM.40.3.879}
#'
#' @examples
#' \dontrun{
#' library(lavaan)
#' library(PsyMetricTools)
#'
#' mod <- '
#'   M1 ~ a1*X1 + a2*X2
#'   Y  ~ b1*M1 + c1*X1 + c2*X2
#'   ind_X1 := a1*b1
#'   ind_X2 := a2*b1
#'   tot_X1 := c1 + ind_X1
#'   tot_X2 := c2 + ind_X2
#' '
#' fit <- sem(mod, data = your_data)
#'
#' plot_mediation_forest(
#'   fit,
#'   predictors = c("X1","X2"),
#'   outcome = "Y",
#'   predictor_labels = c(X1 = "Predictor 1", X2 = "Predictor 2")
#' )
#' }
#' @importFrom ggplot2 ggplot aes geom_vline geom_linerange geom_point
#'   geom_text scale_color_manual scale_y_discrete scale_x_continuous labs
#'   theme_minimal theme element_text element_blank element_line position_dodge
#'   margin facet_wrap
#' @importFrom dplyr filter mutate arrange desc
#' @importFrom lavaan standardizedSolution
#' @export
plot_mediation_forest <- function(fit,
                                  predictors,
                                  outcome = "suic",
                                  predictor_regression = NULL,
                                  indirect_prefix = "ind_",
                                  total_prefix = "tot_",
                                  predictor_labels = NULL,
                                  effect_levels = c("Directo","Indirecto","Total"),
                                  effect_colors = c(Directo  = "#5B8DBE",
                                                    Indirecto = "#E07A5F",
                                                    Total    = "#3D405B"),
                                  layout = c("facet","stacked"),
                                  effect_dodge = 0.55,
                                  sig_threshold = 0.05,
                                  sort_by = c("total","indirect","direct"),
                                  show_zero = TRUE,
                                  show_p_text = TRUE,
                                  show_beta_text = FALSE,
                                  p_text_position = c("right","panel"),
                                  point_size = 2.4,
                                  error_width = 0.7,
                                  title = NULL,
                                  subtitle = NULL,
                                  caption = NULL,
                                  x_lim = NULL,
                                  facet_scales = "fixed") {

  # ---- Validaciones --------------------------------------------------------
  if (!inherits(fit, "lavaan"))
    stop("`fit` debe ser un objeto lavaan ajustado.", call. = FALSE)
  if (length(predictors) < 1)
    stop("Especifique al menos un predictor.", call. = FALSE)
  sort_by <- match.arg(sort_by)
  layout <- match.arg(layout)
  p_text_position <- match.arg(p_text_position)

  est <- lavaan::standardizedSolution(fit)

  # ---- Mapeo stem -> regression name --------------------------------------
  if (is.null(predictor_regression)) {
    predictor_regression <- setNames(predictors, predictors)
  } else {
    miss <- setdiff(predictors, names(predictor_regression))
    if (length(miss) > 0) {
      add <- setNames(miss, miss)
      predictor_regression <- c(predictor_regression, add)
    }
  }

  # Directo: regresión outcome ~ predictor (usando regression names)
  reg_names <- predictor_regression[predictors]
  direct_raw <- est[est$op == "~" & est$lhs == outcome &
                     est$rhs %in% reg_names, , drop = FALSE]
  reg_to_stem <- setNames(names(predictor_regression), predictor_regression)
  direct_df <- data.frame(
    predictor = unname(reg_to_stem[direct_raw$rhs]),
    effect = factor("Directo", levels = effect_levels),
    beta = direct_raw$est.std, lo = direct_raw$ci.lower,
    hi = direct_raw$ci.upper, p = direct_raw$pvalue,
    stringsAsFactors = FALSE)

  # Indirecto y total: parámetros := definidos (matching exacto)
  pull_def <- function(prefix) {
    keys_full <- paste0(prefix, predictors)
    sub <- est[est$op == ":=" & est$lhs %in% keys_full, , drop = FALSE]
    if (nrow(sub) == 0)
      return(data.frame(predictor = character(0), beta = numeric(0),
                         lo = numeric(0), hi = numeric(0), p = numeric(0),
                         stringsAsFactors = FALSE))
    pref_re <- paste0("^", prefix)
    data.frame(
      predictor = sub("^.*?_", "", sub$lhs),  # remueve cualquier prefijo X_
      beta = sub$est.std, lo = sub$ci.lower, hi = sub$ci.upper,
      p = sub$pvalue, stringsAsFactors = FALSE)
  }
  indirect_df <- pull_def(indirect_prefix)
  if (nrow(indirect_df) > 0)
    indirect_df$effect <- factor("Indirecto", levels = effect_levels)
  total_df <- pull_def(total_prefix)
  if (nrow(total_df) > 0)
    total_df$effect <- factor("Total", levels = effect_levels)

  long_df <- rbind(
    direct_df[, c("predictor","effect","beta","lo","hi","p")],
    if (nrow(indirect_df) > 0)
      indirect_df[, c("predictor","effect","beta","lo","hi","p")] else NULL,
    if (nrow(total_df) > 0)
      total_df[, c("predictor","effect","beta","lo","hi","p")] else NULL)
  long_df <- long_df[long_df$effect %in% effect_levels, , drop = FALSE]
  if (nrow(long_df) == 0)
    stop("No se encontraron efectos para los predictores indicados.",
         call. = FALSE)

  # ---- Etiquetas y orden --------------------------------------------------
  if (!is.null(predictor_labels)) {
    long_df$pred_label <- ifelse(long_df$predictor %in% names(predictor_labels),
                                  predictor_labels[long_df$predictor],
                                  long_df$predictor)
  } else {
    long_df$pred_label <- long_df$predictor
  }

  sort_effect <- switch(sort_by,
                        total = "Total",
                        indirect = "Indirecto",
                        direct = "Directo")
  if (!sort_effect %in% as.character(long_df$effect))
    sort_effect <- as.character(long_df$effect)[1]
  ord_df <- long_df[as.character(long_df$effect) == sort_effect, , drop = FALSE]
  ord_df <- ord_df[order(-abs(ord_df$beta)), , drop = FALSE]
  long_df$pred_label <- factor(long_df$pred_label, levels = rev(ord_df$pred_label))

  # ---- Significancia para forma de punto ----------------------------------
  long_df$sig <- ifelse(long_df$p < sig_threshold, "sig", "ns")

  # ---- Construir ggplot ----------------------------------------------------
  fmt_p <- function(p) {
    sapply(p, function(x) {
      if (is.na(x)) return("")
      if (x < .001) "p<.001"
      else paste0("p=", sub("0\\.", ".", sprintf("%.3f", x)))
    })
  }
  long_df$p_text <- fmt_p(long_df$p)

  # ---- Capa base (común a ambos layouts) ----------------------------------
  use_dodge <- (layout == "stacked") && length(unique(long_df$effect)) > 1
  pos <- if (use_dodge) ggplot2::position_dodge(width = effect_dodge)
         else ggplot2::position_identity()

  p <- ggplot2::ggplot(long_df,
                       ggplot2::aes(x = .data$beta, y = .data$pred_label,
                                    color = .data$effect))
  if (show_zero)
    p <- p + ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                                  color = "grey60", linewidth = 0.4)

  p <- p +
    ggplot2::geom_linerange(
      ggplot2::aes(xmin = .data$lo, xmax = .data$hi),
      linewidth = error_width, position = pos) +
    ggplot2::geom_point(
      ggplot2::aes(shape = .data$sig, fill = .data$effect),
      size = point_size, position = pos, stroke = 0.8) +
    ggplot2::scale_color_manual(values = effect_colors, drop = FALSE) +
    ggplot2::scale_fill_manual(values = effect_colors, drop = FALSE,
                                guide = "none") +
    ggplot2::scale_shape_manual(values = c(sig = 21, ns = 1),
                                 labels = c(sig = "Significativo",
                                            ns = "No significativo"),
                                 name = NULL) +
    ggplot2::labs(x = expression(beta ~ estandarizado ~ "(IC 95 %)"),
                  y = NULL, color = "Tipo de efecto",
                  title = title, subtitle = subtitle, caption = caption) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 13,
                                          hjust = 0),
      plot.subtitle = ggplot2::element_text(size = 10, color = "grey25",
                                             margin = ggplot2::margin(b = 8)),
      plot.caption = ggplot2::element_text(size = 8.5, color = "grey35",
                                            hjust = 0,
                                            margin = ggplot2::margin(t = 8)),
      panel.grid.major.y = ggplot2::element_line(color = "grey92"),
      panel.grid.major.x = ggplot2::element_line(color = "grey95"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "top", legend.box = "horizontal",
      axis.text.y = ggplot2::element_text(color = "grey15"),
      strip.text = ggplot2::element_text(face = "bold", size = 11,
                                          color = "grey15",
                                          margin = ggplot2::margin(b = 4, t = 2)),
      strip.background = ggplot2::element_blank(),
      panel.spacing.x = ggplot2::unit(1.0, "lines"),
      plot.margin = ggplot2::margin(12, 16, 12, 12))

  # ---- Etiquetas β y p ----------------------------------------------------
  if (show_beta_text) {
    fmt_b <- function(x) {
      s <- sprintf("%.3f", x)
      ifelse(substr(s, 1, 2) == "0.", paste0(".", substr(s, 3, nchar(s))),
             ifelse(substr(s, 1, 3) == "-0.",
                    paste0("-.", substr(s, 4, nchar(s))), s))
    }
    long_df$beta_text <- fmt_b(long_df$beta)
    p <- p +
      ggplot2::geom_text(
        data = long_df,
        ggplot2::aes(label = .data$beta_text, x = .data$lo),
        position = pos, hjust = 1.15, size = 2.7, color = "grey25",
        show.legend = FALSE)
  }

  if (show_p_text) {
    if (p_text_position == "panel" && layout == "facet") {
      # Una etiqueta p por punto, alineada al margen derecho del panel via x = Inf
      p <- p +
        ggplot2::geom_text(
          data = long_df,
          ggplot2::aes(label = .data$p_text),
          x = Inf, hjust = 1.05, size = 2.7, color = "grey25",
          show.legend = FALSE)
    } else {
      p <- p +
        ggplot2::geom_text(
          data = long_df,
          ggplot2::aes(label = .data$p_text, x = .data$hi),
          position = pos, hjust = -0.15, size = 2.7, color = "grey25",
          show.legend = FALSE)
    }
  }

  # ---- Faceting opcional --------------------------------------------------
  if (layout == "facet" && length(unique(long_df$effect)) > 1) {
    p <- p + ggplot2::facet_wrap(~ effect, nrow = 1, scales = facet_scales)
  }

  if (!is.null(x_lim))
    p <- p + ggplot2::scale_x_continuous(limits = x_lim, expand = ggplot2::expansion(mult = c(0.05, 0.18)))
  else
    p <- p + ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.18)))

  p
}
