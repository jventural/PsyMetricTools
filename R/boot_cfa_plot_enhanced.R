#' @title Boot CFA Plot Enhanced (panel A enriquecido)
#' @description Versión mejorada de \code{boot_cfa_plot()} que mantiene el lenguaje
#'   visual base (boxplot azul + tabla descriptiva) pero agrega capas que responden
#'   más preguntas de un vistazo:
#'   \enumerate{
#'     \item Banda verde (\emph{pasa}) y roja (\emph{falla}) detrás del boxplot.
#'     \item Línea de cutoff (rojo punteado) con etiqueta lateral por columna.
#'     \item Jitter de las réplicas (azul muy claro) detrás del boxplot.
#'     \item Mediana con valor numérico anotado al lado del boxplot.
#'     \item Porcentaje de réplicas que cumplen el criterio (\emph{compliance \%})
#'           en verde bold sobre el panel.
#'     \item Tabla descriptiva con cabecera coloreada y bordes APA.
#'   }
#'   La función original \code{boot_cfa_plot()} permanece intacta.
#'
#' @param df Data frame producido por \code{boot_cfa()}.
#' @param save Lógico. Guardar el gráfico (default \code{FALSE}).
#' @param path Ruta del archivo a guardar.
#' @param dpi Resolución (default 600).
#' @param palette \[Deprecated\] Vector de 3 colores. Si se especifica, se usará
#'   ignorando \code{semantic_colors}. Se mantiene por compatibilidad hacia atrás.
#' @param accent_color \[Deprecated\] Color de acento. Si se especifica con
#'   \code{palette} no nulo, se usará. Si \code{semantic_colors=TRUE}, se ignora.
#' @param semantic_colors Lógico. Si \code{TRUE} (default), el color del boxplot
#'   se asigna según el veredicto de compliance: verde si \%pasa ≥ 90, rojo si
#'   < 90. Esto hace el plot dialogar visualmente con las bandas pasa/falla del
#'   fondo. Si \code{FALSE}, usa la paleta única \code{palette}.
#' @param pass_palette Vector de 3 verdes de claro a oscuro para variables que
#'   cumplen el criterio (default verdes de RColorBrewer).
#' @param fail_palette Vector de 3 rojos de claro a oscuro para variables que
#'   no cumplen el criterio.
#' @param pass_accent Color de borde/mediana para boxplots verdes.
#' @param fail_accent Color de borde/mediana para boxplots rojos.
#' @param compliance_threshold Umbral de \%pasa que separa "pass" de "fail"
#'   (default 90).
#' @param lang Idioma del gráfico: \code{"es"} (default) o \code{"en"}. Afecta
#'   etiquetas de eje Y, etiqueta de compliance (\emph{X\% pasa} / \emph{X\% pass})
#'   y demás textos visibles. Los nombres de los índices (CFI, RMSEA, etc.) y
#'   "cutoff", "Mdn" se mantienen como notación universal.
#' @param table_style Estadísticos descriptivos a reportar en la tabla bajo cada
#'   sub-panel. Tres opciones:
#'   \itemize{
#'     \item \code{"iqr_min_max"} (default) — IQR + min + max. Coherente con la
#'       mediana del boxplot (todos no-paramétricos / robustos).
#'     \item \code{"mean_sd_ci"} — mean + SD + IC 95\% percentil bootstrap. Estándar
#'       APA, comparable con la literatura clásica.
#'     \item \code{"cv_min_max"} — coeficiente de variación (\%) + min + max.
#'       Útil para comparar dispersión entre índices con escalas distintas, pero
#'       puede ser engañoso cuando la media se acerca a 0 (e.g., RMSEA bajo).
#'   }
#' @param cutoffs Lista nombrada con los valores de corte por índice. Default:
#'   \code{list(omega = 0.70, CFI = 0.95, TLI = 0.95, RMSEA = 0.08, SRMR = 0.08, CRMR = 0.08)}.
#' @param higher_better Lista nombrada lógica indicando si el cutoff es un mínimo
#'   (\code{TRUE}, e.g., CFI ≥ .95) o un máximo (\code{FALSE}, e.g., RMSEA ≤ .08).
#' @param show_jitter Mostrar jitter de réplicas detrás del boxplot (default \code{TRUE}).
#' @param show_compliance Mostrar \% compliance arriba (default \code{TRUE}).
#' @param show_median_label Mostrar etiqueta \code{Mdn = X.XXX} al lado del boxplot
#'   (default \code{TRUE}).
#' @param show_zone_bands Mostrar bandas verde/roja de fondo (default \code{TRUE}).
#' @param show_cutoff_label Mostrar etiqueta del valor del cutoff
#'   (default \code{TRUE}).
#' @param show_tables Mostrar tabla descriptiva debajo del boxplot
#'   (default \code{TRUE}).
#' @param exclude_indices Vector con índices a excluir (e.g., \code{c("CRMR")}).
#' @param ... Argumentos adicionales para \code{ggsave}.
#'
#' @return Un objeto \code{patchwork} listo para imprimirse o componer con
#'   \code{patchwork::wrap_elements()}.
#'
#' @examples
#' \dontrun{
#' results_boot <- boot_cfa(new_df = df, model_string = m,
#'                          item_prefix = "PHQ", n_replications = 1000)
#' boot_cfa_plot_enhanced(results_boot)
#'
#' # Combinar con ridgeline (Panel B)
#' library(patchwork)
#' panel_A <- wrap_elements(boot_cfa_plot_enhanced(results_boot)) + ggtitle("A")
#' panel_B <- wrap_elements(plot_cfa_stability_ridgeline(stab,
#'             gradient_colors = c("#9ecae1", "#08519c"))) + ggtitle("B")
#' panel_A / panel_B
#' }
#'
#' @export
#' @importFrom magrittr %>%
boot_cfa_plot_enhanced <- function(df,
                                    save = FALSE,
                                    path = "Plot_boot_cfa_enhanced.jpg",
                                    dpi = 600,
                                    palette = NULL,
                                    accent_color = NULL,
                                    semantic_colors = TRUE,
                                    pass_palette = c("#bae4b3", "#74c476", "#238b45"),
                                    fail_palette = c("#fcae91", "#fb6a4a", "#cb181d"),
                                    pass_accent  = "#00441b",
                                    fail_accent  = "#67000d",
                                    compliance_threshold = 90,
                                    lang = c("es", "en"),
                                    table_style = c("iqr_min_max", "mean_sd_ci", "cv_min_max"),
                                    cutoffs = list(omega = 0.70, CFI = 0.95, TLI = 0.95,
                                                   RMSEA = 0.08, SRMR = 0.08, CRMR = 0.08),
                                    higher_better = list(omega = TRUE, CFI = TRUE, TLI = TRUE,
                                                         RMSEA = FALSE, SRMR = FALSE, CRMR = FALSE),
                                    show_jitter = TRUE,
                                    show_compliance = TRUE,
                                    show_median_label = TRUE,
                                    show_zone_bands = TRUE,
                                    show_cutoff_label = TRUE,
                                    show_tables = TRUE,
                                    exclude_indices = NULL,
                                    ...) {

  lang <- match.arg(lang)
  table_style <- match.arg(table_style)
  i18n <- list(
    es = list(
      y_omega  = "ω (Confiabilidad)",
      y_comp   = "Comparativos",
      y_abs    = "Absolutos",
      pass_int = "%.0f%% pasa",
      pass_dec = "%.1f%% pasa"
    ),
    en = list(
      y_omega  = "ω (Reliability)",
      y_comp   = "Comparative",
      y_abs    = "Absolute",
      pass_int = "%.0f%% pass",
      pass_dec = "%.1f%% pass"
    )
  )[[lang]]

  ## ----- Constantes visuales -----
  # Modo legacy: si el usuario pasó palette+accent_color, se usa la paleta única
  legacy_mode <- !semantic_colors || (!is.null(palette) && !is.null(accent_color))
  if (legacy_mode) {
    if (is.null(palette)) palette <- c("#deebf7", "#9ecae1", "#3182bd")
    if (is.null(accent_color)) accent_color <- "#08519c"
  }
  red_cut    <- "#cb181d"
  green_pass <- "#238b45"
  zone_green <- "#e5f5e0"
  zone_red   <- "#fee0d2"

  # Helper: dada un valor pct_pass, retorna fill y accent según verdict
  pick_fills_accent <- function(ann_df, K) {
    if (legacy_mode) {
      legacy_fills <- if (K == 1) palette[3]
                       else if (K == 2) palette[2:3]
                       else palette
      if (K > 3) legacy_fills <- grDevices::colorRampPalette(palette)(K)
      return(list(fills = legacy_fills,
                  accents = rep(accent_color, K)))
    }
    # Modo semántico: por variable, asignar verde si pasa, rojo si falla
    fills <- character(K); accents <- character(K)
    n_pass <- sum(ann_df$pct_pass >= compliance_threshold, na.rm = TRUE)
    n_fail <- K - n_pass
    pass_idx <- which(ann_df$pct_pass >= compliance_threshold)
    fail_idx <- which(ann_df$pct_pass <  compliance_threshold)
    # Asignar gradiente claro→oscuro DENTRO de cada grupo
    if (n_pass > 0) {
      pp <- if (n_pass == 1) pass_palette[3]
            else if (n_pass == 2) pass_palette[2:3]
            else pass_palette
      if (n_pass > 3) pp <- grDevices::colorRampPalette(pass_palette)(n_pass)
      fills[pass_idx]   <- pp
      accents[pass_idx] <- pass_accent
    }
    if (n_fail > 0) {
      fp <- if (n_fail == 1) fail_palette[3]
            else if (n_fail == 2) fail_palette[2:3]
            else fail_palette
      if (n_fail > 3) fp <- grDevices::colorRampPalette(fail_palette)(n_fail)
      fills[fail_idx]   <- fp
      accents[fail_idx] <- fail_accent
    }
    list(fills = fills, accents = accents)
  }

  ## ----- Helpers (parametrizables) -----
  apa_table_theme <- function(header_fill, accent) {
    gridExtra::ttheme_default(
      core    = list(bg_params = list(fill = "white", col = NA),
                     fg_params = list(fontface = 1)),
      colhead = list(bg_params = list(fill = header_fill, col = NA),
                     fg_params = list(col = accent, fontface = 2)),
      rowhead = list(fg_params = list(col = "black", fontface = 1)),
      base_size = 9
    )
  }

  add_h_borders <- function(tbl, accent) {
    tbl <- gtable::gtable_add_grob(tbl,
      grobs = grid::segmentsGrob(x0 = grid::unit(0, "npc"), x1 = grid::unit(1, "npc"),
                                  y0 = grid::unit(1, "npc"), y1 = grid::unit(1, "npc"),
                                  gp = grid::gpar(lwd = 1.5, col = accent)),
      t = 1, l = 1, r = ncol(tbl))
    tbl <- gtable::gtable_add_grob(tbl,
      grobs = grid::segmentsGrob(x0 = grid::unit(0, "npc"), x1 = grid::unit(1, "npc"),
                                  y0 = grid::unit(0, "npc"), y1 = grid::unit(0, "npc"),
                                  gp = grid::gpar(lwd = 1.5, col = accent)),
      t = nrow(tbl), l = 1, r = ncol(tbl))
    if (nrow(tbl) > 1) {
      tbl <- gtable::gtable_add_grob(tbl,
        grobs = grid::segmentsGrob(x0 = grid::unit(0, "npc"), x1 = grid::unit(1, "npc"),
                                    y0 = grid::unit(1, "npc") - grid::unit(1, "pt"),
                                    y1 = grid::unit(1, "npc") - grid::unit(1, "pt"),
                                    gp = grid::gpar(lwd = 0.6, col = "grey70")),
        t = 2, l = 1, r = ncol(tbl))
    }
    tbl
  }

  build_panel <- function(values_list, vars, y_label) {
    K <- length(vars)
    dat_long <- do.call(rbind, lapply(seq_along(vars), function(i) {
      data.frame(xn = i, var = vars[i], Value = values_list[[vars[i]]],
                 stringsAsFactors = FALSE)
    }))

    # Opción A: mean, SD, IC 95% percentil bootstrap (default histórico)
    # Opción B: CV (Coefficient of Variation = SD/Mean*100), min, max
    fmt_ic <- function(lo, hi) {
      lo_s <- sub("^(-?)0\\.", "\\1.", sprintf("%.3f", lo))
      hi_s <- sub("^(-?)0\\.", "\\1.", sprintf("%.3f", hi))
      sprintf("[%s, %s]", lo_s, hi_s)
    }
    if (table_style == "mean_sd_ci") {
      res_tbl <- dat_long %>%
        dplyr::group_by(.data$var) %>%
        dplyr::summarise(
          mean    = round(mean(.data$Value, na.rm = TRUE), 3),
          SD      = round(stats::sd(.data$Value, na.rm = TRUE), 3),
          `95% CI` = fmt_ic(
            stats::quantile(.data$Value, 0.025, na.rm = TRUE),
            stats::quantile(.data$Value, 0.975, na.rm = TRUE)
          ),
          .groups = "drop"
        ) %>%
        dplyr::arrange(match(.data$var, vars)) %>%
        dplyr::rename(Index = "var")
    } else if (table_style == "cv_min_max") {
      # cv_min_max: coefficient of variation (%) + min + max
      res_tbl <- dat_long %>%
        dplyr::group_by(.data$var) %>%
        dplyr::summarise(
          `CV (%)` = round(stats::sd(.data$Value, na.rm = TRUE) /
                             mean(.data$Value, na.rm = TRUE) * 100, 2),
          min      = round(min(.data$Value, na.rm = TRUE), 3),
          max      = round(max(.data$Value, na.rm = TRUE), 3),
          .groups = "drop"
        ) %>%
        dplyr::arrange(match(.data$var, vars)) %>%
        dplyr::rename(Index = "var")
    } else {
      # iqr_min_max: non-parametric coherence with median in plot
      res_tbl <- dat_long %>%
        dplyr::group_by(.data$var) %>%
        dplyr::summarise(
          IQR = round(stats::quantile(.data$Value, 0.75, na.rm = TRUE) -
                        stats::quantile(.data$Value, 0.25, na.rm = TRUE), 3),
          min = round(min(.data$Value, na.rm = TRUE), 3),
          max = round(max(.data$Value, na.rm = TRUE), 3),
          .groups = "drop"
        ) %>%
        dplyr::arrange(match(.data$var, vars)) %>%
        dplyr::rename(Index = "var")
    }

    ann_df <- data.frame(var = vars, xn = seq_along(vars),
                         stringsAsFactors = FALSE)
    ann_df$cutoff <- vapply(ann_df$var, function(v) {
      if (!is.null(cutoffs[[v]])) cutoffs[[v]] else NA_real_
    }, numeric(1))
    ann_df$higher <- vapply(ann_df$var, function(v) {
      if (!is.null(higher_better[[v]])) isTRUE(higher_better[[v]]) else TRUE
    }, logical(1))
    ann_df$median_v <- vapply(ann_df$var, function(v) median(values_list[[v]], na.rm = TRUE),
                              numeric(1))
    ann_df$pct_pass <- vapply(seq_len(nrow(ann_df)), function(i) {
      v <- ann_df$var[i]; cutv <- ann_df$cutoff[i]
      if (is.na(cutv)) return(NA_real_)
      if (ann_df$higher[i]) mean(values_list[[v]] >= cutv) * 100
      else                  mean(values_list[[v]] <= cutv) * 100
    }, numeric(1))
    ann_df$xmin <- ann_df$xn - 0.45
    ann_df$xmax <- ann_df$xn + 0.45

    all_vals <- unlist(values_list)
    # Espacio extra arriba para compliance label, abajo para Mdn label
    y_lo <- min(c(all_vals, ann_df$cutoff), na.rm = TRUE) - 0.05
    y_hi <- max(c(all_vals, ann_df$cutoff), na.rm = TRUE) + 0.07

    # Asignar colores según veredicto (verde si pasa, rojo si falla) o legacy
    fa <- pick_fills_accent(ann_df, K)
    fills_var   <- fa$fills
    accents_var <- fa$accents
    # Color "dominante" para axis title, mediana, outliers
    panel_accent <- if (length(unique(accents_var)) == 1) accents_var[1] else "grey25"
    # Header de tabla: combinar tono claro del grupo dominante
    n_pass_panel <- sum(ann_df$pct_pass >= compliance_threshold, na.rm = TRUE)
    header_fill <- if (n_pass_panel == K) pass_palette[1]
                    else if (n_pass_panel == 0) fail_palette[1]
                    else "#e2e8f0"

    bp <- ggplot2::ggplot()

    if (isTRUE(show_zone_bands)) {
      bp <- bp +
        ggplot2::geom_rect(data = ann_df,
          ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                       ymin = ifelse(.data$higher, .data$cutoff, -Inf),
                       ymax = ifelse(.data$higher, Inf,           .data$cutoff)),
          fill = zone_green, alpha = 0.55) +
        ggplot2::geom_rect(data = ann_df,
          ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                       ymin = ifelse(.data$higher, -Inf,           .data$cutoff),
                       ymax = ifelse(.data$higher, .data$cutoff, Inf)),
          fill = zone_red, alpha = 0.40)
    }

    bp <- bp +
      ggplot2::geom_segment(data = ann_df,
        ggplot2::aes(x = .data$xmin, xend = .data$xmax,
                     y = .data$cutoff, yend = .data$cutoff),
        color = red_cut, linetype = "dashed", linewidth = 0.7)

    if (isTRUE(show_cutoff_label)) {
      # Si todos los cutoffs son iguales, una sola etiqueta a la derecha del panel.
      # Si difieren, una etiqueta por columna.
      uniq_cutoffs <- unique(ann_df$cutoff[!is.na(ann_df$cutoff)])
      if (length(uniq_cutoffs) == 1) {
        bp <- bp +
          ggplot2::annotate("text",
            x = K + 0.55, y = uniq_cutoffs,
            label = sprintf("cutoff = %.2f", uniq_cutoffs),
            hjust = 1, vjust = -0.4, size = 2.6, color = red_cut,
            fontface = "italic")
      } else {
        bp <- bp +
          ggplot2::geom_text(data = ann_df,
            ggplot2::aes(x = .data$xmax + 0.02, y = .data$cutoff,
                         label = sprintf("cutoff = %.2f", .data$cutoff)),
            hjust = 0, vjust = -0.4, size = 2.6, color = red_cut,
            fontface = "italic")
      }
    }

    if (isTRUE(show_jitter)) {
      bp <- bp +
        ggplot2::geom_jitter(data = dat_long,
          ggplot2::aes(x = .data$xn, y = .data$Value, color = factor(.data$xn)),
          width = 0.16, height = 0, alpha = 0.18, size = 0.5,
          show.legend = FALSE)
    }

    bp <- bp +
      ggplot2::geom_boxplot(data = dat_long,
        ggplot2::aes(x = .data$xn, y = .data$Value,
                     fill  = factor(.data$xn),
                     color = factor(.data$xn),
                     group = .data$xn),
        width = 0.45, outlier.shape = 21, outlier.fill = panel_accent,
        outlier.color = "white", outlier.size = 1.6,
        alpha = 0.92, linewidth = 0.5,
        show.legend = FALSE)

    if (isTRUE(show_median_label)) {
      # Mdn debajo del boxplot dentro de la columna: x = xn (centrado), hjust = 0.5
      # y = mínimo del jitter - pequeño offset para no chocar con el bigote inferior
      ann_df$mdn_y <- vapply(ann_df$var, function(v) min(values_list[[v]], na.rm = TRUE),
                              numeric(1)) - (y_hi - y_lo) * 0.025
      # Color del Mdn label = accent de la columna (verde si pasa, rojo si falla)
      bp <- bp +
        ggplot2::geom_label(data = ann_df,
          ggplot2::aes(x = .data$xn, y = .data$mdn_y,
                       label = sprintf("Mdn = %.3f", .data$median_v)),
          hjust = 0.5, size = 2.6, fontface = "bold",
          color = accents_var,
          fill = "white", label.padding = grid::unit(0.10, "lines"),
          label.r = grid::unit(0, "lines"))
    }

    if (isTRUE(show_compliance)) {
      # Formato compacto: si es 100% exacto u entero, sin decimal
      ann_df$pct_label <- ifelse(
        abs(ann_df$pct_pass - round(ann_df$pct_pass)) < 0.05,
        sprintf(i18n$pass_int, ann_df$pct_pass),
        sprintf(i18n$pass_dec, ann_df$pct_pass)
      )
      bp <- bp +
        ggplot2::geom_label(data = ann_df,
          ggplot2::aes(x = .data$xn, y = y_hi - (y_hi - y_lo) * 0.02,
                       label = .data$pct_label),
          size = 2.6, fontface = "bold", color = green_pass,
          fill = "#f0fff0", label.padding = grid::unit(0.10, "lines"),
          label.r = grid::unit(0, "lines"),
          label.size = 0)
    }

    bp <- bp +
      ggplot2::scale_fill_manual(values = fills_var) +
      ggplot2::scale_color_manual(values = accents_var) +
      ggplot2::scale_x_continuous(breaks = ann_df$xn, labels = ann_df$var,
                                   limits = c(0.4, K + 0.95)) +
      ggplot2::coord_cartesian(ylim = c(y_lo, y_hi), clip = "off") +
      ggplot2::labs(y = y_label, x = NULL) +
      ggplot2::theme_bw(base_size = 11) +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor   = ggplot2::element_blank(),
        axis.text          = ggplot2::element_text(color = "black"),
        axis.title.y       = ggplot2::element_text(color = panel_accent, face = "bold"),
        plot.margin        = ggplot2::margin(8, 4, 4, 8)
      )

    if (isTRUE(show_tables)) {
      tbl_grob <- gridExtra::tableGrob(res_tbl, rows = NULL,
                                        theme = apa_table_theme(header_fill, panel_accent))
      tbl_grob <- add_h_borders(tbl_grob, panel_accent)
      bp / patchwork::wrap_elements(full = tbl_grob) +
        patchwork::plot_layout(heights = c(3.2, 1))
    } else {
      bp
    }
  }

  ## ----- Extraer datos -----
  # Omega: columnas DESPUÉS de fit_measures1 (convención boot_cfa)
  if ("fit_measures1" %in% names(df)) {
    idx <- which(names(df) == "fit_measures1")
    omega_dat <- df[, -seq_len(idx), drop = FALSE]
  } else {
    omega_dat <- df[, vapply(df, is.numeric, logical(1)), drop = FALSE]
  }
  omega_list <- as.list(omega_dat)
  # Renombrar primera columna a "omega" si solo hay una
  if (length(omega_list) == 1) {
    names(omega_list) <- "omega"
  }

  # CFI/TLI/RMSEA/SRMR/CRMR desde fit_measures1
  fit_df <- purrr::map_dfr(df$fit_measures1, tibble::as_tibble)
  comp_indices <- intersect(c("CFI", "TLI"), names(fit_df))
  abs_indices  <- intersect(c("RMSEA", "SRMR", "CRMR"), names(fit_df))
  if (!is.null(exclude_indices)) {
    comp_indices <- setdiff(comp_indices, toupper(exclude_indices))
    abs_indices  <- setdiff(abs_indices,  toupper(exclude_indices))
  }

  comp_list <- lapply(comp_indices, function(v) round(fit_df[[v]], 3))
  names(comp_list) <- comp_indices
  abs_list  <- lapply(abs_indices,  function(v) round(fit_df[[v]], 3))
  names(abs_list)  <- abs_indices

  ## ----- Construir paneles -----
  plots <- list()
  if (length(omega_list) > 0)
    plots <- c(plots, list(build_panel(omega_list, names(omega_list), i18n$y_omega)))
  if (length(comp_list) > 0)
    plots <- c(plots, list(build_panel(comp_list, names(comp_list), i18n$y_comp)))
  if (length(abs_list)  > 0)
    plots <- c(plots, list(build_panel(abs_list,  names(abs_list),  i18n$y_abs)))

  if (length(plots) == 0) {
    stop("No hay datos para graficar.")
  }

  combined <- Reduce("|", plots)
  if (length(plots) >= 3) {
    combined <- combined + patchwork::plot_layout(widths = c(0.85, 1, 1.2))
  }

  if (isTRUE(save)) {
    ggplot2::ggsave(filename = path, plot = combined,
                    width = 13, height = 6.5, dpi = dpi, units = "in", ...)
  }

  class(combined) <- c("boot_cfa_plot_enhanced", class(combined))
  invisible(combined)
}
