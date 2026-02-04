#' @title Plot Likert Scale Items
#' @description Creates a visualization for Likert scale items.
#' @param Data Data frame containing the survey data.
#' @param name_items Prefix for item names.
#' @param ranges Range of item numbers to include.
#' @param exclude Items to exclude (default NULL).
#' @param text_size Text size for labels (default 3).
#' @return A ggplot object.
#' @export
Plot_Likert <- function(Data, name_items, ranges, exclude = NULL, text_size = 3) {
  # Verificar que los paquetes necesarios esten disponibles
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("El paquete 'ggplot2' es necesario. Instalalo con: install.packages('ggplot2')")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("El paquete 'dplyr' es necesario. Instalalo con: install.packages('dplyr')")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("El paquete 'tidyr' es necesario. Instalalo con: install.packages('tidyr')")
  }

  # Construir nombres de columnas basados en los rangos especificados
  cols <- paste0(name_items, ranges)

  # Excluir columnas si se especifica
  if (!is.null(exclude)) {
    exclude_cols <- paste0(name_items, exclude)
    cols <- setdiff(cols, exclude_cols)
  }

  # Verificar que todas las columnas especificadas existan en el dataframe
  if (!all(cols %in% names(Data))) {
    stop("Algunas columnas especificadas no existen en el dataframe.")
  }

  # Transformar datos para el grafico
  df_pivot <- Data %>%
    tidyr::pivot_longer(cols = dplyr::all_of(cols), names_to = "Variables", values_to = "Var_Ptjes") %>%
    dplyr::mutate(Variables = factor(Variables, levels = dplyr::select(Data, dplyr::all_of(cols)) %>% names())) %>%
    dplyr::select(Variables, Var_Ptjes)

  # Generar el grafico
  ggplot2::ggplot(df_pivot, ggplot2::aes(x = as.factor(Var_Ptjes))) +
    ggplot2::geom_bar(fill = "#CCD1D1", colour = "black") +
    ggplot2::geom_text(stat = "count", ggplot2::aes(label = scales::percent(ggplot2::after_stat(count)/nrow(Data), accuracy = 0.01)),
              vjust = -0.1, size = text_size) +
    ggplot2::scale_y_continuous(limits = c(0, nrow(Data))) +
    ggplot2::facet_wrap(~ Variables, scale = "free_x") +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Tasas de respuesta", y = "") +
    ggplot2::theme(axis.text.y = ggplot2::element_blank())
}
