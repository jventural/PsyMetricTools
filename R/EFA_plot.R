#' @title Plot EFA Factor Loadings
#' @description Creates a visualization of factor loadings from EFA.
#' @param specifications Lavaan model object or a data.frame with columns:
#'   est.std, ci.lower, ci.upper, lhs, rhs, op.
#' @param item_prefix Prefix for item names.
#' @return A ggplot object showing factor loadings.
#' @export
EFA_plot <- function(specifications, item_prefix) {
  # Verificar tipo de input y preparar datos

if (inherits(specifications, "lavaan")) {
    # Si es objeto lavaan, extraer soluciones estandarizadas
    std_solution <- lavaan::standardizedsolution(specifications)
  } else if (is.data.frame(specifications)) {
    # Si es dataframe, verificar columnas requeridas
    required_cols <- c("est.std", "ci.lower", "ci.upper", "lhs", "rhs", "op")
    missing_cols <- setdiff(required_cols, names(specifications))
    if (length(missing_cols) > 0) {
      stop("El data.frame debe contener las columnas: ",
           paste(required_cols, collapse = ", "),
           "\nColumnas faltantes: ", paste(missing_cols, collapse = ", "))
    }
    std_solution <- specifications
  } else if (is.list(specifications) && !is.data.frame(specifications)) {
    # Si es una lista, intentar convertir a dataframe
    std_solution <- tryCatch({
      as.data.frame(specifications[[1]])
    }, error = function(e) {
      stop("No se pudo convertir la lista a data.frame. ",
           "Proporcione un objeto lavaan o un data.frame con columnas: ",
           "est.std, ci.lower, ci.upper, lhs, rhs, op")
    })
    required_cols <- c("est.std", "ci.lower", "ci.upper", "lhs", "rhs", "op")
    missing_cols <- setdiff(required_cols, names(std_solution))
    if (length(missing_cols) > 0) {
      stop("El data.frame debe contener las columnas: ",
           paste(required_cols, collapse = ", "),
           "\nColumnas faltantes: ", paste(missing_cols, collapse = ", "))
    }
  } else {
    stop("'specifications' debe ser un objeto lavaan, un data.frame, o una lista. ",
         "Clase recibida: ", class(specifications)[1])
  }

  # Preparar los datos con una nueva columna que indique si un item es complejo
  complex_items <- std_solution %>%
    dplyr::filter(op == "=~") %>%
    dplyr::mutate(
      item = as.numeric(stringr::str_remove(rhs, item_prefix)),
      factor = as.factor(lhs)
    ) %>%
    dplyr::group_by(item) %>%
    dplyr::mutate(complex = sum(est.std > 0.30) > 1) %>%
    dplyr::ungroup()

  # Graficar con corrección de la condición para el borde rojo
  figure2 <- ggplot2::ggplot(complex_items, ggplot2::aes(x = est.std, xmin = ci.lower, xmax = ci.upper, y = item)) +
    ggplot2::annotate(geom = "rect",
             xmin = -1, xmax = 1,
             ymin = -Inf, ymax = Inf,
             fill = "grey90") +
    ggplot2::annotate(geom = "rect",
             xmin = -0.7, xmax = 0.7,
             ymin = -Inf, ymax = Inf,
             fill = "grey93") +
    ggplot2::annotate(geom = "rect",
             xmin = -0.4, xmax = 0.4,
             ymin = -Inf, ymax = Inf,
             fill = "grey96") +
    ggplot2::geom_vline(xintercept = 0, color = "white") +
    ggplot2::geom_pointrange(ggplot2::aes(alpha = abs(est.std) < 0.4,
                        color = "black"),
                    fatten = 5,
                    size = 0.8,
                    shape = 21,
                    fill = "black",
                    stroke = ifelse(complex_items$complex, 1, 0),
                    color = ifelse(complex_items$complex, "red", "black")) +
    ggplot2::geom_text(ggplot2::aes(label = item, color = abs(est.std) < 0.4), size = 2) +
    ggplot2::scale_color_manual(values = c("white", "black"), guide = "none") +
    ggplot2::scale_alpha_manual(values = c(1, 1/5)) +
    ggplot2::scale_x_continuous(expression(lambda[standardized]),
                       expand = c(0, 0), limits = c(-1, 1),
                       breaks = c(-1, -0.7, -0.4, 0, 0.4, 0.7, 1),
                       labels = c("-1", "-.7", "-.4", "0", ".4", ".7", "1")) +
    ggplot2::scale_y_continuous(breaks = 1:65, sec.axis = ggplot2::sec_axis(~ . * 1, breaks = 1:65)) +
    ggplot2::ggtitle("") +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(size = 7),   # Tamaño de los números en el eje X
      axis.text.y = ggplot2::element_text(size = 7),   # Tamaño de los números en el eje Y
      axis.title.x = ggplot2::element_text(size = 12),  # Tamaño del título del eje X
      axis.title.y = ggplot2::element_text(size = 12),  # Tamaño del título del eje Y
      strip.text = ggplot2::element_text(size = 12)     # Tamaño de los títulos de los facetas
    ) +
    ggplot2::facet_wrap(~ factor)

  return(figure2)
}
