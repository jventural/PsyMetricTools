#' Generate Lavaan Model with Thresholds
#'
#' Generates complete lavaan model syntax including factor loadings and item thresholds.
#'
#' @param num_items Number of items in the model.
#' @param item_value Vector of loading values for each item.
#' @param t_values Vector of threshold values.
#' @param excluded_items Items to exclude (commented out), or "none".
#'
#' @return A character string with complete lavaan model syntax.
#' @examples
#' \dontrun{
#' # Generate a simple 1-factor model with 5 items and 4 thresholds
#' model <- generate_model_lavaan(
#'   num_items = 5,
#'   item_value = c(0.7, 0.8, 0.75, 0.6, 0.65),  # Factor loadings
#'   t_values = c(-1.5, -0.5, 0.5, 1.5)           # Threshold values
#' )
#' cat(model)
#' # Output:
#' # F1 =~ 0.7*Item1 + 0.8*Item2 + 0.75*Item3 + 0.6*Item4 + 0.65*Item5
#' # F1 ~~ 1*F1
#' # Item1 | -1.5*t1
#' # Item1 | -0.5*t2
#' # ...
#'
#' # Generate model with some items excluded (commented out)
#' model2 <- generate_model_lavaan(
#'   num_items = 5,
#'   item_value = c(0.7, 0.8, 0.75, 0.6, 0.65),
#'   t_values = c(-1.5, -0.5, 0.5, 1.5),
#'   excluded_items = c("Item3t1", "Item3t2")  # Exclude Item3 thresholds 1 and 2
#' )
#' cat(model2)
#' }
#' @export
generate_model_lavaan <- function(num_items,
                          item_value,
                          t_values,
                          excluded_items = "none") {
  # Crear nombres de ítems
  item_names <- paste0("Item", 1:num_items)

  # Iniciar la ecuación de análisis factorial
  text <- paste0("F1 =~ ", item_value[1], "*", item_names[1])

  # Agregar los ítems restantes a la ecuación
  for (i in 2:num_items) {
    text <- paste0(text, " + ", item_value[i], "*", item_names[i])
  }

  text <- paste0(text, "\n\nF1 ~~ 1*F1\n\n")

  # Añadir umbrales para cada ítem
  for (i in 1:num_items) {
    for (j in 1:length(t_values)) {
      line <- paste0(item_names[i], " | ", t_values[j], "*t", j)
      item_threshold_key <- paste("Item", i, "t", j, sep = "")

      # Si el ítem está excluido, comentar la línea
      if (excluded_items != "none" && item_threshold_key %in% excluded_items) {
        line <- paste0("# ", line)
      }
      text <- paste0(text, line, "\n")
    }
    text <- paste0(text, "\n")
  }
  return(text)
}
