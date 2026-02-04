#' Extract Factor Items for Lavaan Syntax
#'
#' Creates lavaan model syntax by extracting items with non-zero loadings per factor.
#'
#' @param data Data frame with Items column and factor loading columns.
#' @param prefixes Vector of factor prefixes (e.g., c("f1", "f2")).
#'
#' @return A character string with lavaan model syntax.
#' @examples
#' \dontrun{
#' # Create a data frame with factor loadings (after EFA)
#' loadings_df <- data.frame(
#'   Items = c("Item1", "Item2", "Item3", "Item4", "Item5", "Item6"),
#'   f1 = c(0.75, 0.68, 0.72, 0, 0, 0),
#'   f2 = c(0, 0, 0, 0.70, 0.65, 0.78)
#' )
#'
#' # Extract lavaan syntax for factors f1 and f2
#' model_syntax <- extract_f_items(
#'   data = loadings_df,
#'   prefixes = c("f1", "f2")
#' )
#' cat(model_syntax)
#' # Output:
#' # f1 =~ Item1 + Item2 + Item3
#' # f2 =~ Item4 + Item5 + Item6
#'
#' # Use with lavaan
#' library(lavaan)
#' fit <- cfa(model_syntax, data = my_data, ordered = TRUE, estimator = "WLSMV")
#' }
#' @export
extract_f_items <- function(data, prefixes) {
  results <- character(length(prefixes))

  for (i in seq_along(prefixes)) {
    prefix <- prefixes[i]
    # Obtener las columnas que comienzan con el prefijo especificado
    selected_cols <- grep(paste0("^", prefix), names(data), value = TRUE)

    # Filtrar los elementos de las columnas seleccionadas que no son cero
    items <- data$Items[rowSums(data[selected_cols] != 0) > 0]

    # Generar la cadena final
    result <- paste(items, collapse = " + ")

    results[i] <- paste0(prefix, " =~ ", result)
  }

  return(paste(results, collapse = "\n"))
}
