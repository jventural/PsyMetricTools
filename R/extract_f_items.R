#' Extract Factor Items for Lavaan Syntax
#'
#' Creates lavaan model syntax by extracting items with non-zero loadings per factor.
#'
#' @param data Data frame with Items column and factor loading columns.
#' @param prefixes Vector of factor prefixes (e.g., c("f1", "f2")).
#'
#' @return A character string with lavaan model syntax.
#'
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
