#' @name extract_f_items
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
