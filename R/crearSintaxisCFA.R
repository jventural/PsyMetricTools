#' @name crearSintaxisCFA
#' @export
crearSintaxisCFA <- function(df) {
  sintaxis_modelo <- ""

  # Verificar si la columna Factores existe en el dataframe
  if ("Factores" %in% names(df)) {
    # Identificar los factores únicos
    factores_unicos <- unique(df$Factores)

    # Iterar sobre cada factor único
    for (factor in factores_unicos) {
      # Filtrar los items que pertenecen a este factor
      items_del_factor <- df$Items[df$Factores == factor]

      # Crear la parte de la sintaxis del modelo para este factor
      sintaxis_factor <- paste0("F", which(factores_unicos == factor), " =~ ", paste(items_del_factor, collapse = " + "), "\n")

      # Agregar esta parte de la sintaxis al modelo completo
      sintaxis_modelo <- paste0(sintaxis_modelo, sintaxis_factor)
    }
  } else {
    # Identificar automáticamente los factores basados en las cargas más altas de los ítems
    cols_factores <- names(df)[grepl("^f[0-9]+$", names(df))]

    for (col in cols_factores) {
      items_del_factor <- df$Items[df[[col]] > 0]
      if (length(items_del_factor) > 0) {
        sintaxis_factor <- paste0(col, " =~ ", paste(items_del_factor, collapse = " + "), "\n")
        sintaxis_modelo <- paste0(sintaxis_modelo, sintaxis_factor)
      }
    }
  }

  # Retornar la sintaxis completa del modelo
  return(trimws(sintaxis_modelo)) # Eliminar espacios en blanco al final
}
