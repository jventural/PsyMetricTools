crearSintaxisCFA <- function(df) {
  # Identificar los factores únicos
  factores_unicos <- unique(df$Factores)

  # Inicializar una cadena para almacenar la sintaxis completa del modelo
  sintaxis_modelo <- ""

  # Iterar sobre cada factor único
  for (factor in factores_unicos) {
    # Filtrar los items que pertenecen a este factor
    items_del_factor <- df$Items[df$Factores == factor]

    # Crear la parte de la sintaxis del modelo para este factor
    # Ejemplo: F1 =~ item1 + item2 + item3
    sintaxis_factor <- paste0("F", which(factores_unicos == factor), " =~ ", paste(items_del_factor, collapse = " + "), "\n")

    # Agregar esta parte de la sintaxis al modelo completo
    sintaxis_modelo <- paste0(sintaxis_modelo, sintaxis_factor)
  }

  # Retornar la sintaxis completa del modelo
  trimws(sintaxis_modelo) # Eliminar espacios en blanco al final
}
