generate_table <- function(summary_results) {
  # Inicializar un vector vacío para almacenar las filas de la tabla
  table_rows <- c()

  # Definir el orden de las variables, con Sexo y Edad primero
  ordered_vars <- c("Sexo", "Edad", setdiff(names(summary_results), c("Sexo", "Edad")))

  # Procesar cada elemento en summary_results según el orden definido
  for (var in ordered_vars) {
    if (var == "Edad") {
      # Agregar la fila de Edad (media y SD)
      mean_val <- round(summary_results[[var]]$mean_Edad, 2)
      sd_val   <- round(summary_results[[var]]$sd_Edad, 2)
      table_rows <- c(
        table_rows,
        "Edad (mean, SD)",
        paste0(mean_val, " (", sd_val, ")"),
        ""  # sin porcentaje para Edad
      )
    } else {
      # Agregar el nombre de la variable como título
      table_rows <- c(table_rows, var, "", "")

      # Agregar las categorías, los conteos y los porcentajes sin '%'
      for (i in seq_len(nrow(summary_results[[var]]))) {
        category   <- summary_results[[var]][i, 1]
        count      <- summary_results[[var]][i, 2]
        percentage <- round(summary_results[[var]][i, 3], 2)
        table_rows <- c(
          table_rows,
          category,
          count,
          as.character(percentage)  # solo el número
        )
      }
    }
  }

  # Convertir las filas en una matriz de 3 columnas y luego a data.frame
  table_matrix <- matrix(table_rows, ncol = 3, byrow = TRUE)
  colnames(table_matrix) <- c("Variables sociodemográficas", "n", "%")

  df_table <- as.data.frame(table_matrix, stringsAsFactors = FALSE)

  # (Opcional) Si quieres que la columna '%' sea numérica:
  # df_table[["%"]] <- as.numeric(df_table[["%"]])

  return(df_table)
}
