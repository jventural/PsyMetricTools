generate_table <- function(summary_results) {
  rows <- list()

  # Orden requerido: Sexo, Edad, y luego el resto
  ordered_vars <- c("Sexo", "Edad", setdiff(names(summary_results), c("Sexo", "Edad")))

  for (var in ordered_vars) {
    if (var == "Edad") {
      mean_val <- round(as.numeric(summary_results[[var]]$mean_Edad), 2)
      sd_val   <- round(as.numeric(summary_results[[var]]$sd_Edad), 2)
      rows[[length(rows) + 1]] <- c("Edad (mean, SD)", paste0(mean_val, " (", sd_val, ")"), "")
    } else {
      # Título de bloque
      rows[[length(rows) + 1]] <- c(var, "", "")

      df_var <- summary_results[[var]]
      # Primera columna = categorías; segunda = n; tercera (Porcentaje) = ya ajustada
      for (i in seq_len(nrow(df_var))) {
        category <- as.character(df_var[[1]][i])
        count    <- as.character(df_var[["n"]][i])
        pct      <- as.character(round(as.numeric(df_var[["Porcentaje"]][i]), 2))  # sin símbolo
        rows[[length(rows) + 1]] <- c(category, count, pct)
      }
    }
  }

  table_matrix <- do.call(rbind, rows)
  colnames(table_matrix) <- c("Variables sociodemográficas", "n", "%")
  df_table <- as.data.frame(table_matrix, stringsAsFactors = FALSE)

  # (Opcional) convertir n y % a numéricas manteniendo las filas-título vacías:
  suppressWarnings(df_table[["n"]] <- as.numeric(df_table[["n"]]))
  suppressWarnings(df_table[["%"]] <- as.numeric(df_table[["%"]]))

  df_table
}
