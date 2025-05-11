export_summary_tables <- function(summary_Tabla_Total,
                                  summary_Tabla_EFA,
                                  summary_Tabla_CFA,
                                  file_name = "Tablas_Resumenes.xlsx") {
  library(dplyr)
  library(openxlsx)

  # 1. Renombrar columnas para evitar choques
  df_total <- summary_Tabla_Total %>%
    rename(Total_n   = n,
           Total_pct = `%`)
  df_efa <- summary_Tabla_EFA %>%
    rename(EFA_n   = n,
           EFA_pct = `%`)
  df_cfa <- summary_Tabla_CFA %>%
    rename(CFA_n   = n,
           CFA_pct = `%`)

  # 2. Unirlas por la primera columna
  df_merged <- df_total %>%
    full_join(df_efa, by = "Variables sociodemográficas") %>%
    full_join(df_cfa, by = "Variables sociodemográficas")

  # 3. Opcional: sustituir NA por ""
  df_merged[is.na(df_merged)] <- ""

  # 4. Crear workbook y hoja
  wb <- createWorkbook()
  addWorksheet(wb, "Resumen_Tablas", gridLines = TRUE)

  # 5. Fila 1 de encabezado: fusión de columnas
  #    - Columna 1 queda vacía (título de la variable ya irá en la fila 2)
  header1 <- c("", "Total", "", "EFA", "", "CFA", "")
  writeData(wb, "Resumen_Tablas", t(header1),
            startRow = 1, startCol = 1,
            colNames = FALSE, rowNames = FALSE)

  # Fusionar celdas para cada bloque de dos columnas
  mergeCells(wb, "Resumen_Tablas", cols = 2:3, rows = 1)  # Total
  mergeCells(wb, "Resumen_Tablas", cols = 4:5, rows = 1)  # EFA
  mergeCells(wb, "Resumen_Tablas", cols = 6:7, rows = 1)  # CFA

  # 6. Fila 2 de encabezado: nombres genéricos
  header2 <- c("Variables sociodemográficas",
               "n", "%",   # bajo Total
               "n", "%",   # bajo EFA
               "n", "%")   # bajo CFA
  writeData(wb, "Resumen_Tablas", t(header2),
            startRow = 2, startCol = 1,
            colNames = FALSE, rowNames = FALSE)

  # 7. Escribir los datos en la fila 3 hacia abajo
  writeData(wb, "Resumen_Tablas", df_merged,
            startRow = 3, startCol = 1,
            colNames = FALSE, rowNames = FALSE)

  # 8. Ajustar anchos de columna (opcional)
  setColWidths(wb, "Resumen_Tablas", cols = 1, widths = 30)
  setColWidths(wb, "Resumen_Tablas", cols = 2:7, widths = 12)

  # 9. Guardar
  saveWorkbook(wb, file_name, overwrite = TRUE)
  cat("Archivo guardado como", file_name, "\n")

  # Devolver la tabla combinada si quieres inspeccionarla
  invisible(df_merged)
}
