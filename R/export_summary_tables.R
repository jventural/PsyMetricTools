export_summary_tables <- function(summary_Tabla_Total, summary_Tabla_EFA, summary_Tabla_CFA, file_name = "Tablas_Resumenes.xlsx") {
  library(openxlsx)
  # Reemplazar los valores NA por una cadena vacía
  summary_Tabla_Total[is.na(summary_Tabla_Total)] <- ""
  summary_Tabla_EFA[is.na(summary_Tabla_EFA)] <- ""
  summary_Tabla_CFA[is.na(summary_Tabla_CFA)] <- ""

  # Crear un archivo de Excel nuevo
  wb <- createWorkbook()

  # Añadir una hoja al archivo de Excel
  addWorksheet(wb, "Resumen_Tablas")

  # Escribir cada tabla en su respectiva posición
  writeData(wb, "Resumen_Tablas", summary_Tabla_Total, startCol = 1, startRow = 1)
  writeData(wb, "Resumen_Tablas", summary_Tabla_EFA, startCol = ncol(summary_Tabla_Total) + 3, startRow = 1)
  writeData(wb, "Resumen_Tablas", summary_Tabla_CFA, startCol = ncol(summary_Tabla_Total) + ncol(summary_Tabla_EFA) + 6, startRow = 1)

  # Guardar el archivo
  saveWorkbook(wb, file_name, overwrite = TRUE)

  cat("Archivo guardado como", file_name, "\n")
}
