save_to_excel_table <- function(file_name, ...) {
  if (!require(openxlsx)) install.packages("openxlsx")
  # Crear un objeto Workbook
  wb <- createWorkbook()

  # Extraer los argumentos adicionales (las tablas de resumen)
  tables <- list(...)

  # Extraer los nombres de las tablas proporcionadas como argumentos
  table_names <- names(tables)

  # Si no se proporcionaron nombres, asignar nombres predeterminados a las hojas
  if (is.null(table_names) || any(table_names == "")) {
    table_names <- paste0("Sheet", seq_along(tables))
  }

  # Añadir cada tabla como una hoja diferente
  for (i in seq_along(tables)) {
    addWorksheet(wb, table_names[i])
    writeData(wb, sheet = table_names[i], tables[[i]])
  }

  # Guardar el workbook en un archivo Excel
  saveWorkbook(wb, file = file_name, overwrite = TRUE)
}
