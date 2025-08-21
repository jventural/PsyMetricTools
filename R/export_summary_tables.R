export_summary_tables <- function(summary_Tabla_Total,
                                  summary_Tabla_EFA,
                                  summary_Tabla_CFA,
                                  file_name = "Tablas_Resumenes.xlsx") {
  library(dplyr); library(openxlsx)

  df_total <- summary_Tabla_Total %>%
    dplyr::rename(Total_n   = n,  Total_pct = `%`)
  df_efa <- summary_Tabla_EFA %>%
    dplyr::rename(EFA_n     = n,  EFA_pct   = `%`)
  df_cfa <- summary_Tabla_CFA %>%
    dplyr::rename(CFA_n     = n,  CFA_pct   = `%`)

  df_merged <- df_total %>%
    dplyr::full_join(df_efa, by = "Variables sociodemográficas") %>%
    dplyr::full_join(df_cfa, by = "Variables sociodemográficas")

  df_merged[is.na(df_merged)] <- ""

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Resumen_Tablas", gridLines = TRUE)

  header1 <- c("", "Total", "", "EFA", "", "CFA", "")
  openxlsx::writeData(wb, "Resumen_Tablas", t(header1), startRow = 1, startCol = 1,
                      colNames = FALSE, rowNames = FALSE)
  openxlsx::mergeCells(wb, "Resumen_Tablas", cols = 2:3, rows = 1)
  openxlsx::mergeCells(wb, "Resumen_Tablas", cols = 4:5, rows = 1)
  openxlsx::mergeCells(wb, "Resumen_Tablas", cols = 6:7, rows = 1)

  header2 <- c("Variables sociodemográficas", "n", "%", "n", "%", "n", "%")
  openxlsx::writeData(wb, "Resumen_Tablas", t(header2), startRow = 2, startCol = 1,
                      colNames = FALSE, rowNames = FALSE)

  openxlsx::writeData(wb, "Resumen_Tablas", df_merged, startRow = 3, startCol = 1,
                      colNames = FALSE, rowNames = FALSE)

  openxlsx::setColWidths(wb, "Resumen_Tablas", cols = 1, widths = 30)
  openxlsx::setColWidths(wb, "Resumen_Tablas", cols = 2:7, widths = 12)

  openxlsx::saveWorkbook(wb, file_name, overwrite = TRUE)
  invisible(df_merged)
}
