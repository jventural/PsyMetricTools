#' Save Tables to Excel File
#'
#' Saves multiple data frames to an Excel file with separate sheets.
#'
#' @param file_name Path to the output Excel file.
#' @param ... Named data frames to save as sheets.
#' @param na.string String to use for NA values (default: "").
#'
#' @return Invisibly returns NULL. Writes Excel file to disk.
#'
#' @export
save_to_excel_table <- function(file_name, ..., na.string = "") {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required. Please install it.")
  }
  # Create Workbook object
  wb <- openxlsx::createWorkbook()

  # Extract additional arguments (summary tables)
  tables <- list(...)

  # Extract table names from arguments
  table_names <- names(tables)

  # If no names provided, assign default sheet names
  if (is.null(table_names) || any(table_names == "")) {
    table_names <- paste0("Sheet", seq_along(tables))
  }

  # Add each table as a different sheet
  for (i in seq_along(tables)) {
    openxlsx::addWorksheet(wb, table_names[i])
    openxlsx::writeData(wb, sheet = table_names[i], tables[[i]], na.string = na.string)
  }

  # Save workbook to Excel file
  openxlsx::saveWorkbook(wb, file = file_name, overwrite = TRUE)
}
