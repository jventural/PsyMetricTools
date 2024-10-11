\name{save_to_excel_table}
\alias{save_to_excel_table}
\title{Save Multiple Summary Tables to a Single Excel File with Multiple Sheets}

\description{
The `save_to_excel_table` function saves multiple summary tables into a single Excel file, with each table stored in a separate sheet. It automatically names each sheet based on the provided table names or assigns default names if none are supplied. This function is designed for users who need to consolidate multiple tables into a single file for streamlined reporting and data management.
}

\usage{
save_to_excel_table(file_name, ...)
}

\arguments{
  \item{file_name}{A character string specifying the name of the Excel file to be created, including the \code{.xlsx} extension (e.g., \code{"SummaryTables.xlsx"}).}
  \item{...}{One or more dataframes representing the summary tables to be saved in the Excel file. Each table will be written to a separate sheet in the workbook. The names of the arguments will be used as the sheet names, if provided.}
}

\details{
The `save_to_excel_table` function first checks whether the \code{openxlsx} package is installed. If the package is not available, it installs it automatically. Next, it creates a new workbook using the \code{createWorkbook()} function from the \code{openxlsx} package.

The function then extracts the list of summary tables passed as arguments and retrieves their names. If no names are provided or if some names are empty, it assigns default sheet names in the format \code{"Sheet1"}, \code{"Sheet2"}, etc.

Each table is added to the workbook as a separate sheet using the \code{addWorksheet()} and \code{writeData()} functions. The final workbook is saved using the specified \code{file_name}, with the option to overwrite any existing file with the same name.
}

\value{
An Excel file saved at the specified location, containing each summary table in a separate sheet. The output file structure allows for clear organization and easy navigation between different summary tables.
}

\examples{
# Example usage with sample summary tables

# Create sample tables
summary_Tabla_Total <- data.frame(
  Variables = c("M", "F"),
  n = c(30, 70),
  Porcentaje = c(30, 70)
)

summary_Tabla_EFA <- data.frame(
  Variables = c("Urban", "Rural"),
  n = c(60, 40),
  Porcentaje = c(60, 40)
)

summary_Tabla_CFA <- data.frame(
  Variables = c("Public", "Private"),
  n = c(55, 45),
  Porcentaje = c(55, 45)
)

# Save the tables to a single Excel file with three sheets
save_to_excel_table(
  file_name = "Output/SummaryTables.xlsx",
  summary_Tabla_Total = summary_Tabla_Total,
  summary_Tabla_EFA = summary_Tabla_EFA,
  summary_Tabla_CFA = summary_Tabla_CFA
)
}

\author{
Dr. José Ventura-León
}
