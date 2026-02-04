\name{export_summary_tables}
\alias{export_summary_tables}
\title{Save Multiple Summary Tables to a Single Excel Sheet Horizontally}

\description{
The `export_summary_tables` function exports multiple summary tables (e.g., `summary_Tabla_Total`, `summary_Tabla_EFA`, and `summary_Tabla_CFA`) to a single Excel sheet, aligning the tables horizontally. This is useful for users who want to consolidate multiple summary tables into one cohesive sheet for easy comparison and streamlined data presentation.
}

\usage{
export_summary_tables(summary_Tabla_Total, summary_Tabla_EFA,
                      summary_Tabla_CFA,
                      file_name = "Tablas_Resumenes.xlsx")
}

\arguments{
  \item{summary_Tabla_Total}{A dataframe containing the first summary table (e.g., demographic or statistical data) to be written to the Excel sheet, positioned on the left.}
  \item{summary_Tabla_EFA}{A dataframe containing the second summary table to be written to the Excel sheet, positioned in the center.}
  \item{summary_Tabla_CFA}{A dataframe containing the third summary table to be written to the Excel sheet, positioned on the right.}
  \item{file_name}{A character string specifying the name of the Excel file to be created, including the \code{.xlsx} extension (e.g., \code{"Tablas_Resumenes.xlsx"}). The default is \code{"Tablas_Resumenes.xlsx"}.}
}

\details{
The `export_summary_tables` function leverages the \code{openxlsx} package to create a new Excel workbook and adds a single sheet named "Resumen_Tablas". It writes the three provided summary tables into this sheet, placing them side by side with customizable spacing.

The function first replaces any \code{NA} values in the tables with empty strings to prevent errors during the export process. It then positions each table horizontally using the \code{writeData()} function, ensuring that the tables do not overlap. The resulting Excel file allows users to view all summary tables in a single sheet for efficient comparison and reporting.
}

\value{
An Excel file with a single sheet containing the three summary tables aligned horizontally.
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

# Save the tables to a single Excel sheet
export_summary_tables(
  summary_Tabla_Total = summary_Tabla_Total,
  summary_Tabla_EFA = summary_Tabla_EFA,
  summary_Tabla_CFA = summary_Tabla_CFA,
  file_name = "Output/Tablas_Resumenes.xlsx"
)
}

\author{
Dr. José Ventura-León
}
