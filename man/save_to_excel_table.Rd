\name{save_to_excel_table}
\alias{save_to_excel_table}
\title{Save Multiple Summary Tables to a Single Excel File with Multiple Sheets}

\description{
The save_to_excel_table function saves multiple summary tables into a single Excel file, with each table stored in a separate sheet.
}

\usage{
save_to_excel_table(file_name, ..., na.string = "")
}

\arguments{
  \item{file_name}{A character string specifying the name of the Excel file to be created.}
  \item{...}{One or more dataframes representing the summary tables to be saved in the Excel file.}
  \item{na.string}{Character string to use for missing values (default "").}
}

\value{
An Excel file saved at the specified location, containing each summary table in a separate sheet.
}

\examples{
\dontrun{
summary_Tabla_Total <- data.frame(Variables = c("M", "F"), n = c(30, 70))
save_to_excel_table(file_name = "test.xlsx", summary_Tabla_Total = summary_Tabla_Total)
}
}

\author{
Dr. Jose Ventura-Leon
}

