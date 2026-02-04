#' Extract Fit Indices from Factor Analysis
#'
#' Extracts fit indices (RMSEA, TLI, CFI, BIC) from psych factor analysis results.
#'
#' @param factors_data A psych factor analysis result object.
#'
#' @return A data frame with fit index names and values.
#'
#' @export
extract_fit <- function(factors_data) {
  # Verificar que el paquete requerido esté instalado
  if (!requireNamespace("psych", quietly = TRUE)) {
    stop("Package 'psych' is required but not installed. Please install it with install.packages('psych')")
  }

  fa.CFI <- function(x) {
    nombre <- paste(x, "CFI", sep = ".")
    nombre <- ((x$null.chisq - x$null.dof) - (x$STATISTIC - x$dof)) / (x$null.chisq - x$null.dof)
    return(nombre)
  }
  cfi <- fa.CFI(factors_data)

  result <- data.frame(
    Bondades = c("RMSEA", "lower_RMSEA", "upper_RMSEA", "confidence", "TLI", "CFI", "null.chisq", "objective", "BIC"),
    values = c(
      factors_data[["RMSEA"]],
      factors_data[["lower"]][1],
      factors_data[["upper"]][1],
      factors_data[["lower"]][2],
      factors_data[["TLI"]],
      cfi,
      factors_data[["null.chisq"]],
      factors_data[["objective"]],
      factors_data[["BIC"]]
    )
  )
  return(result)
}

