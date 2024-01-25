extract_fit <- function(factors_data) {
  # Función para instalar y cargar librerías
  install_and_load <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }

  # Instalar y cargar la librería requerida
  install_and_load("psych")

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

