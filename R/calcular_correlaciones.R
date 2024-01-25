calcular_correlaciones <- function(data, columna_inicial, columna_final, method = c("spearman", "pearson"), winsorize = FALSE) {

  # Función para instalar y cargar librerías
  install_and_load <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }

  # Instalar y cargar las librerías requeridas
  install_and_load("dplyr")
  install_and_load("WRS2")

  # Asegurar que method sea válido
  method <- match.arg(method)

  # Seleccionar las columnas
  column_range <- which(names(data) %in% c(columna_inicial, columna_final))
  data <- data[, min(column_range):max(column_range)]

  if (winsorize && method == "pearson") {
    # Calcular la correlación de Pearson winsorizado
    result <- WRS2::winall(data, tr = 0.2)$cor %>%
      round(2) %>%
      diag_aba_na() %>%
      as.data.frame()
  } else {
    # Calcular la correlación normal
    result <- data %>%
      cor(method = method) %>%
      round(2) %>%
      diag_aba_na() %>%
      as.data.frame()
  }

  return(result)
}

# Función auxiliar para manejar NA en la diagonal
diag_aba_na <- function(mat) {
  diag(mat) <- NA
  return(mat)
}
