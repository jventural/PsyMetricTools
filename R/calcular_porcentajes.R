calcular_porcentajes <- function(data, columnas) {
  # Función para instalar y cargar librerías
  install_and_load <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }

  # Instalar y cargar las librerías requeridas
  install_and_load("tidyverse")

  resultados_lista <- map(columnas, function(col) {
    data %>%
      group_by(.data[[col]]) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(Porcentaje = n / sum(n) * 100)
  })

  resultados_lista <- setNames(resultados_lista, columnas)
  return(resultados_lista)
}
