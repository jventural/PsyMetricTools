calculate_descriptives <- function(data, start_col, end_col) {
  # Función para instalar y cargar librerías
  install_and_load <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }

  # Instalar y cargar las librerías requeridas
  install_and_load("tidyverse")
  install_and_load("psych")

  # Cálculo de estadísticas descriptivas
  data %>%
    select(start_col:end_col) %>%
    psych::describe() %>%
    as.data.frame() %>%
    mutate("%" = mean/max*100) %>%
    select(mean, sd, min, max, skew, kurtosis, "%") %>%
    rename(Media = mean, DE = sd, Min. = min, Max. = max, g1 = skew, g2 = kurtosis) %>%
    round(2) %>%
    rownames_to_column(var = "Variables")
}

