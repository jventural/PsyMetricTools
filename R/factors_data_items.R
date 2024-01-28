factors_data_items <- function(summary_data, data_items, num_factors, apply_threshold = TRUE) {
  # Función para instalar y cargar librerías
  install_and_load <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }

  # Instalar y cargar las librerías requeridas
  install_and_load("dplyr")
  install_and_load("readxl")
  install_and_load("rlang")

  # Nombre de los items
  Items_name <- summary_data %>% dplyr::select(Items) %>% pull()

  # Seleccionar solo los items con los nombres previamente elegidos
  target <- data_items %>% filter(Items %in% Items_name)

  factor_names <- paste0("F", 1:num_factors)

  result <- inner_join(summary_data, target) %>%
    dplyr::select(-h2, -u2) %>%
    as_tibble()

  if (apply_threshold) {
    result <- result %>%
      mutate(across(all_of(factor_names), ~ case_when(. <= 0.30 ~ 0, TRUE ~ .)))
  }

  result <- result %>%
    arrange(across(all_of(factor_names), desc))

  return(result)
}


