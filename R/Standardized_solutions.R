Standardized_solutions <- function(specification, name_items, apply_threshold = TRUE) {
  # Función para instalar y cargar librerías
  install_and_load <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }

  # Instalar y cargar las librerías requeridas
  install_and_load("tidyr")
  install_and_load("dplyr")
  install_and_load("stringr")

  # Procesamiento de los datos
  result <- standardizedsolution(specification) %>%
    filter(op == "=~") %>%
    mutate(item = str_remove(rhs, name_items) %>% as.double(),
           factor = str_remove(lhs, "f")) %>%
    dplyr::select(lhs, rhs, est.std) %>%
    pivot_wider(names_from = lhs, values_from = c(est.std))

  # Aplicar el umbral
  if (apply_threshold) {
    result <- result %>%
      mutate(across(starts_with("f"), ~ ifelse(abs(.) > 0.30, ., 0)))
  }

  # Identificar el factor principal para cada ítem
  result <- result %>%
    rowwise() %>%
    mutate(max_factor = list(which(abs(c_across(starts_with("f"))) == max(abs(c_across(starts_with("f"))), na.rm = TRUE), arr.ind = TRUE))) %>%
    ungroup()

  # Extraer el índice del factor principal y su valor
  result$max_factor_index <- sapply(result$max_factor, function(x) if (length(x) > 0) x[1] else NA)
  result$max_factor_value <- sapply(result$max_factor, function(x) if (length(x) > 0) x[2] else NA)

  # Ordenar por el índice del factor principal y luego por el valor absoluto de la carga
  result <- result %>%
    arrange(max_factor_index, desc(abs(max_factor_value))) %>%
    dplyr::select(-max_factor, -max_factor_index, -max_factor_value) # Limpiar las columnas temporales

  # Renombrar para la salida final
  result <- result %>%
    rename(Items = rhs)

  return(result)
}


