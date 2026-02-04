#' @title Standardized Factor Solutions for EFA
#' @description Extracts and formats standardized factor loadings from lavaan EFA.
#' @param specification A lavaan model object.
#' @param name_items Prefix for item names.
#' @param apply_threshold Logical, whether to apply 0.30 threshold (default TRUE).
#' @return A data frame with items and factor loadings.
#' @examples
#' \dontrun{
#' # First run EFA_modern to get the specification object
#' set.seed(123)
#' n <- 300
#' data_efa <- data.frame(
#'   Item1 = sample(1:5, n, replace = TRUE),
#'   Item2 = sample(1:5, n, replace = TRUE),
#'   Item3 = sample(1:5, n, replace = TRUE),
#'   Item4 = sample(1:5, n, replace = TRUE),
#'   Item5 = sample(1:5, n, replace = TRUE),
#'   Item6 = sample(1:5, n, replace = TRUE),
#'   Item7 = sample(1:5, n, replace = TRUE),
#'   Item8 = sample(1:5, n, replace = TRUE),
#'   Item9 = sample(1:5, n, replace = TRUE)
#' )
#'
#' # Run EFA
#' efa_result <- EFA_modern(
#'   n_factors = 3,
#'   n_items = 9,
#'   name_items = "Item",
#'   data = data_efa,
#'   apply_threshold = TRUE
#' )
#'
#' # Extract standardized solutions from the 3-factor model
#' loadings <- Standardized_solutions(
#'   specification = efa_result$Specifications[[3]],
#'   name_items = "Item",
#'   apply_threshold = TRUE
#' )
#' print(loadings)
#'
#' # Without threshold (show all loadings)
#' loadings_full <- Standardized_solutions(
#'   specification = efa_result$Specifications[[3]],
#'   name_items = "Item",
#'   apply_threshold = FALSE
#' )
#' print(loadings_full)
#' }
#' @export
Standardized_solutions <- function(specification, name_items, apply_threshold = TRUE) {
  # Check for required packages
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package 'tidyr' is required but not installed.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package 'stringr' is required but not installed.")
  }

  # Procesamiento de los datos
  result <- lavaan::standardizedsolution(specification) %>%
    dplyr::filter(op == "=~") %>%
    dplyr::mutate(item = stringr::str_remove(rhs, name_items) %>% as.double(),
           factor = stringr::str_remove(lhs, "f")) %>%
    dplyr::select(lhs, rhs, est.std) %>%
    tidyr::pivot_wider(names_from = lhs, values_from = c(est.std))

  # Aplicar el umbral
  if (apply_threshold) {
    result <- result %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("f"), ~ ifelse(abs(.) > 0.30, ., 0)))
  }

  # Identificar el factor principal para cada item
  # FIX: Usar which.max() en lugar de which(..., arr.ind=TRUE) para soportar 1 factor
  # El problema anterior: which() con arr.ind=TRUE retorna vector (no matriz) cuando hay 1 sola columna
  result <- result %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      max_factor_index = which.max(abs(dplyr::c_across(dplyr::starts_with("f")))),
      max_factor_value = max(abs(dplyr::c_across(dplyr::starts_with("f"))), na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  # Ordenar por el indice del factor principal y luego por el valor absoluto de la carga
  result <- result %>%
    dplyr::arrange(max_factor_index, desc(abs(max_factor_value))) %>%
    dplyr::select(-max_factor_index, -max_factor_value) # Limpiar las columnas temporales

  # Renombrar para la salida final
  result <- result %>%
    dplyr::rename(Items = rhs)

  return(result)
}

