#' @name factors_data_items
#' @export
factors_data_items <- function(summary_data, data_items, num_factors, apply_threshold = TRUE) {
  # Nombre de los items
  Items_name <- summary_data %>% dplyr::select(Items) %>% dplyr::pull()

  # Seleccionar solo los items con los nombres previamente elegidos
  target <- data_items %>% dplyr::filter(Items %in% Items_name)

  factor_names <- paste0("F", 1:num_factors)

  result <- dplyr::inner_join(summary_data, target) %>%
    dplyr::select(-h2, -u2) %>%
    tibble::as_tibble()

  if (apply_threshold) {
    result <- result %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(factor_names), ~ dplyr::case_when(. <= 0.30 ~ 0, TRUE ~ .)))
  }

  result <- result %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(factor_names), desc))

  return(result)
}


