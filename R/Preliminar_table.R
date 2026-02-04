#' @title Preliminary Response Frequency Table
#' @description Creates a table of response frequencies for items.
#' @param data Data frame containing the items.
#' @return A tibble with response frequencies as percentages.
#' @export
Preliminar_table <- function(data) {
  data %>%
    psych::response.frequencies() %>%
    tibble::as_tibble() %>%
    dplyr::select(-miss) %>%
    dplyr::mutate(Items = c(1:3)) %>%
    dplyr::relocate(Items) %>%
    dplyr::mutate(dplyr::across(`1`:`5`, ~ .x * 100)) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), round, 2))
}
