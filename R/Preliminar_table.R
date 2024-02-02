Preliminar_table <- function(data) {
  library(dplyr)
  library(psych)
  data %>%
    psych::response.frequencies() %>%
    as_tibble() %>%
    dplyr::select(-miss) %>%
    dplyr::mutate(Items = c(1:3)) %>%
    relocate(Items) %>%
    dplyr::mutate(across(`1`:`5`, ~ .x * 100)) %>%
    dplyr::mutate(across(where(is.numeric), round, 2))
}
