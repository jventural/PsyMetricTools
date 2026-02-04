#' @title Preliminary Response Frequency Table
#' @description Creates a table of response frequencies for items.
#' @param data Data frame containing the items.
#' @return A tibble with response frequencies as percentages.
#' @examples
#' \dontrun{
#' # Create sample Likert data (1-5 scale)
#' set.seed(123)
#' data <- data.frame(
#'   Item1 = sample(1:5, 100, replace = TRUE),
#'   Item2 = sample(1:5, 100, replace = TRUE),
#'   Item3 = sample(1:5, 100, replace = TRUE)
#' )
#'
#' # Get response frequency table
#' freq_table <- Preliminar_table(data)
#' print(freq_table)
#' # Output shows percentage of responses for each response option (1-5)
#' # Items   1     2     3     4     5
#' # 1      20.0  18.0  22.0  21.0  19.0
#' # 2      19.0  21.0  20.0  22.0  18.0
#' # 3      21.0  19.0  18.0  20.0  22.0
#' }
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
