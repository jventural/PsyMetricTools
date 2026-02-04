#' Summarize Column Statistics
#'
#' Calculates descriptive statistics for a column, excluding zero values.
#'
#' @param data Data frame containing the column.
#' @param col_name Name of the column to summarize.
#'
#' @return A data frame with mean, SD, min, and max values.
#'
#' @export
summarise_column <- function(data, col_name) {
  data %>%
    filter(!!sym(col_name) != 0) %>%
    summarise(M = mean(!!sym(col_name), na.rm = TRUE),
              sd = sd(!!sym(col_name), na.rm = TRUE),
              min = min(!!sym(col_name), na.rm = TRUE),
              max = max(!!sym(col_name), na.rm = TRUE),
              .groups = 'drop') %>%
    mutate(column = col_name) %>%
    select(column, everything())
}
