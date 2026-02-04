#' @name factor_summary
#' @export
factor_summary <- function(factors_data, num_items, num_factors) {
  factor_names <- paste0("F", 1:num_factors)
  column_names <- c(factor_names, "h2", "u2")

  result <- data.frame((factors_data$loadings)[1:num_items,],
                       factors_data$communalities,
                       factors_data$uniquenesses) %>%
    stats::setNames(., column_names) %>%
    round(2) %>%
    tibble::rownames_to_column(var = "Items")

  return(result)
}
