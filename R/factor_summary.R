#' Create Factor Analysis Summary
#'
#' Creates a summary table with loadings, communalities, and uniquenesses.
#'
#' @param factors_data A factor analysis result object with loadings.
#' @param num_items Number of items in the analysis.
#' @param num_factors Number of factors extracted.
#'
#' @return A data frame with items, factor loadings, h2, and u2.
#' @examples
#' \dontrun{
#' library(psych)
#'
#' # Create sample data
#' set.seed(123)
#' n <- 300
#' data <- data.frame(
#'   Item1 = sample(1:5, n, replace = TRUE),
#'   Item2 = sample(1:5, n, replace = TRUE),
#'   Item3 = sample(1:5, n, replace = TRUE),
#'   Item4 = sample(1:5, n, replace = TRUE),
#'   Item5 = sample(1:5, n, replace = TRUE),
#'   Item6 = sample(1:5, n, replace = TRUE)
#' )
#'
#' # Run EFA with psych package
#' efa_result <- fa(data, nfactors = 2, rotate = "oblimin", fm = "pa")
#'
#' # Create summary table
#' summary_table <- factor_summary(
#'   factors_data = efa_result,
#'   num_items = 6,
#'   num_factors = 2
#' )
#' print(summary_table)
#' # Output:
#' #   Items   F1    F2   h2   u2
#' # 1 Item1 0.75  0.10 0.58 0.42
#' # 2 Item2 0.68  0.05 0.47 0.53
#' # ...
#' }
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
