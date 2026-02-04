#' Extract Factor Correlations from EFA
#'
#' Extracts the inter-factor correlation matrix (Phi) from EFA results.
#'
#' @param x An EFA result object containing factor correlations.
#'
#' @return A matrix of factor correlations rounded to 2 decimals.
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
#' # Extract factor correlations
#' factor_cors <- cor_afe(efa_result)
#' print(factor_cors)
#' # Output: 2x2 matrix of factor correlations
#' }
#' @export
cor_afe <- function(x) {
  x$Phi %>% round(2)
}
