#' Extract Factor Correlations from EFA
#'
#' Extracts the inter-factor correlation matrix (Phi) from EFA results.
#'
#' @param x An EFA result object containing factor correlations.
#'
#' @return A matrix of factor correlations rounded to 2 decimals.
#'
#' @export
cor_afe <- function(x) {
  factors_data$Phi %>% round(2)
}
