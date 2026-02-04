#' Create Initial Model Configuration
#'
#' Creates an initial model configuration list for factor analysis.
#'
#' @param n_factors Number of factors.
#' @param n_items Number of items.
#' @param name_items Vector of item names or prefix.
#' @param exclude_items Optional vector of items to exclude.
#'
#' @return A list with model configuration parameters.
#'
#' @export
createInitialModel <- function(n_factors, n_items, name_items, exclude_items = NULL) {
  initialModel <- list(
    n_factors = n_factors,
    n_items = n_items,
    name_items = name_items,
    exclude_items = exclude_items
  )

  return(initialModel)
}
