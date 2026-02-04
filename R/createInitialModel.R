#' @name createInitialModel
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
