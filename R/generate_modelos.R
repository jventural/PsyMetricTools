#' Generate EFA Models for Multiple Factor Solutions
#'
#' Generates lavaan EFA model syntax for 1 to n_factors solutions.
#'
#' @param n_factors Maximum number of factors to test.
#' @param specific_items Optional vector of specific item names.
#' @param name_items Prefix for item names if specific_items is NULL.
#' @param n_items Number of items if specific_items is NULL.
#' @param exclude_items Optional vector of items to exclude.
#'
#' @return A list of model syntax strings for each factor solution.
#'
#' @export
generate_modelos <- function(n_factors, specific_items = NULL, name_items = NULL, n_items = NULL, exclude_items = NULL) {
  generate_models <- function(n_factors, specific_items, name_items, n_items, exclude_items) {
    # Si se proporcionan specific_items, se utilizan directamente. De lo contrario, se generan los nombres de los ítems.
    if (!is.null(specific_items)) {
      var_names <- specific_items
    } else {
      var_names <- paste0(name_items, 1:n_items)
    }

    if (!is.null(exclude_items)) {
      var_names <- setdiff(var_names, exclude_items)
    }

    var_sum <- paste(var_names, collapse = "+")

    models <- list()

    for (i in 1:n_factors) {
      factors <- paste0("efa(\"efa\")*f", 1:i, collapse = " +\n")
      model_formula <- paste0(factors, " =~\n", var_sum)
      models[[i]] <- model_formula
    }

    return(models)
  }

  models <- generate_models(n_factors, specific_items, name_items, n_items, exclude_items)
  return(models)
}
