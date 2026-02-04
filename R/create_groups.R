#' Create Item Groups for Factor Analysis
#'
#' Creates a list of item index groups for factor definitions.
#'
#' @param names Character vector of group/factor names.
#' @param values Numeric vector with number of items per group.
#'
#' @return A named list with item indices for each group.
#'
#' @export
create_groups <- function(names, values) {
  if(length(names) != length(values)) {
    stop("The length of names and values must be the same.")
  }

  groups_list <- list()
  start <- 1
  for(i in seq_along(names)) {
    end <- start + values[i] - 1
    groups_list[[names[i]]] <- seq(start, end)
    start <- end + 1
  }

  groups <- structure(groups_list, Names = names)
  return(groups)
}
