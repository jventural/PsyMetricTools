#' Create Item Groups for Factor Analysis
#'
#' Creates a list of item index groups for factor definitions.
#'
#' @param names Character vector of group/factor names.
#' @param values Numeric vector with number of items per group.
#'
#' @return A named list with item indices for each group.
#' @examples
#' \dontrun{
#' # Create groups for a 3-factor model with different items per factor
#' groups <- create_groups(
#'   names = c("Anxiety", "Depression", "Stress"),
#'   values = c(5, 4, 6)  # 5 items for Anxiety, 4 for Depression, 6 for Stress
#' )
#'
#' # View the groups
#' groups
#' # $Anxiety
#' # [1] 1 2 3 4 5
#' # $Depression
#' # [1] 6 7 8 9
#' # $Stress
#' # [1] 10 11 12 13 14 15
#'
#' # Use with crear_modelo_lavaan
#' model <- crear_modelo_lavaan(
#'   nombre = "Item",
#'   Anxiety = groups$Anxiety,
#'   Depression = groups$Depression,
#'   Stress = groups$Stress
#' )
#' cat(model)
#'
#' # Example for a 2-factor scale
#' groups2 <- create_groups(
#'   names = c("Factor1", "Factor2"),
#'   values = c(8, 7)
#' )
#' }
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
