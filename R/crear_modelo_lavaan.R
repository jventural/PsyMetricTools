#' Create Lavaan Model Syntax
#'
#' Creates lavaan model syntax from item prefix and factor specifications.
#'
#' @param nombre Prefix for item names.
#' @param ... Named numeric vectors specifying item indices for each factor.
#'
#' @return A character string with lavaan model syntax.
#' @examples
#' \dontrun{
#' # Create a 3-factor CFA model with item prefix "Item"
#' model <- crear_modelo_lavaan(
#'   nombre = "Item",
#'   F1 = 1:4,      # Items 1-4 load on Factor 1
#'   F2 = 5:8,      # Items 5-8 load on Factor 2
#'   F3 = 9:12      # Items 9-12 load on Factor 3
#' )
#' cat(model)
#' # Output:
#' # F1 =~ Item1 + Item2 + Item3 + Item4
#' # F2 =~ Item5 + Item6 + Item7 + Item8
#' # F3 =~ Item9 + Item10 + Item11 + Item12
#'
#' # Create a 2-factor model with non-consecutive items
#' model2 <- crear_modelo_lavaan(
#'   nombre = "Q",
#'   Anxiety = c(1, 3, 5, 7),
#'   Depression = c(2, 4, 6, 8)
#' )
#' cat(model2)
#'
#' # Use with lavaan
#' library(lavaan)
#' fit <- cfa(model, data = my_data, ordered = TRUE, estimator = "WLSMV")
#' }
#' @export
crear_modelo_lavaan <- function(nombre, ...) {
  argumentos <- list(...)

  variables <- names(argumentos)

  modelo <- character(length(variables))

  for (i in seq_along(variables)) {
    variable <- variables[i]
    indices <- argumentos[[variable]]
    items <- paste0(nombre, indices)
    modelo[i] <- paste0(variable, " =~ ", paste(items, collapse = " + "))
  }

  return(paste(modelo, collapse = "\n"))
}
