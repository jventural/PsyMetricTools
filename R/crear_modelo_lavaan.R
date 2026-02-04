#' Create Lavaan Model Syntax
#'
#' Creates lavaan model syntax from item prefix and factor specifications.
#'
#' @param nombre Prefix for item names.
#' @param ... Named numeric vectors specifying item indices for each factor.
#'
#' @return A character string with lavaan model syntax.
#'
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
