#' @name crear_modelo_lavaan
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
