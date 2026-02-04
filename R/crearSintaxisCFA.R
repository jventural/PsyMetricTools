#' Create CFA Syntax from Data Frame
#'
#' Generates lavaan CFA model syntax from a data frame with items and factors.
#'
#' @param df A data frame with Items and Factores columns, or factor loading columns (f1, f2, etc.).
#'
#' @return A character string with lavaan model syntax.
#' @examples
#' \dontrun{
#' # Example 1: Create CFA syntax from EFA results with factor loadings
#' # Assume you have EFA results with items and their factor loadings
#' efa_loadings <- data.frame(
#'   Items = c("Item1", "Item2", "Item3", "Item4", "Item5", "Item6"),
#'   f1 = c(0.75, 0.68, 0.72, 0, 0, 0),
#'   f2 = c(0, 0, 0, 0.70, 0.65, 0.78)
#' )
#'
#' # Generate CFA syntax
#' model <- crearSintaxisCFA(efa_loadings)
#' cat(model)
#' # Output:
#' # f1 =~ Item1 + Item2 + Item3
#' # f2 =~ Item4 + Item5 + Item6
#'
#' # Example 2: Create CFA syntax from a data frame with explicit factor assignments
#' factor_assignments <- data.frame(
#'   Items = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6"),
#'   Factores = c("Anxiety", "Anxiety", "Anxiety", "Depression", "Depression", "Depression")
#' )
#'
#' model2 <- crearSintaxisCFA(factor_assignments)
#' cat(model2)
#' # Output:
#' # F1 =~ Q1 + Q2 + Q3
#' # F2 =~ Q4 + Q5 + Q6
#'
#' # Use generated syntax with lavaan
#' library(lavaan)
#' fit <- cfa(model, data = my_data, ordered = TRUE, estimator = "WLSMV")
#' }
#' @export
crearSintaxisCFA <- function(df) {
  sintaxis_modelo <- ""

  # Verificar si la columna Factores existe en el dataframe
  if ("Factores" %in% names(df)) {
    # Identificar los factores únicos
    factores_unicos <- unique(df$Factores)

    # Iterar sobre cada factor único
    for (factor in factores_unicos) {
      # Filtrar los items que pertenecen a este factor
      items_del_factor <- df$Items[df$Factores == factor]

      # Crear la parte de la sintaxis del modelo para este factor
      sintaxis_factor <- paste0("F", which(factores_unicos == factor), " =~ ", paste(items_del_factor, collapse = " + "), "\n")

      # Agregar esta parte de la sintaxis al modelo completo
      sintaxis_modelo <- paste0(sintaxis_modelo, sintaxis_factor)
    }
  } else {
    # Identificar automáticamente los factores basados en las cargas más altas de los ítems
    cols_factores <- names(df)[grepl("^f[0-9]+$", names(df))]

    for (col in cols_factores) {
      items_del_factor <- df$Items[df[[col]] > 0]
      if (length(items_del_factor) > 0) {
        sintaxis_factor <- paste0(col, " =~ ", paste(items_del_factor, collapse = " + "), "\n")
        sintaxis_modelo <- paste0(sintaxis_modelo, sintaxis_factor)
      }
    }
  }

  # Retornar la sintaxis completa del modelo
  return(trimws(sintaxis_modelo)) # Eliminar espacios en blanco al final
}
