#' @title Group Items by Factor Loadings
#' @description Groups items by their factor loadings above a threshold.
#' @param df Data frame with items and factor loadings.
#' @param item_col Name of the column containing item identifiers.
#' @param threshold Threshold for factor loading (default 0).
#' @return A named list with items grouped by factor.
#' @examples
#' \dontrun{
#' # Create a data frame with factor loadings
#' loadings_df <- data.frame(
#'   Items = c("Item1", "Item2", "Item3", "Item4", "Item5", "Item6"),
#'   f1 = c(0.75, 0.68, 0.72, 0.10, 0.05, 0.08),
#'   f2 = c(0.08, 0.12, 0.05, 0.70, 0.65, 0.78)
#' )
#'
#' # Group items by factor (using threshold of 0.30)
#' groups <- agrupar_por_factor(
#'   df = loadings_df,
#'   item_col = "Items",
#'   threshold = 0.30
#' )
#' print(groups)
#' # $f1
#' # [1] "Item1" "Item2" "Item3"
#' # $f2
#' # [1] "Item4" "Item5" "Item6"
#'
#' # Use with EFA_modern results
#' efa_result <- EFA_modern(n_factors = 2, n_items = 6,
#'                          name_items = "Item", data = my_data,
#'                          apply_threshold = TRUE)
#' groups <- agrupar_por_factor(efa_result$result_df, threshold = 0.30)
#' }
#' @export
agrupar_por_factor <- function(df, item_col = "Items", threshold = 0) {
  # 1. Identificar columnas de factor (numéricas) descartando item_col y otras no-numéricas
  posibles_factores <- setdiff(names(df), item_col)

  # Quedarse solo con aquellas que sean numéricas:
  factor_cols <- posibles_factores[sapply(df[posibles_factores], is.numeric)]

  # 2. Para cada factor, filtrar ítems cuya carga > threshold
  grupos <- lapply(factor_cols, function(fac) {
    df[[item_col]][ df[[fac]] > threshold ]
  })

  # 3. Asignar nombres (los mismos nombres de columnas de factor)
  names(grupos) <- factor_cols

  return(grupos)
}
