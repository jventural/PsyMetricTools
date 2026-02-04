#' @title Modern Exploratory Factor Analysis
#' @description Performs exploratory factor analysis using lavaan with rotation.
#' @param n_factors Number of factors to extract.
#' @param n_items Number of items.
#' @param name_items Prefix for item names.
#' @param data Data frame containing the items.
#' @param apply_threshold Logical, whether to apply threshold to factor loadings.
#' @param estimator Estimator to use (default "WLSMV").
#' @param rotation Rotation method (default "oblimin").
#' @param exclude_items Items to exclude (default NULL).
#' @return A list containing fit indices, specifications, interfactor correlations, and pattern matrix.
#' @export
EFA_modern <- function(n_factors,
                       n_items,
                       name_items,
                       data,
                       apply_threshold,
                       estimator = "WLSMV",
                       rotation = "oblimin",
                       exclude_items = NULL) {
  # Generar modelo para lavaan exploratorio
  modelos <- generate_modelos(n_factors = n_factors, n_items = n_items, name_items = name_items, exclude_items = exclude_items)

  # Especificación para lavaan exploratorio con rotación
  Specifications <- specification_models(modelos, data = data, estimator = estimator, rotation = rotation)

  # Bondades de ajuste para lavaan exploratorio
  Bondades_Original <- extract_fit_measures(Specifications)

  # Matriz patron de lavaan exploratorio
  result_df <- Standardized_solutions(Specifications[[n_factors]], name_items = name_items, apply_threshold = apply_threshold)

  # Correlación entre factores
  InterFactor <- lavaan::inspect(Specifications[[n_factors]], what = "std")$psi

  # Retornar todos los resultados
  return(list(
    Bondades_Original = Bondades_Original,
    Specifications = Specifications,
    InterFactor = InterFactor,
    result_df = result_df
  ))
}
