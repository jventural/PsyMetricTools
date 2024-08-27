EFA_modern <- function(n_factors,
                       n_items,
                       name_items,
                       data,
                       apply_threshold,
                       estimator = "WLSMV",
                       rotation = "oblimin",
                       exclude_items = NULL) {
  library(lavaan)
  library(dplyr)

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
