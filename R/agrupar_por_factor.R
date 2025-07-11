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
