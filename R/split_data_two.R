#' @title Split Data into Two Subsets
#' @description Splits a data frame into exploratory and confirmatory subsets.
#' @param df Data frame to split.
#' @param perc_exploratorio Proportion for exploratory subset (default 0.5).
#' @param perc_confirmatorio Proportion for confirmatory subset (default 0.5).
#' @param seed Random seed for reproducibility (default NULL).
#' @return A list with exploratorio and confirmatorio data frames.
#' @export
split_data_two <- function(df, perc_exploratorio = 0.5, perc_confirmatorio = 0.5, seed = NULL) {
  # Asegurarse de que los porcentajes suman 1 (o 100%)
  if ((perc_exploratorio + perc_confirmatorio) != 1) {
    stop("La suma de los porcentajes debe ser igual a 1")
  }

  # Establecer una semilla para reproducibilidad si se proporciona
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Calcular los tamaños de cada subconjunto
  n <- nrow(df)
  n_exploratorio <- floor(perc_exploratorio * n)
  # El restante para confirmatorio, para evitar problemas de redondeo
  n_confirmatorio <- n - n_exploratorio

  # Crear índices aleatorios
  indices <- sample(seq_len(n))

  # Dividir los datos
  exploratorio <- df[indices[1:n_exploratorio], ]
  confirmatorio <- df[indices[(n_exploratorio + 1):n], ]

  # Retornar una lista con los dos dataframes
  return(list(exploratorio = exploratorio, confirmatorio = confirmatorio))
}
