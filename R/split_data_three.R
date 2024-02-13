split_data_three <- function(df, perc_piloto = 0.10, perc_exploratorio = 0.45, perc_confirmatorio = 0.45, seed = NULL) {
  # Asegurarse de que los porcentajes suman 1 (o 100%)
  if ((perc_piloto + perc_exploratorio + perc_confirmatorio) != 1) {
    stop("La suma de los porcentajes debe ser igual a 1")
  }

  # Establecer una semilla para reproducibilidad si se proporciona
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Calcular los tamaños de cada subconjunto
  n <- nrow(df)
  n_piloto <- floor(perc_piloto * n)
  n_exploratorio <- floor(perc_exploratorio * n)
  # El restante para confirmatorio, para evitar problemas de redondeo
  n_confirmatorio <- n - n_piloto - n_exploratorio

  # Crear índices aleatorios
  indices <- sample(seq_len(n))

  # Dividir los datos
  piloto <- df[indices[1:n_piloto], ]
  exploratorio <- df[indices[(n_piloto + 1):(n_piloto + n_exploratorio)], ]
  confirmatorio <- df[indices[(n_piloto + n_exploratorio + 1):n], ]

  # Retornar una lista con los tres dataframes
  return(list(piloto = piloto, exploratorio = exploratorio, confirmatorio = confirmatorio))
}
