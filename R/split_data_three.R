#' @title Split Data into Three Subsets
#' @description Splits a data frame into pilot, exploratory and confirmatory subsets.
#' @param df Data frame to split.
#' @param perc_piloto Proportion for pilot subset (default 0.10).
#' @param perc_exploratorio Proportion for exploratory subset (default 0.45).
#' @param perc_confirmatorio Proportion for confirmatory subset (default 0.45).
#' @param seed Random seed for reproducibility (default NULL).
#' @return A list with piloto, exploratorio and confirmatorio data frames.
#' @examples
#' \dontrun{
#' # Create sample data
#' set.seed(123)
#' data <- data.frame(
#'   Item1 = sample(1:5, 1000, replace = TRUE),
#'   Item2 = sample(1:5, 1000, replace = TRUE),
#'   Item3 = sample(1:5, 1000, replace = TRUE),
#'   Item4 = sample(1:5, 1000, replace = TRUE)
#' )
#'
#' # Split data into 10% pilot, 45% EFA, 45% CFA
#' splits <- split_data_three(
#'   df = data,
#'   perc_piloto = 0.10,
#'   perc_exploratorio = 0.45,
#'   perc_confirmatorio = 0.45,
#'   seed = 123
#' )
#'
#' # Access the subsets
#' pilot_data <- splits$piloto
#' efa_data <- splits$exploratorio
#' cfa_data <- splits$confirmatorio
#'
#' nrow(pilot_data)  # Should be 100
#' nrow(efa_data)    # Should be 450
#' nrow(cfa_data)    # Should be 450
#'
#' # Custom proportions: 20% pilot, 40% EFA, 40% CFA
#' splits2 <- split_data_three(
#'   df = data,
#'   perc_piloto = 0.20,
#'   perc_exploratorio = 0.40,
#'   perc_confirmatorio = 0.40,
#'   seed = 456
#' )
#' }
#' @export
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
